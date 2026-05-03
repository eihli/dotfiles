#!/usr/bin/env python3
"""Create a skill folder from a portable template.

The default output is the common denominator accepted by Claude Code, Codex,
and OpenCode: a folder containing SKILL.md with only name and description
frontmatter. Add resources, target-specific notes, or eval scaffolding only
when the skill needs them.
"""

from __future__ import annotations

import argparse
import re
from pathlib import Path


VALID_TARGETS = {"common", "claude", "codex", "opencode", "all"}
VALID_RESOURCES = {"scripts", "references", "assets"}

SKILL_TEMPLATE = """---
name: {skill_name}
description: |
  TODO: What this skill does AND when to trigger it. Include concrete user
  phrases, file types, or contexts. Keep this portable: only name and
  description are present by default so Claude Code, Codex, and OpenCode can
  all load the skill.
---

# {skill_title}

## Goal

[TODO: 1-2 sentences explaining what this skill enables.]

## Workflow

[TODO: Write imperative steps for another AI agent. Keep the portable core here.
Explain why fragile steps matter so the agent can generalize to edge cases.]
{target_sections}
{resource_section}
"""

CLAUDE_SECTION = """

## Claude Code Notes

[TODO: Delete this section unless the skill intentionally targets Claude Code.]
Claude-only behavior belongs in SKILL.md frontmatter. Examples include
`disable-model-invocation`, `user-invocable`, `allowed-tools`, `context: fork`,
`agent`, `paths`, `arguments`, and `hooks`. Add only the fields this skill
actually needs; otherwise keep the portable `name` + `description` frontmatter.
"""

CODEX_SECTION = """

## Codex Notes

[TODO: Delete this section unless the skill intentionally targets Codex.]
Use `agents/openai.yaml` for Codex UI metadata, implicit-invocation policy, and
tool dependency declarations. Keep Codex policy out of SKILL.md so the portable
core remains readable by other tools.
"""

OPENCODE_SECTION = """

## OpenCode Notes

[TODO: Delete this section unless the skill intentionally targets OpenCode.]
OpenCode recognizes only `name`, `description`, `license`, `compatibility`, and
`metadata` in SKILL.md frontmatter. Configure access in `opencode.json` with
`permission.skill`; do not rely on Claude-only frontmatter for OpenCode behavior.
"""

RESOURCE_SECTION = """

## Resources

[TODO: Reference only resources that exist and matter. Delete unused subsections.]
{resource_items}
"""

RESOURCE_ITEM = {
    "scripts": """
### scripts/

Executable helpers for deterministic or repetitive work. Test every script before
shipping the skill.
""",
    "references": """
### references/

Reference material to load on demand. Link each file from SKILL.md with a short
hint about when to read it.
""",
    "assets": """
### assets/

Files used in the final output, such as templates, images, fonts, or boilerplate.
Assets are not general documentation.
""",
}

EXAMPLE_SCRIPT = '''#!/usr/bin/env python3
"""Example helper script for {skill_name}. Replace or delete."""


def main() -> None:
    print("Example script for {skill_name}")


if __name__ == "__main__":
    main()
'''

EXAMPLE_REFERENCE = """# Reference Documentation for {skill_title}

Placeholder for detailed reference material. Replace or delete.

Use reference docs for API details, schemas, long examples, and variant-specific
patterns that should not live in SKILL.md.
"""

EXAMPLE_ASSET = """Placeholder asset.

Assets are files used in the agent's output, not loaded into context. Replace or
delete this file.
"""

OPENAI_YAML_TEMPLATE = """interface:
  display_name: "{skill_title}"
  short_description: "TODO: One short user-facing description"
  default_prompt: "Use ${skill_name} to TODO: describe the default task."
policy:
  allow_implicit_invocation: true
"""

EVALS_TEMPLATE = """{{
  "skill_name": "{skill_name}",
  "evals": [
    {{
      "id": 1,
      "prompt": "Use ${skill_name} to perform a minimal representative task.",
      "expected_output": "Describe the concrete artifact, answer, or behavior that a successful run should produce.",
      "files": [],
      "expectations": [
        "output_contains: TODO replace with text that should appear in a successful output.",
        "transcript_contains: TODO replace with a workflow step the executor should perform."
      ]
    }},
    {{
      "id": 2,
      "prompt": "TODO: Natural-language request that should benefit from this skill.",
      "expected_output": "Describe what success looks like for this realistic task.",
      "files": [],
      "expectations": [
        "TODO: Add an objectively verifiable expectation."
      ]
    }}
  ]
}}
"""

TRIGGER_EVALS_TEMPLATE = """[
  {{
    "query": "TODO: Realistic request that should trigger {skill_name}, with enough detail to need the skill.",
    "should_trigger": true
  }},
  {{
    "query": "TODO: Near-miss request that shares keywords with {skill_name} but should use a different workflow.",
    "should_trigger": false
  }}
]
"""


def title_case(skill_name: str) -> str:
    """Convert a hyphen-case skill name to Title Case for display.

    >>> title_case("data-cleaner")
    'Data Cleaner'
    >>> title_case("pdf")
    'Pdf'
    >>> title_case("a-b-c")
    'A B C'
    """
    return " ".join(word.capitalize() for word in skill_name.split("-"))


def normalize_targets(raw_targets: str) -> set[str]:
    targets = {part.strip() for part in raw_targets.split(",") if part.strip()}
    unknown = targets - VALID_TARGETS
    if unknown:
        raise ValueError(f"Unknown target(s): {', '.join(sorted(unknown))}")
    if not targets:
        return {"common"}
    if "all" in targets:
        return {"claude", "codex", "opencode"}
    if "common" in targets and len(targets) > 1:
        raise ValueError("Use common by itself, or choose claude,codex,opencode")
    return targets


def normalize_resources(raw_resources: str) -> set[str]:
    if not raw_resources:
        return set()
    resources = {part.strip() for part in raw_resources.split(",") if part.strip()}
    unknown = resources - VALID_RESOURCES
    if unknown:
        raise ValueError(f"Unknown resource(s): {', '.join(sorted(unknown))}")
    return resources


def validate_skill_name(skill_name: str) -> str | None:
    if not re.fullmatch(r"[a-z0-9]+(-[a-z0-9]+)*", skill_name):
        return "Skill name must be lowercase letters/digits with single hyphen separators"
    if len(skill_name) > 64:
        return "Skill name must be 64 characters or fewer"
    return None


def build_target_sections(targets: set[str]) -> str:
    sections = []
    if "claude" in targets:
        sections.append(CLAUDE_SECTION)
    if "codex" in targets:
        sections.append(CODEX_SECTION)
    if "opencode" in targets:
        sections.append(OPENCODE_SECTION)
    return "".join(sections)


def build_resource_section(resources: set[str]) -> str:
    if not resources:
        return ""
    items = "".join(
        RESOURCE_ITEM[name]
        for name in ("scripts", "references", "assets")
        if name in resources
    )
    return RESOURCE_SECTION.format(resource_items=items.rstrip())


def write_resources(
    skill_dir: Path,
    skill_name: str,
    skill_title: str,
    resources: set[str],
) -> list[str]:
    written: list[str] = []
    if "scripts" in resources:
        scripts_dir = skill_dir / "scripts"
        scripts_dir.mkdir()
        example_script = scripts_dir / "example.py"
        example_script.write_text(EXAMPLE_SCRIPT.format(skill_name=skill_name))
        example_script.chmod(0o755)
        written.append("scripts/example.py")

    if "references" in resources:
        references_dir = skill_dir / "references"
        references_dir.mkdir()
        (references_dir / "reference.md").write_text(
            EXAMPLE_REFERENCE.format(skill_title=skill_title)
        )
        written.append("references/reference.md")

    if "assets" in resources:
        assets_dir = skill_dir / "assets"
        assets_dir.mkdir()
        (assets_dir / "example.txt").write_text(EXAMPLE_ASSET)
        written.append("assets/example.txt")

    return written


def write_codex_metadata(skill_dir: Path, skill_name: str, skill_title: str) -> str:
    agents_dir = skill_dir / "agents"
    agents_dir.mkdir(exist_ok=True)
    (agents_dir / "openai.yaml").write_text(
        OPENAI_YAML_TEMPLATE.format(skill_name=skill_name, skill_title=skill_title)
    )
    return "agents/openai.yaml"


def write_evals(skill_dir: Path, skill_name: str) -> list[str]:
    evals_dir = skill_dir / "evals"
    evals_dir.mkdir()
    (evals_dir / "evals.json").write_text(EVALS_TEMPLATE.format(skill_name=skill_name))
    (evals_dir / "trigger-evals.json").write_text(
        TRIGGER_EVALS_TEMPLATE.format(skill_name=skill_name)
    )
    return ["evals/evals.json", "evals/trigger-evals.json"]


def init_skill(
    skill_name: str,
    path: str,
    *,
    resources: set[str],
    targets: set[str],
    include_evals: bool,
) -> Path | None:
    error = validate_skill_name(skill_name)
    if error:
        print(f"Error: {error}")
        return None

    skill_dir = Path(path).expanduser().resolve() / skill_name
    if skill_dir.exists():
        print(f"Error: skill directory already exists: {skill_dir}")
        return None

    skill_dir.mkdir(parents=True, exist_ok=False)
    print(f"Created {skill_dir}")

    skill_title = title_case(skill_name)
    skill_md = SKILL_TEMPLATE.format(
        skill_name=skill_name,
        skill_title=skill_title,
        target_sections=build_target_sections(targets),
        resource_section=build_resource_section(resources),
    )
    (skill_dir / "SKILL.md").write_text(skill_md)
    print("  SKILL.md")

    for item in write_resources(skill_dir, skill_name, skill_title, resources):
        print(f"  {item}")

    if "codex" in targets:
        print(f"  {write_codex_metadata(skill_dir, skill_name, skill_title)}")

    if include_evals:
        for item in write_evals(skill_dir, skill_name):
            print(f"  {item}")

    print()
    print(f"Skill '{skill_name}' initialized at {skill_dir}")
    print("Next:")
    print("  1. Fill in SKILL.md, especially the description.")
    print("  2. Delete unused target notes and placeholder resource files.")
    print("  3. Review evals/evals.json and evals/trigger-evals.json with the user.")
    print("  4. Run scripts/quick_validate.py to check the skill structure.")
    return skill_dir


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Initialize a portable Agent Skill")
    parser.add_argument("skill_name", help="hyphen-case skill name")
    parser.add_argument("--path", required=True, help="output directory")
    parser.add_argument(
        "--resources",
        default="",
        help="comma-separated optional resources: scripts,references,assets",
    )
    parser.add_argument(
        "--targets",
        default="common",
        help="common (default), claude, codex, opencode, all, or comma-separated list",
    )
    parser.add_argument("--evals", action="store_true", help="create evals/evals.json")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    try:
        targets = normalize_targets(args.targets)
        resources = normalize_resources(args.resources)
    except ValueError as exc:
        print(f"Error: {exc}")
        raise SystemExit(1)

    print(f"Initializing skill: {args.skill_name}")
    print(f"  at: {args.path}")
    print(f"  targets: {','.join(sorted(targets)) if targets else 'common'}")
    print(f"  resources: {','.join(sorted(resources)) if resources else 'none'}")
    print(f"  evals: {'yes' if args.evals else 'no'}")
    print()

    result = init_skill(
        args.skill_name,
        args.path,
        resources=resources,
        targets=targets,
        include_evals=args.evals,
    )
    raise SystemExit(0 if result else 1)


if __name__ == "__main__":
    main()
