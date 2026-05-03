#!/usr/bin/env python3
"""Validate an Agent Skill folder.

Default validation accepts the union of known Claude Code, Codex, and OpenCode
frontmatter so existing target-specific skills do not fail. Use
`--target common` to enforce the portable name+description subset.
"""

from __future__ import annotations

import argparse
import json
import re
import sys
from pathlib import Path
from typing import Any

import yaml


COMMON_FIELDS = {"name", "description"}
CLAUDE_FIELDS = COMMON_FIELDS | {
    "when_to_use",
    "argument-hint",
    "arguments",
    "disable-model-invocation",
    "user-invocable",
    "allowed-tools",
    "model",
    "effort",
    "context",
    "agent",
    "hooks",
    "paths",
    "shell",
}
CODEX_FIELDS = COMMON_FIELDS | {
    "license",
    "allowed-tools",
    "metadata",
}
OPENCODE_FIELDS = COMMON_FIELDS | {
    "license",
    "compatibility",
    "metadata",
}
TARGET_FIELDS = {
    "common": COMMON_FIELDS,
    "claude": CLAUDE_FIELDS,
    "codex": CODEX_FIELDS,
    "opencode": OPENCODE_FIELDS,
    "all": CLAUDE_FIELDS | CODEX_FIELDS | OPENCODE_FIELDS,
}

NAME_RE = re.compile(r"^[a-z0-9]+(-[a-z0-9]+)*$")
FRONTMATTER_RE = re.compile(r"^---\n(.*?)\n---", re.DOTALL)


def _load_frontmatter(skill_md: Path) -> tuple[dict[str, Any] | None, str]:
    content = skill_md.read_text()
    if not content.startswith("---"):
        return None, "No YAML frontmatter found"

    match = FRONTMATTER_RE.match(content)
    if not match:
        return None, "Invalid frontmatter format"

    try:
        frontmatter = yaml.safe_load(match.group(1))
    except yaml.YAMLError as exc:
        return None, f"Invalid YAML in frontmatter: {exc}"

    if not isinstance(frontmatter, dict):
        return None, "Frontmatter must be a YAML dictionary"
    return frontmatter, ""


def _validate_name(name: Any, skill_path: Path) -> str | None:
    if not isinstance(name, str):
        return f"Name must be a string, got {type(name).__name__}"

    name = name.strip()
    if not name:
        return "Name cannot be empty"
    if not NAME_RE.fullmatch(name):
        return (
            f"Name '{name}' must be lowercase letters/digits with single hyphen "
            "separators"
        )
    if len(name) > 64:
        return f"Name is too long ({len(name)} characters). Maximum is 64."
    if skill_path.name != name:
        return f"Name '{name}' must match directory name '{skill_path.name}'"
    return None


def _validate_description(description: Any) -> str | None:
    if not isinstance(description, str):
        return f"Description must be a string, got {type(description).__name__}"

    description = description.strip()
    if not description:
        return "Description cannot be empty"
    if "<" in description or ">" in description:
        return "Description cannot contain angle brackets (< or >)"
    if len(description) > 1024:
        return (
            f"Description is too long ({len(description)} characters). "
            "Maximum is 1024."
        )
    return None


def _validate_metadata(metadata: Any) -> str | None:
    if metadata is None:
        return None
    if not isinstance(metadata, dict):
        return "metadata must be a YAML mapping"
    bad_keys = [key for key in metadata if not isinstance(key, str)]
    bad_values = [value for value in metadata.values() if not isinstance(value, str)]
    if bad_keys or bad_values:
        return "metadata keys and values must be strings"
    return None


def _validate_evals(skill_path: Path, skill_name: str) -> str | None:
    evals_path = skill_path / "evals" / "evals.json"
    if not evals_path.exists():
        return None

    try:
        payload = json.loads(evals_path.read_text())
    except json.JSONDecodeError as exc:
        return f"Invalid evals/evals.json: {exc}"

    if not isinstance(payload, dict):
        return "evals/evals.json must be a JSON object"
    if {"version", "skill", "cases"} <= set(payload):
        return (
            "evals/evals.json uses the old local schema. Use Anthropic-style "
            "fields: skill_name and evals; put trigger checks in "
            "evals/trigger-evals.json."
        )
    if payload.get("skill_name") != skill_name:
        return "evals/evals.json skill_name must match SKILL.md name"

    evals = payload.get("evals")
    if not isinstance(evals, list) or not evals:
        return "evals/evals.json evals must be a non-empty list"

    seen_ids: set[int] = set()
    for index, case in enumerate(evals):
        prefix = f"eval case {index}"
        if not isinstance(case, dict):
            return f"{prefix} must be an object"
        case_id = case.get("id")
        if type(case_id) is not int:
            return f"{prefix} id must be an integer"
        if case_id in seen_ids:
            return f"Duplicate eval case id: {case_id}"
        seen_ids.add(case_id)

        prompt = case.get("prompt")
        if not isinstance(prompt, str) or not prompt.strip():
            return f"eval case {case_id} prompt must be a non-empty string"

        expected_output = case.get("expected_output")
        if not isinstance(expected_output, str) or not expected_output.strip():
            return f"eval case {case_id} expected_output must be a non-empty string"

        files = case.get("files", [])
        if not isinstance(files, list):
            return f"eval case {case_id} files must be a list"
        if any(not isinstance(file, str) or not file.strip() for file in files):
            return f"eval case {case_id} files must be non-empty strings"

        expectations = case.get("expectations", [])
        if not isinstance(expectations, list):
            return f"eval case {case_id} expectations must be a list"
        if any(
            not isinstance(expectation, str) or not expectation.strip()
            for expectation in expectations
        ):
            return f"eval case {case_id} expectations must be non-empty strings"

    return None


def _validate_trigger_evals(skill_path: Path) -> str | None:
    evals_path = skill_path / "evals" / "trigger-evals.json"
    if not evals_path.exists():
        return None

    try:
        payload = json.loads(evals_path.read_text())
    except json.JSONDecodeError as exc:
        return f"Invalid evals/trigger-evals.json: {exc}"

    if not isinstance(payload, list) or not payload:
        return "evals/trigger-evals.json must be a non-empty JSON array"

    for index, case in enumerate(payload):
        if not isinstance(case, dict):
            return f"trigger eval case {index} must be an object"
        query = case.get("query")
        if not isinstance(query, str) or not query.strip():
            return f"trigger eval case {index} query must be a non-empty string"
        if not isinstance(case.get("should_trigger"), bool):
            return f"trigger eval case {index} should_trigger must be boolean"

    return None


def validate_skill(skill_path: str | Path, target: str = "all") -> tuple[bool, str]:
    """Validate a skill folder.

    Args:
        skill_path: Directory containing SKILL.md.
        target: `common`, `claude`, `codex`, `opencode`, or `all`.
    """
    skill_path = Path(skill_path)
    if target not in TARGET_FIELDS:
        return False, f"Unknown target '{target}'"

    skill_md = skill_path / "SKILL.md"
    if not skill_md.exists():
        return False, "SKILL.md not found"

    frontmatter, error = _load_frontmatter(skill_md)
    if error:
        return False, error
    assert frontmatter is not None

    allowed = TARGET_FIELDS[target]
    unexpected = set(frontmatter) - allowed
    if unexpected:
        return False, (
            f"Unexpected key(s) for target '{target}': "
            f"{', '.join(sorted(unexpected))}. Allowed properties are: "
            f"{', '.join(sorted(allowed))}"
        )

    missing = COMMON_FIELDS - set(frontmatter)
    if missing:
        return False, f"Missing required field(s): {', '.join(sorted(missing))}"

    name_error = _validate_name(frontmatter["name"], skill_path)
    if name_error:
        return False, name_error

    description_error = _validate_description(frontmatter["description"])
    if description_error:
        return False, description_error

    metadata_error = _validate_metadata(frontmatter.get("metadata"))
    if metadata_error:
        return False, metadata_error

    evals_error = _validate_evals(skill_path, frontmatter["name"].strip())
    if evals_error:
        return False, evals_error
    trigger_evals_error = _validate_trigger_evals(skill_path)
    if trigger_evals_error:
        return False, trigger_evals_error

    return True, "Skill is valid!"


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Validate an Agent Skill folder")
    parser.add_argument("skill_directory")
    parser.add_argument(
        "--target",
        choices=sorted(TARGET_FIELDS),
        default="all",
        help="frontmatter compatibility target (default: all)",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    valid, message = validate_skill(args.skill_directory, target=args.target)
    print(message)
    raise SystemExit(0 if valid else 1)


if __name__ == "__main__":
    main()
