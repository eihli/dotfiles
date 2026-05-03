---
name: skill-creator
description: |
  Create, edit, and iterate on skills. Use whenever the user wants to scaffold a new
  skill, improve an existing one, tune a skill's triggering description, or plan bundled
  resources (scripts, references, assets), target Claude/Codex/OpenCode compatibility,
  or add evals for a skill. Trigger on phrases like "make a skill", "turn this into a
  skill", "update this skill", "my skill isn't triggering", "make it cross-tool", or
  any request that produces a reusable workflow another AI agent should follow.
---

# Skill Creator

Guidance for creating skills that compatible AI agents can load and run. A skill is a
self-contained package: a common-denominator `SKILL.md` plus optional bundled resources
and tool-specific metadata. Design the portable core first, then add Claude/Codex/OpenCode
extras only when they solve a real invocation, permission, UI, or execution problem.

## When the user asks for a skill

Figure out where the user is in the lifecycle and jump in there:

- No draft yet: capture intent, plan resources, initialize, then write SKILL.md.
- Has a draft: focus on editing, tightening, and (if useful) testing.
- Updating an existing skill: preserve the original name and directory. Copy to a
  writable location before editing if the installed path is read-only.
- Wants to fix triggering: focus on the description (see "Description Optimization").

Stay flexible. If the user says "just vibe with me", skip the formal process.

## Core Principles

### Concise Is Key

The context window is shared by system prompt, conversation, other skills' metadata, and
the actual user request. Only add context the agent does not already have. Challenge each
sentence: "Does this paragraph justify its token cost?" Prefer concise examples over
verbose explanations.

### Explain the Why, Not Heavy-Handed MUSTs

Modern LLMs have good theory of mind. When you explain *why* something matters, the agent
can generalize to edge cases. Rigid ALWAYS/NEVER instructions are a yellow flag; reframe
and explain the reasoning instead. Reserve imperative MUSTs for genuinely fragile steps
where deviation causes real harm.

### Set Appropriate Degrees of Freedom

Match specificity to the task's fragility:

- **High freedom (prose instructions)**: multiple valid approaches, context-dependent
  decisions, heuristic choices.
- **Medium freedom (pseudocode, parameterized scripts)**: a preferred pattern exists,
  some variation is acceptable.
- **Low freedom (specific scripts, few parameters)**: fragile operations where
  consistency is critical.

Think of the agent exploring a path: narrow bridges need guardrails; open fields don't.

### Principle of Lack of Surprise

Skills must not contain malware, exploit code, credential theft, or content that could
compromise system security. Nothing in a skill should surprise the user relative to its
stated purpose. Decline requests to create misleading skills or skills designed to
facilitate unauthorized access or data exfiltration.

## Anatomy of a Skill

```
skill-name/
├── SKILL.md                    (required)
│   ├── YAML frontmatter        (name, description for portable skills)
│   └── Markdown body           (instructions)
└── Optional bundled resources:
    ├── scripts/                executable code (Python, bash, etc.)
    ├── references/             documentation loaded on demand
    ├── assets/                 files used in the agent's output
    ├── agents/                 optional grader/analyzer/comparator prompts
    ├── eval-viewer/            optional human review UI for eval artifacts
    ├── agents/openai.yaml      Codex UI/policy metadata (Codex-specific)
    └── evals/
        ├── evals.json          output-quality test prompts and expectations
        └── trigger-evals.json  description-triggering queries
```

### SKILL.md

- **Portable frontmatter**: use `name` and `description` only by default. This is the
  safest common denominator across Claude Code, Codex, and OpenCode.
- **Target-specific frontmatter**: add extra fields only when the user explicitly targets
  that tool. Claude Code supports fields such as `disable-model-invocation`,
  `allowed-tools`, `context`, `agent`, `arguments`, and hooks. OpenCode ignores unknown
  fields and only recognizes `name`, `description`, `license`, `compatibility`, and
  `metadata`. Codex keeps tool-specific policy/UI in `agents/openai.yaml`.
- **Body**: markdown. Loaded only *after* the skill triggers — putting "when to use this
  skill" in the body does nothing.

### scripts/

Executable code for tasks that are deterministic, repetitive, or otherwise benefit from
not rewriting the same code each invocation.

- Token efficient: the agent can execute scripts without reading them into context.
- Still readable: the agent can open them when patching or adjusting for environment.
- Test scripts before shipping — a broken script wastes every future invocation.

### references/

Markdown documentation the agent loads on demand.

- Keep SKILL.md lean; move detailed schemas, API docs, and long examples here.
- Reference each file from SKILL.md with a clear hint about when to read it.
- For files >100 lines, include a table of contents at the top.
- Avoid duplication: information lives in SKILL.md *or* in a reference, never both.

### assets/

Files the agent uses *in its output* (templates, boilerplate, images, fonts). Not loaded
into context, just copied or opened from scripts.

### What Not to Include

Skip README.md, INSTALLATION_GUIDE.md, CHANGELOG.md, and other auxiliary docs. A skill is
for an agent to do a job — not for humans to learn how it was built.

Exception: `evals/evals.json` is useful for skill development. Keep raw traces,
screenshots, generated apps, and large run artifacts outside the packaged skill, such as
under `evals/artifacts/` ignored by git or in a temp directory.

## Progressive Disclosure

Three tiers of loading:

1. **Metadata** (name + description): always in context (~100 words).
2. **SKILL.md body**: loaded when the skill triggers. Target <500 lines.
3. **Bundled resources**: loaded as needed; scripts can execute without loading.

### Pattern 1: High-level guide with references

```markdown
## Advanced features
- **Form filling**: see [references/forms.md]
- **API reference**: see [references/api.md]
- **Examples**: see [references/examples.md]
```

### Pattern 2: Domain organization

When a skill spans multiple domains or frameworks, split by variant so the agent reads
only the relevant file:

```
cloud-deploy/
├── SKILL.md          (workflow + provider selection)
└── references/
    ├── aws.md
    ├── gcp.md
    └── azure.md
```

### Pattern 3: Conditional details

Link to deep-dives only when specific features are invoked:

```markdown
For simple edits, modify the XML directly.

**For tracked changes**: see [references/redlining.md]
**For OOXML internals**: see [references/ooxml.md]
```

Keep references one level deep from SKILL.md — avoid nested chains.

## Skill Creation Process

### 1. Capture Intent

Extract answers from the conversation first; ask only what you cannot infer.

1. What should this skill enable the agent to do?
2. When should it trigger? (what user phrases or contexts)
3. What is the expected output format?
4. Are there verifiable outputs (file transforms, data extraction, deterministic code
   generation) that would benefit from test cases? Subjective outputs (style, art) often
   do not — let the user decide.

Wait to write the skill until you have concrete examples in hand. A vague skill becomes
a vague skill.

### 2. Plan Reusable Contents

For each concrete example, ask:

- What would the agent do from scratch?
- Which scripts, references, or assets would save work if bundled?

Example analysis for a `pdf-editor` skill:

- "Rotate this PDF" is rewritten often → bundle `scripts/rotate_pdf.py`.
- "Fill this form" needs schema discovery → bundle `references/form-fields.md`.
- "Brand this PDF" reuses templates → bundle `assets/cover-template.pdf`.

### 3. Initialize

If starting fresh, use `scripts/init_skill.py`:

```bash
scripts/init_skill.py <skill-name> --path <output-directory>
```

By default this creates a common-denominator `SKILL.md` with only `name` and
`description` frontmatter. Add optional pieces intentionally:

```bash
scripts/init_skill.py my-skill --path ~/.agents/skills
scripts/init_skill.py my-skill --path ~/.agents/skills --resources scripts,references
scripts/init_skill.py my-skill --path ~/.agents/skills --targets codex --evals
scripts/init_skill.py my-skill --path ~/.agents/skills --targets claude,codex,opencode
```

Use `--resources` only for resources the skill actually needs. Use `--targets` only when
the skill should scaffold tool-specific notes or metadata. Use `--evals` for any
non-trivial workflow or when you are improving an existing skill.

Skip this step if the skill already exists.

### 4. Edit

Write for another AI agent, not a human reader. Include procedural knowledge, domain
details, and gotchas that are non-obvious.

#### Frontmatter

```yaml
---
name: skill-name
description: |
  What the skill does AND when to use it. Include concrete trigger phrases
  and contexts. Many agents under-trigger skills — lean slightly assertive:
  "Use whenever the user mentions X, Y, or Z, even if they don't explicitly
  ask for a 'skill'."
---
```

Keep the portable default as `name` + `description`. If a target-specific behavior is
needed, prefer the least surprising place:

- **Claude Code**: add Claude-only frontmatter (`disable-model-invocation`,
  `user-invocable`, `allowed-tools`, `context: fork`, `agent`, `paths`, `arguments`,
  `hooks`) when the skill is meant to be invoked or sandboxed that way.
- **Codex**: add `agents/openai.yaml` for UI metadata, `policy.allow_implicit_invocation`,
  and tool dependency declarations. Do not put Codex policy in `SKILL.md`.
- **OpenCode**: put access policy in `opencode.json` (`permission.skill`) or agent
  frontmatter. OpenCode only uses a small SKILL.md frontmatter subset and ignores the
  rest, so do not rely on Claude-only fields for OpenCode behavior.

Keep descriptions specific and context-rich. Bad: `"Format this data"`. Good: `"Clean
and normalize messy spreadsheet data — column headers with typos, inconsistent date
formats, mixed types. Use when a user pastes tabular data or references an .xlsx/.csv
file that needs cleanup before analysis."`

#### Body

- Use imperative or infinitive form ("Extract the fields", "Run the script").
- Reference bundled resources explicitly with a hint about when to read them.
- Test every bundled script by running it.
- Delete placeholder files from init.

### 5. Validate

Run structural validation after editing:

```bash
scripts/quick_validate.py <path/to/skill-folder>
scripts/quick_validate.py <path/to/skill-folder> --target common
scripts/quick_validate.py <path/to/skill-folder> --target opencode
```

Use `--target common` to prove the skill is portable (`name` + `description` only).
Use a tool target when you deliberately added target-specific frontmatter.

### 6. Add Evals

For every non-trivial skill, create evals before calling it done. Keep two eval
families separate:

- `evals/evals.json`: task/output evals. These prompts should be run with the skill
  and compared to a baseline without the skill or with the previous skill version.
- `evals/trigger-evals.json`: description-triggering evals. These queries test whether
  the description causes the skill to trigger when it should and stay quiet on near
  misses.

Ask the user to review the proposed eval prompts before treating them as accepted:
"Here are a few test cases I'd like to try. Do these look right, or do you want to add
more?" Bad evals produce misleading confidence.

Use Anthropic-style `evals/evals.json` for output evals:

```json
{
  "skill_name": "skill-name",
  "evals": [
    {
      "id": 1,
      "prompt": "User's realistic task prompt",
      "expected_output": "Human-readable description of successful output",
      "files": [],
      "expectations": [
        "The output includes X",
        "The run uses bundled script Y"
      ]
    },
    {
      "id": 2,
      "prompt": "Another realistic task prompt",
      "expected_output": "What success looks like",
      "files": ["evals/files/example-input.txt"],
      "expectations": []
    }
  ]
}
```

Good output-eval coverage:

- 2-3 realistic prompts that a user would actually type.
- For file-transform skills, include small representative input files under
  `evals/files/`.
- `expected_output` should describe the artifact, response, or behavior that counts as
  success.
- Add `expectations` after or during test execution. Good expectations are objectively
  verifiable and discriminating — they should fail when the skill merely appears to work
  but misses the important outcome.

Anthropic-style run loop:

1. Spawn runs for every eval in the same turn, with-skill and baseline.
2. Save each eval's prompt and expectations to `eval_metadata.json` in the run
   directory.
3. Capture timing/token data from completion notifications when available.
4. Grade each run against expectations; use scripts for programmatic checks where
   possible.
5. Aggregate pass rate, timing, and token usage.
6. Put outputs and benchmark data in front of the user for review before revising the
   skill.

If subagents, browser access, or the exact Anthropic eval viewer are unavailable, keep
the same shape: save run artifacts, present outputs and grades to the user, collect
feedback, then iterate.

This skill includes a portable local runner for that shape:

```bash
scripts/run_skill_evals.py <path/to/skill> --executor mock --grader mock
scripts/run_skill_evals.py <path/to/skill> \
  --executor command \
  --command-template 'agent-cli --prompt-file {prompt_file}' \
  --grader deterministic
```

The runner creates:

```text
<skill-name>-workspace/
└── iteration-1/
    ├── eval-1-descriptive-name/
    │   ├── eval_metadata.json
    │   ├── inputs/
    │   ├── with_skill/run-1/{outputs/,transcript.md,timing.json,grading.json}
    │   └── without_skill/run-1/{outputs/,transcript.md,timing.json,grading.json}
    ├── benchmark.json
    ├── benchmark.md
    └── review.html
```

Use `--executor mock --grader mock` only to smoke-test the artifact pipeline. For real
runs, use `--executor command` with a tool-specific noninteractive command. The command
template receives `{cwd}`, `{prompt_file}`, `{outputs_dir}`, `{run_dir}`, `{config}`,
and `{skill_path}` placeholders, and the runner installs the skill into project-local
`.agents/skills`, `.claude/skills`, `.codex/skills`, and `.opencode/skills` directories
for the `with_skill` config. Baseline runs omit that installed skill. If a user's global
tool config still loads the same skill, use a wrapper command that isolates that tool's
config while preserving authentication.

Default deterministic grading supports machine-checkable expectation prefixes:

- `output_contains: text`
- `transcript_contains: text`
- `file_exists: relative/path`
- `file_absent: relative/path`

Free-form expectations fail closed under deterministic grading; use
`--grader-command-template` or review `review.html` for natural-language judgments.

Use `evals/trigger-evals.json` for description optimization:

```json
[
  {
    "query": "Substantive realistic request that should trigger the skill",
    "should_trigger": true
  },
  {
    "query": "Near-miss request sharing terms but needing another workflow",
    "should_trigger": false
  }
]
```

Good trigger-eval coverage:

- 8-10 positive natural-language trigger cases with varied phrasing.
- 8-10 near-miss negatives that share keywords but should use a different workflow.
- Concrete, substantive queries with realistic details, paths, URLs, typos, or context.
  Simple one-step queries often do not trigger skills because the agent can handle them
  directly.

For important skills, compare a baseline run without the skill to a run with the skill.
Score small, concrete expectations first: expected commands run, required files created,
forbidden files absent, output headings present, no secret exposure, no unnecessary
network/destructive steps. Use an LLM rubric only for subjective quality.

Capture traces/artifacts outside the shipped skill (for example
`evals/artifacts/<date>-<case>/`) and keep the prompt set under version control. When a
real task exposes a miss, add the prompt as a new eval before changing the skill.

### 7. Package (optional)

For distribution as a single file:

```bash
scripts/package_skill.py <path/to/skill-folder>
```

Produces a `.skill` file (a zip with a `.skill` extension). The script validates
frontmatter, naming, and structure before packaging. Not every tool consumes `.skill`
files — many just read directories directly — so this step is optional.

### 8. Iterate

After real use, notice struggles or inefficiencies. The iteration loop is where skills
get good:

- **Generalize from feedback**: a skill should work across many prompts, not just the
  three test cases under your nose. Fix the pattern, not the instance. Fiddly,
  overfit edits are a warning sign.
- **Keep the prompt lean**: remove instructions that aren't pulling their weight. If the
  agent is wasting time on unproductive steps, cut the instructions that cause them.
- **Explain the why**: if you find yourself writing ALWAYS or NEVER in caps, reframe and
  explain the reasoning. The agent handles edge cases better when it understands
  motivation.
- **Bundle repeated work**: if multiple runs independently wrote similar helper scripts
  or took the same multi-step approach, that's a signal to bundle a script. Write it
  once; save every future invocation from reinventing it.

## Writing Patterns

### Strict output format

```markdown
## Report structure
ALWAYS use this exact template:

# [Title]
## Executive summary
## Key findings
## Recommendations
```

### Flexible output format

```markdown
## Report structure
A sensible default — adapt as needed:

# [Title]
## Summary
## Findings
## Recommendations
```

### Input/output examples

```markdown
## Commit message format

**Example 1:**
Input: Added user authentication with JWT tokens
Output: feat(auth): implement JWT-based authentication

**Example 2:**
Input: Fixed a bug where dates displayed wrong in reports
Output: fix(reports): correct timezone handling in date formatting
```

Examples communicate style faster than prose descriptions.

### Sequential workflow

```markdown
Filling a PDF form:

1. Analyze the form (run analyze_form.py)
2. Create field mapping (edit fields.json)
3. Validate mapping (run validate_fields.py)
4. Fill the form (run fill_form.py)
5. Verify output (run verify_output.py)
```

### Conditional workflow

```markdown
1. Determine the modification type:
   **Creating new content?** → follow "Creation workflow"
   **Editing existing content?** → follow "Editing workflow"
```

See `references/workflows.md` and `references/output-patterns.md` for more.

## Description Optimization

The description field decides whether the skill triggers at all. After the skill works
well, tune the description:

1. Write 16–20 realistic user queries: roughly half should trigger the skill, half
   should not.
2. Make the should-trigger queries cover different phrasings — formal, casual,
   implicit, and cases where the user doesn't name the skill or file type.
3. Make the should-not-trigger queries near-misses — queries sharing keywords but
   needing something else. "Write a fibonacci function" is a bad negative test for a
   PDF skill; it's too easy to reject.
4. Record these queries in `evals/trigger-evals.json` and score them against trigger
   behavior.
5. Revise the description based on failures. Iterate.

Note: simple one-step queries ("read this file") may not trigger any skill because the
agent handles them directly. Test queries should be substantive enough that the agent
would genuinely benefit from the skill's guidance.

## Tool-Specific Compatibility

This skill ships as a portable directory symlinked into each tool's skill root:

- **Common locations**: `~/.agents/skills/<name>/SKILL.md` and repo-local
  `.agents/skills/<name>/SKILL.md`.
- **Claude Code**: also scans `~/.claude/skills/<name>/` and `.claude/skills/<name>/`.
  Claude custom commands and skills now share behavior; prefer skills for new work.
- **Codex**: scans `.agents/skills` from cwd up to repo root, `~/.agents/skills`, admin
  locations, and system skills. Codex supports symlinked skill folders.
- **OpenCode**: scans `.opencode/skills`, `.claude/skills`, and `.agents/skills` in
  project and global locations. Access is controlled by `permission.skill`.

Differences to keep in mind when writing skills:

- Subagent availability varies. Don't hard-code "spawn a subagent" as a required step;
  phrase it as "delegate to a subagent if available, otherwise run inline".
- Display/browser availability varies. Don't assume the agent can `open` HTML in a
  browser; offer a file-based fallback ("write `report.html`, tell the user the path").
- Skill invocation syntax varies. Codex commonly supports `$skill-name`; Claude Code
  supports `/skill-name`; OpenCode agents load skills through the `skill` tool.
- CLI/eval tools vary. Anything like `claude -p`, `codex exec --json`, or an OpenCode
  command wrapper is tool-specific. Gate these behind the matching target.

When in doubt, write the skill in terms of *capabilities* ("if you can run scripts in
parallel, do so"), not *specific tools*.

## Reference Files

- `references/workflows.md` — sequential and conditional workflow patterns
- `references/output-patterns.md` — template, example, and formatting patterns
- `agents/grader.md` — guidance for LLM grading of eval outputs
- `agents/analyzer.md` — guidance for interpreting benchmark results
- `scripts/run_skill_evals.py` — artifact-producing eval runner
- `scripts/aggregate_benchmark.py` — benchmark aggregation from `grading.json`
- `eval-viewer/generate_review.py` — static HTML review page generator
