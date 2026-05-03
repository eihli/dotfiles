#!/usr/bin/env python3
"""Run skill output evals through skill/baseline executions and review artifacts."""

from __future__ import annotations

import argparse
import importlib.util
import json
import os
import re
import shutil
import shlex
import subprocess
import sys
import time
from dataclasses import dataclass
from pathlib import Path
from typing import Any

try:
    from .aggregate_benchmark import write_benchmark
    from .grade_run import grade_run
    from .quick_validate import validate_skill
except ImportError:
    from aggregate_benchmark import write_benchmark
    from grade_run import grade_run
    from quick_validate import validate_skill


SKILL_INSTALL_DIRS = (
    ".agents/skills",
    ".claude/skills",
    ".codex/skills",
    ".opencode/skills",
)


@dataclass(frozen=True)
class EvalCase:
    eval_id: int
    prompt: str
    expected_output: str
    files: list[str]
    expectations: list[str]


def slugify(text: str, max_len: int = 48) -> str:
    slug = re.sub(r"[^a-zA-Z0-9]+", "-", text.strip().lower()).strip("-")
    return (slug or "eval")[:max_len].strip("-") or "eval"


def load_evals(skill_path: Path) -> tuple[str, list[EvalCase]]:
    evals_path = skill_path / "evals" / "evals.json"
    if not evals_path.exists():
        raise FileNotFoundError(f"Missing {evals_path}")
    payload = json.loads(evals_path.read_text())
    if not isinstance(payload, dict):
        raise ValueError("evals/evals.json must be a JSON object")
    skill_name = payload.get("skill_name")
    if not isinstance(skill_name, str) or not skill_name:
        raise ValueError("evals/evals.json skill_name must be a non-empty string")

    cases: list[EvalCase] = []
    for raw_case in payload.get("evals", []):
        if not isinstance(raw_case, dict):
            continue
        cases.append(
            EvalCase(
                eval_id=int(raw_case["id"]),
                prompt=str(raw_case["prompt"]),
                expected_output=str(raw_case["expected_output"]),
                files=[str(item) for item in raw_case.get("files", [])],
                expectations=[
                    str(item) for item in raw_case.get("expectations", []) if item
                ],
            )
        )
    if not cases:
        raise ValueError("evals/evals.json evals must contain at least one case")
    return skill_name, cases


def next_iteration_dir(workspace: Path, requested: str | None) -> Path:
    if requested:
        return workspace / requested
    existing = []
    if workspace.exists():
        for child in workspace.glob("iteration-*"):
            if not child.is_dir():
                continue
            try:
                existing.append(int(child.name.split("-", 1)[1]))
            except (IndexError, ValueError):
                continue
    return workspace / f"iteration-{(max(existing) if existing else 0) + 1}"


def copy_skill(skill_path: Path, target_root: Path, skill_name: str) -> list[Path]:
    copied: list[Path] = []
    for install_dir in SKILL_INSTALL_DIRS:
        destination = target_root / install_dir / skill_name
        destination.parent.mkdir(parents=True, exist_ok=True)
        shutil.copytree(
            skill_path,
            destination,
            ignore=shutil.ignore_patterns("__pycache__", "*.pyc", "evals/artifacts"),
        )
        copied.append(destination)
    return copied


def copy_eval_inputs(skill_path: Path, case: EvalCase, eval_dir: Path) -> list[Path]:
    inputs_dir = eval_dir / "inputs"
    inputs_dir.mkdir(parents=True, exist_ok=True)
    copied: list[Path] = []
    skill_root = skill_path.resolve()

    for rel in case.files:
        source = (skill_path / rel).resolve()
        try:
            source.relative_to(skill_root)
        except ValueError as exc:
            raise ValueError(f"Eval file escapes skill directory: {rel}") from exc
        if not source.exists():
            raise FileNotFoundError(f"Eval file not found: {source}")

        destination = inputs_dir / rel
        destination.parent.mkdir(parents=True, exist_ok=True)
        if source.is_dir():
            shutil.copytree(source, destination, dirs_exist_ok=True)
        else:
            shutil.copy2(source, destination)
        copied.append(destination)
    return copied


def write_eval_metadata(eval_dir: Path, case: EvalCase, eval_name: str) -> Path:
    path = eval_dir / "eval_metadata.json"
    payload = {
        "eval_id": case.eval_id,
        "eval_name": eval_name,
        "prompt": case.prompt,
        "expected_output": case.expected_output,
        "files": case.files,
        "expectations": case.expectations,
    }
    path.write_text(json.dumps(payload, indent=2) + "\n")
    return path


def build_prompt(
    case: EvalCase,
    config: str,
    skill_name: str,
    installed_skill_path: Path | None,
    input_paths: list[Path],
    outputs_dir: Path,
) -> str:
    lines = [
        "# Skill Eval Run",
        "",
        f"Configuration: {config}",
        f"Skill name: {skill_name}",
        "",
    ]
    if installed_skill_path:
        lines.extend(
            [
                f"Skill path: {installed_skill_path}",
                "Read and follow that skill when it is relevant to the task.",
                "",
            ]
        )
    else:
        lines.extend(
            [
                f"Do not use any skill named `{skill_name}` for this baseline run.",
                "Use only general capabilities and the prompt below.",
                "",
            ]
        )

    lines.extend(
        [
            "## Eval Prompt",
            "",
            case.prompt,
            "",
            "## Expected Output",
            "",
            case.expected_output,
            "",
            "## Input Files",
            "",
        ]
    )
    if input_paths:
        lines.extend(f"- {path}" for path in input_paths)
    else:
        lines.append("- none")
    lines.extend(
        [
            "",
            "## Output Instructions",
            "",
            f"Save final user-facing artifacts under: {outputs_dir}",
            "If you only produce a textual answer, write it to response.md there.",
        ]
    )
    return "\n".join(lines) + "\n"


def output_metrics(outputs_dir: Path, returncode: int) -> dict[str, Any]:
    files = [path for path in outputs_dir.rglob("*") if path.is_file()]
    output_chars = 0
    for path in files:
        try:
            output_chars += len(path.read_text(errors="replace"))
        except UnicodeDecodeError:
            continue
        except OSError:
            continue
    return {
        "tool_calls": {},
        "total_tool_calls": 0,
        "total_steps": 0,
        "files_created": [
            path.relative_to(outputs_dir).as_posix() for path in sorted(files)
        ],
        "errors_encountered": 0 if returncode == 0 else 1,
        "output_chars": output_chars,
        "transcript_chars": 0,
    }


def write_timing(run_dir: Path, start: float, end: float) -> dict[str, Any]:
    duration = end - start
    payload = {
        "duration_ms": round(duration * 1000),
        "total_duration_seconds": round(duration, 4),
    }
    (run_dir / "timing.json").write_text(json.dumps(payload, indent=2) + "\n")
    return payload


def execute_mock(
    run_dir: Path,
    outputs_dir: Path,
    prompt: str,
    config: str,
    case: EvalCase,
) -> int:
    response = [
        f"# Mock response for eval {case.eval_id}",
        "",
        f"Configuration: {config}",
        "",
        "This mock executor verifies the eval artifact pipeline.",
        "",
        "## Prompt",
        "",
        case.prompt,
        "",
        "## Expected Output",
        "",
        case.expected_output,
    ]
    (outputs_dir / "response.md").write_text("\n".join(response) + "\n")
    (run_dir / "transcript.md").write_text(
        prompt
        + "\n--- mock executor ---\n"
        + f"Created {outputs_dir / 'response.md'}\n"
    )
    return 0


def execute_command(
    run_dir: Path,
    outputs_dir: Path,
    prompt_path: Path,
    command_template: str,
    timeout: int,
    config: str,
    skill_path: Path | None,
) -> int:
    project_dir = run_dir / "project"
    project_dir.mkdir(parents=True, exist_ok=True)

    rendered = command_template.format(
        cwd=project_dir,
        prompt_file=prompt_path,
        outputs_dir=outputs_dir,
        run_dir=run_dir,
        config=config,
        skill_path=skill_path or "",
    )
    env = os.environ.copy()
    env.update(
        {
            "SKILL_EVAL_RUN_DIR": str(run_dir),
            "SKILL_EVAL_OUTPUTS_DIR": str(outputs_dir),
            "SKILL_EVAL_PROMPT_FILE": str(prompt_path),
            "SKILL_EVAL_CONFIG": config,
        }
    )
    result = subprocess.run(
        shlex.split(rendered),
        cwd=project_dir,
        env=env,
        capture_output=True,
        text=True,
        timeout=timeout,
        check=False,
    )
    transcript = [
        f"# Command run: {config}",
        "",
        f"Command: `{rendered}`",
        f"Return code: {result.returncode}",
        "",
        "## Stdout",
        "",
        result.stdout,
        "",
        "## Stderr",
        "",
        result.stderr,
    ]
    (run_dir / "transcript.md").write_text("\n".join(transcript))
    if result.stdout.strip() and not any(outputs_dir.iterdir()):
        (outputs_dir / "response.md").write_text(result.stdout)
    return result.returncode


def run_one(
    *,
    skill_path: Path,
    skill_name: str,
    baseline_skill_path: Path | None,
    case: EvalCase,
    eval_dir: Path,
    config: str,
    run_number: int,
    executor: str,
    command_template: str | None,
    grader: str,
    grader_command_template: str | None,
    timeout: int,
) -> Path:
    run_dir = eval_dir / config / f"run-{run_number}"
    outputs_dir = run_dir / "outputs"
    outputs_dir.mkdir(parents=True, exist_ok=True)

    project_dir = run_dir / "project"
    installed_skill_path: Path | None = None
    if config == "with_skill":
        installed_skill_path = copy_skill(skill_path, project_dir, skill_name)[0]
    elif config == "old_skill" and baseline_skill_path:
        installed_skill_path = copy_skill(baseline_skill_path, project_dir, skill_name)[0]

    input_paths = sorted((eval_dir / "inputs").rglob("*"))
    input_files = [path for path in input_paths if path.is_file()]
    prompt = build_prompt(
        case, config, skill_name, installed_skill_path, input_files, outputs_dir
    )
    prompt_path = run_dir / "prompt.md"
    prompt_path.write_text(prompt)

    start = time.monotonic()
    if executor == "mock":
        returncode = execute_mock(run_dir, outputs_dir, prompt, config, case)
    else:
        if not command_template:
            raise ValueError("--command-template is required for command executor")
        returncode = execute_command(
            run_dir,
            outputs_dir,
            prompt_path,
            command_template,
            timeout,
            config,
            installed_skill_path,
        )
    end = time.monotonic()

    timing = write_timing(run_dir, start, end)
    metrics = output_metrics(outputs_dir, returncode)
    metrics["transcript_chars"] = len((run_dir / "transcript.md").read_text())
    (outputs_dir / "metrics.json").write_text(json.dumps(metrics, indent=2) + "\n")
    grade_payload = grade_run(
        run_dir,
        grader=grader,
        command_template=grader_command_template,
        timeout=timeout,
    )
    grade_payload["timing"] = timing
    grade_payload["execution_metrics"] = metrics
    (run_dir / "grading.json").write_text(json.dumps(grade_payload, indent=2) + "\n")
    return run_dir


def load_review_module(skill_creator_dir: Path):
    path = skill_creator_dir / "eval-viewer" / "generate_review.py"
    spec = importlib.util.spec_from_file_location("generate_review", path)
    module = importlib.util.module_from_spec(spec)
    if not spec or not spec.loader:
        raise RuntimeError(f"Cannot load {path}")
    spec.loader.exec_module(module)
    return module


def run_skill_evals(
    skill_path: str | Path,
    workspace: str | Path | None = None,
    iteration: str | None = None,
    executor: str = "mock",
    command_template: str | None = None,
    baseline_skill_path: str | Path | None = None,
    runs_per_config: int = 1,
    grader: str = "deterministic",
    grader_command_template: str | None = None,
    timeout: int = 600,
    review: bool = True,
) -> dict[str, Path]:
    skill_path = Path(skill_path).resolve()
    valid, message = validate_skill(skill_path, target="all")
    if not valid:
        raise ValueError(f"Skill validation failed: {message}")

    skill_name, cases = load_evals(skill_path)
    workspace_path = (
        Path(workspace).resolve()
        if workspace
        else skill_path.parent / f"{skill_name}-workspace"
    )
    iteration_dir = next_iteration_dir(workspace_path, iteration)
    iteration_dir.mkdir(parents=True, exist_ok=False)

    baseline_path = Path(baseline_skill_path).resolve() if baseline_skill_path else None
    baseline_config = "old_skill" if baseline_path else "without_skill"
    configs = ["with_skill", baseline_config]

    for case in cases:
        eval_name = slugify(case.prompt)
        eval_dir = iteration_dir / f"eval-{case.eval_id}-{eval_name}"
        eval_dir.mkdir(parents=True)
        write_eval_metadata(eval_dir, case, eval_name)
        copy_eval_inputs(skill_path, case, eval_dir)
        for config in configs:
            for run_number in range(1, runs_per_config + 1):
                run_one(
                    skill_path=skill_path,
                    skill_name=skill_name,
                    baseline_skill_path=baseline_path,
                    case=case,
                    eval_dir=eval_dir,
                    config=config,
                    run_number=run_number,
                    executor=executor,
                    command_template=command_template,
                    grader=grader,
                    grader_command_template=grader_command_template,
                    timeout=timeout,
                )

    benchmark_json, benchmark_md, _ = write_benchmark(
        iteration_dir, skill_name=skill_name, skill_path=str(skill_path)
    )
    review_html = iteration_dir / "review.html"
    if review:
        viewer = load_review_module(Path(__file__).resolve().parents[1])
        review_html = viewer.generate_review(
            iteration_dir,
            skill_name=skill_name,
            benchmark_path=benchmark_json,
            output_path=review_html,
        )

    return {
        "workspace": workspace_path,
        "iteration": iteration_dir,
        "benchmark_json": benchmark_json,
        "benchmark_md": benchmark_md,
        "review_html": review_html,
    }


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Run skill evals")
    parser.add_argument("skill_path", type=Path)
    parser.add_argument("--workspace", type=Path)
    parser.add_argument("--iteration")
    parser.add_argument(
        "--executor",
        choices=["mock", "command"],
        default="mock",
        help="mock is for local smoke tests; command runs an agent CLI template",
    )
    parser.add_argument(
        "--command-template",
        help=(
            "Command executor template. Placeholders: {cwd}, {prompt_file}, "
            "{outputs_dir}, {run_dir}, {config}, {skill_path}"
        ),
    )
    parser.add_argument("--baseline-skill-path", type=Path)
    parser.add_argument("--runs-per-config", type=int, default=1)
    parser.add_argument(
        "--grader",
        choices=["deterministic", "mock"],
        default="deterministic",
    )
    parser.add_argument("--grader-command-template")
    parser.add_argument("--timeout", type=int, default=600)
    parser.add_argument("--no-review", action="store_true")
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.executor == "command" and not args.command_template:
        print("--command-template is required with --executor command", file=sys.stderr)
        raise SystemExit(2)
    result = run_skill_evals(
        args.skill_path,
        workspace=args.workspace,
        iteration=args.iteration,
        executor=args.executor,
        command_template=args.command_template,
        baseline_skill_path=args.baseline_skill_path,
        runs_per_config=args.runs_per_config,
        grader=args.grader,
        grader_command_template=args.grader_command_template,
        timeout=args.timeout,
        review=not args.no_review,
    )
    for key, path in result.items():
        print(f"{key}: {path}")


if __name__ == "__main__":
    main()
