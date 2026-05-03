#!/usr/bin/env python3
"""Grade one skill-eval run and write grading.json.

The default grader is intentionally conservative and deterministic. It handles
machine-checkable expectations such as `output_contains: text` and
`file_exists: path`; free-form natural-language expectations fail closed unless
an external grader command is supplied.
"""

from __future__ import annotations

import argparse
import json
import shlex
import subprocess
from pathlib import Path
from typing import Any


TEXT_EXTENSIONS = {
    ".txt",
    ".md",
    ".json",
    ".csv",
    ".py",
    ".js",
    ".ts",
    ".tsx",
    ".jsx",
    ".yaml",
    ".yml",
    ".xml",
    ".html",
    ".css",
    ".sh",
    ".toml",
}
MAX_TEXT_BYTES = 512_000


def load_json(path: Path, default: Any) -> Any:
    if not path.exists():
        return default
    try:
        return json.loads(path.read_text())
    except json.JSONDecodeError:
        return default


def load_eval_metadata(run_dir: Path) -> dict[str, Any]:
    candidates = [
        run_dir / "eval_metadata.json",
        run_dir.parent / "eval_metadata.json",
        run_dir.parent.parent / "eval_metadata.json",
    ]
    for path in candidates:
        if path.exists():
            payload = load_json(path, {})
            if isinstance(payload, dict):
                return payload
    return {}


def read_text_file(path: Path) -> str:
    try:
        raw = path.read_bytes()[:MAX_TEXT_BYTES]
    except OSError:
        return ""
    return raw.decode("utf-8", errors="replace")


def collect_output_text(outputs_dir: Path) -> tuple[str, list[str]]:
    parts: list[str] = []
    files: list[str] = []
    if not outputs_dir.exists():
        return "", files

    for path in sorted(outputs_dir.rglob("*")):
        if not path.is_file():
            continue
        rel = path.relative_to(outputs_dir).as_posix()
        files.append(rel)
        if path.suffix.lower() in TEXT_EXTENSIONS:
            parts.append(f"\n--- {rel} ---\n{read_text_file(path)}")
    return "\n".join(parts), files


def expectation_rule(expectation: str) -> tuple[str, str]:
    if ":" not in expectation:
        return "natural_language", expectation
    name, value = expectation.split(":", 1)
    return name.strip().lower().replace("-", "_"), value.strip()


def grade_expectation(
    expectation: str,
    transcript: str,
    output_text: str,
    output_files: list[str],
    outputs_dir: Path,
) -> dict[str, Any]:
    rule, value = expectation_rule(expectation)
    transcript_lower = transcript.lower()
    output_lower = output_text.lower()
    value_lower = value.lower()

    if rule in {"contains", "output_contains"}:
        passed = value_lower in output_lower
        evidence = (
            f"Found `{value}` in output text."
            if passed
            else f"`{value}` was not found in output text."
        )
    elif rule == "transcript_contains":
        passed = value_lower in transcript_lower
        evidence = (
            f"Found `{value}` in transcript."
            if passed
            else f"`{value}` was not found in transcript."
        )
    elif rule == "file_exists":
        passed = (outputs_dir / value).exists()
        evidence = (
            f"Found output file `{value}`."
            if passed
            else f"Output file `{value}` was not found. Files: {output_files}"
        )
    elif rule == "file_absent":
        passed = not (outputs_dir / value).exists()
        evidence = (
            f"Output file `{value}` is absent."
            if passed
            else f"Output file `{value}` exists but should be absent."
        )
    else:
        passed = False
        evidence = (
            "No deterministic grading rule recognized. Use prefixes like "
            "`output_contains:`, `transcript_contains:`, `file_exists:`, or "
            "provide --grader-command-template for natural-language grading."
        )

    return {"text": expectation, "passed": passed, "evidence": evidence}


def summarize_expectations(expectations: list[dict[str, Any]]) -> dict[str, Any]:
    passed = sum(1 for item in expectations if item.get("passed") is True)
    total = len(expectations)
    failed = total - passed
    pass_rate = passed / total if total else 0.0
    return {
        "passed": passed,
        "failed": failed,
        "total": total,
        "pass_rate": round(pass_rate, 4),
    }


def build_grader_input(run_dir: Path, expectations: list[str]) -> dict[str, Any]:
    outputs_dir = run_dir / "outputs"
    transcript_path = run_dir / "transcript.md"
    output_text, output_files = collect_output_text(outputs_dir)
    return {
        "run_dir": str(run_dir),
        "outputs_dir": str(outputs_dir),
        "transcript_path": str(transcript_path),
        "expectations": expectations,
        "transcript": read_text_file(transcript_path),
        "output_text": output_text,
        "output_files": output_files,
        "timing": load_json(run_dir / "timing.json", {}),
        "metrics": load_json(outputs_dir / "metrics.json", {}),
    }


def normalize_grading(payload: dict[str, Any]) -> dict[str, Any]:
    expectations = payload.get("expectations", [])
    if not isinstance(expectations, list):
        expectations = []

    normalized_expectations = []
    for item in expectations:
        if not isinstance(item, dict):
            continue
        normalized_expectations.append(
            {
                "text": str(item.get("text", "")),
                "passed": item.get("passed") is True,
                "evidence": str(item.get("evidence", "")),
            }
        )

    payload["expectations"] = normalized_expectations
    payload["summary"] = payload.get("summary") or summarize_expectations(
        normalized_expectations
    )
    payload.setdefault("execution_metrics", {})
    payload.setdefault("timing", {})
    payload.setdefault("claims", [])
    payload.setdefault("user_notes_summary", {})
    payload.setdefault("eval_feedback", {})
    return payload


def run_command_grader(
    run_dir: Path,
    expectations: list[str],
    command_template: str,
    timeout: int,
) -> dict[str, Any]:
    grader_input = build_grader_input(run_dir, expectations)
    input_path = run_dir / "grader_input.json"
    grading_path = run_dir / "grading.json"
    input_path.write_text(json.dumps(grader_input, indent=2) + "\n")

    rendered = command_template.format(
        run_dir=run_dir,
        outputs_dir=run_dir / "outputs",
        transcript_path=run_dir / "transcript.md",
        grader_input=input_path,
        grading_json=grading_path,
    )
    result = subprocess.run(
        shlex.split(rendered),
        cwd=run_dir,
        capture_output=True,
        text=True,
        timeout=timeout,
        check=False,
    )

    command_log = {
        "command": rendered,
        "returncode": result.returncode,
        "stdout": result.stdout,
        "stderr": result.stderr,
    }
    (run_dir / "grader_command.json").write_text(
        json.dumps(command_log, indent=2) + "\n"
    )

    if grading_path.exists():
        payload = load_json(grading_path, {})
        if isinstance(payload, dict):
            return normalize_grading(payload)

    try:
        payload = json.loads(result.stdout)
    except json.JSONDecodeError:
        return normalize_grading(
            {
                "expectations": [
                    {
                        "text": item,
                        "passed": False,
                        "evidence": (
                            "External grader did not write grading.json or emit "
                            "valid JSON on stdout."
                        ),
                    }
                    for item in expectations
                ],
                "eval_feedback": {
                    "overall": "External grader output was not parseable JSON."
                },
            }
        )

    if not isinstance(payload, dict):
        payload = {}
    return normalize_grading(payload)


def grade_run(
    run_dir: str | Path,
    expectations: list[str] | None = None,
    grader: str = "deterministic",
    command_template: str | None = None,
    timeout: int = 300,
) -> dict[str, Any]:
    run_dir = Path(run_dir)
    outputs_dir = run_dir / "outputs"
    metadata = load_eval_metadata(run_dir)
    if expectations is None:
        raw_expectations = metadata.get("expectations", [])
        expectations = [
            item for item in raw_expectations if isinstance(item, str) and item.strip()
        ]

    if grader == "mock":
        graded = [
            {
                "text": expectation,
                "passed": True,
                "evidence": "Mock grader marks expectations as passed.",
            }
            for expectation in expectations
        ]
        payload = normalize_grading({"expectations": graded})
    elif command_template:
        payload = run_command_grader(run_dir, expectations, command_template, timeout)
    else:
        transcript = read_text_file(run_dir / "transcript.md")
        output_text, output_files = collect_output_text(outputs_dir)
        graded = [
            grade_expectation(
                expectation, transcript, output_text, output_files, outputs_dir
            )
            for expectation in expectations
        ]
        payload = normalize_grading(
            {
                "expectations": graded,
                "execution_metrics": load_json(outputs_dir / "metrics.json", {}),
                "timing": load_json(run_dir / "timing.json", {}),
            }
        )

    (run_dir / "grading.json").write_text(json.dumps(payload, indent=2) + "\n")
    return payload


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Grade a skill eval run")
    parser.add_argument("run_dir", type=Path)
    parser.add_argument(
        "--grader",
        choices=["deterministic", "mock"],
        default="deterministic",
        help="grading mode when no external command template is supplied",
    )
    parser.add_argument(
        "--grader-command-template",
        help=(
            "External grader command. Placeholders: {grader_input}, "
            "{grading_json}, {run_dir}, {outputs_dir}, {transcript_path}"
        ),
    )
    parser.add_argument("--timeout", type=int, default=300)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    payload = grade_run(
        args.run_dir,
        grader=args.grader,
        command_template=args.grader_command_template,
        timeout=args.timeout,
    )
    print(json.dumps(payload["summary"], indent=2))


if __name__ == "__main__":
    main()
