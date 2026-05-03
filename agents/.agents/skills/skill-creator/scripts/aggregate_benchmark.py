#!/usr/bin/env python3
"""Aggregate skill-eval grading artifacts into benchmark.json and benchmark.md."""

from __future__ import annotations

import argparse
import json
import math
from datetime import datetime, timezone
from pathlib import Path
from typing import Any


PRIMARY_CONFIGS = {"with_skill": 0, "new_skill": 0}
BASELINE_CONFIGS = {"without_skill": 1, "old_skill": 1}


def load_json(path: Path, default: Any) -> Any:
    if not path.exists():
        return default
    try:
        return json.loads(path.read_text())
    except json.JSONDecodeError:
        return default


def stats(values: list[float]) -> dict[str, float]:
    if not values:
        return {"mean": 0.0, "stddev": 0.0, "min": 0.0, "max": 0.0}
    mean = sum(values) / len(values)
    if len(values) > 1:
        variance = sum((value - mean) ** 2 for value in values) / (len(values) - 1)
        stddev = math.sqrt(variance)
    else:
        stddev = 0.0
    return {
        "mean": round(mean, 4),
        "stddev": round(stddev, 4),
        "min": round(min(values), 4),
        "max": round(max(values), 4),
    }


def config_order(name: str) -> tuple[int, str]:
    if name in PRIMARY_CONFIGS:
        return PRIMARY_CONFIGS[name], name
    if name in BASELINE_CONFIGS:
        return BASELINE_CONFIGS[name], name
    return 2, name


def discover_results(iteration_dir: Path) -> list[dict[str, Any]]:
    runs: list[dict[str, Any]] = []
    for eval_dir in sorted(iteration_dir.glob("eval-*")):
        if not eval_dir.is_dir():
            continue
        metadata = load_json(eval_dir / "eval_metadata.json", {})
        eval_id = metadata.get("eval_id")
        eval_name = metadata.get("eval_name") or eval_dir.name

        for config_dir in sorted(eval_dir.iterdir(), key=lambda path: config_order(path.name)):
            if not config_dir.is_dir():
                continue
            if config_dir.name in {"inputs"}:
                continue
            for run_dir in sorted(config_dir.glob("run-*")):
                grading = load_json(run_dir / "grading.json", {})
                if not isinstance(grading, dict) or not grading:
                    continue
                timing = load_json(run_dir / "timing.json", {})
                metrics = grading.get("execution_metrics") or load_json(
                    run_dir / "outputs" / "metrics.json", {}
                )
                summary = grading.get("summary", {})
                try:
                    run_number = int(run_dir.name.split("-", 1)[1])
                except (IndexError, ValueError):
                    run_number = 1

                runs.append(
                    {
                        "eval_id": eval_id,
                        "eval_name": eval_name,
                        "configuration": config_dir.name,
                        "run_number": run_number,
                        "result": {
                            "pass_rate": summary.get("pass_rate", 0.0),
                            "passed": summary.get("passed", 0),
                            "failed": summary.get("failed", 0),
                            "total": summary.get("total", 0),
                            "time_seconds": timing.get(
                                "total_duration_seconds",
                                grading.get("timing", {}).get(
                                    "total_duration_seconds", 0.0
                                ),
                            ),
                            "tokens": timing.get("total_tokens", 0),
                            "tool_calls": metrics.get("total_tool_calls", 0),
                            "errors": metrics.get("errors_encountered", 0),
                        },
                        "expectations": grading.get("expectations", []),
                        "notes": collect_notes(grading),
                    }
                )
    return runs


def collect_notes(grading: dict[str, Any]) -> list[str]:
    notes: list[str] = []
    user_notes = grading.get("user_notes_summary", {})
    if isinstance(user_notes, dict):
        for key in ("uncertainties", "needs_review", "workarounds"):
            values = user_notes.get(key, [])
            if isinstance(values, list):
                notes.extend(str(value) for value in values)
    eval_feedback = grading.get("eval_feedback", {})
    if isinstance(eval_feedback, dict):
        overall = eval_feedback.get("overall")
        if overall:
            notes.append(str(overall))
    return notes


def summarize_runs(runs: list[dict[str, Any]]) -> dict[str, Any]:
    by_config: dict[str, list[dict[str, Any]]] = {}
    for run in runs:
        by_config.setdefault(run["configuration"], []).append(run)

    summary: dict[str, Any] = {}
    for config in sorted(by_config, key=config_order):
        config_runs = by_config[config]
        summary[config] = {
            "pass_rate": stats(
                [float(run["result"].get("pass_rate", 0.0)) for run in config_runs]
            ),
            "time_seconds": stats(
                [float(run["result"].get("time_seconds", 0.0)) for run in config_runs]
            ),
            "tokens": stats(
                [float(run["result"].get("tokens", 0.0)) for run in config_runs]
            ),
        }

    configs = [config for config in sorted(summary, key=config_order)]
    if configs:
        primary = summary[configs[0]]
        baseline = summary[configs[1]] if len(configs) > 1 else {}
        summary["delta"] = {
            "pass_rate": format_delta(
                primary.get("pass_rate", {}).get("mean", 0.0)
                - baseline.get("pass_rate", {}).get("mean", 0.0),
                digits=2,
            ),
            "time_seconds": format_delta(
                primary.get("time_seconds", {}).get("mean", 0.0)
                - baseline.get("time_seconds", {}).get("mean", 0.0),
                digits=1,
            ),
            "tokens": format_delta(
                primary.get("tokens", {}).get("mean", 0.0)
                - baseline.get("tokens", {}).get("mean", 0.0),
                digits=0,
            ),
        }
    return summary


def format_delta(value: float, digits: int) -> str:
    return f"{value:+.{digits}f}"


def analyze_benchmark(runs: list[dict[str, Any]], summary: dict[str, Any]) -> list[str]:
    notes: list[str] = []
    configs = [config for config in sorted(summary, key=config_order) if config != "delta"]
    if len(configs) >= 2:
        primary = configs[0]
        baseline = configs[1]
        primary_rate = summary[primary]["pass_rate"]["mean"]
        baseline_rate = summary[baseline]["pass_rate"]["mean"]
        if primary_rate <= baseline_rate:
            notes.append(
                f"{primary} did not beat {baseline} on pass rate; inspect outputs "
                "before revising the skill."
            )

    by_expectation: dict[str, set[tuple[str, bool]]] = {}
    for run in runs:
        for expectation in run.get("expectations", []):
            text = expectation.get("text")
            if not text:
                continue
            by_expectation.setdefault(text, set()).add(
                (run["configuration"], expectation.get("passed") is True)
            )
    for text, outcomes in by_expectation.items():
        configs_present = {config for config, _ in outcomes}
        all_passed = all(passed for _, passed in outcomes)
        if len(configs_present) > 1 and all_passed:
            notes.append(
                f"Expectation passes in every configuration and may be weak: {text}"
            )
    return notes


def generate_benchmark(
    iteration_dir: str | Path,
    skill_name: str = "",
    skill_path: str = "",
    executor_model: str = "",
    analyzer_model: str = "",
) -> dict[str, Any]:
    iteration_dir = Path(iteration_dir)
    runs = discover_results(iteration_dir)
    summary = summarize_runs(runs)
    notes = analyze_benchmark(runs, summary)
    eval_ids = sorted(
        {run["eval_id"] for run in runs if run.get("eval_id") is not None}
    )
    runs_per_configuration = 0
    if runs:
        counts: dict[tuple[Any, str], int] = {}
        for run in runs:
            key = (run.get("eval_id"), run["configuration"])
            counts[key] = counts.get(key, 0) + 1
        runs_per_configuration = max(counts.values())

    return {
        "metadata": {
            "skill_name": skill_name,
            "skill_path": str(skill_path),
            "executor_model": executor_model,
            "analyzer_model": analyzer_model,
            "timestamp": datetime.now(timezone.utc).strftime("%Y-%m-%dT%H:%M:%SZ"),
            "evals_run": eval_ids,
            "runs_per_configuration": runs_per_configuration,
        },
        "runs": runs,
        "run_summary": summary,
        "notes": notes,
    }


def generate_markdown(benchmark: dict[str, Any]) -> str:
    metadata = benchmark.get("metadata", {})
    summary = benchmark.get("run_summary", {})
    configs = [config for config in sorted(summary, key=config_order) if config != "delta"]
    delta = summary.get("delta", {})

    lines = [
        f"# Skill Benchmark: {metadata.get('skill_name', '')}",
        "",
        f"**Date**: {metadata.get('timestamp', '')}",
        f"**Skill path**: `{metadata.get('skill_path', '')}`",
        f"**Evals**: {', '.join(map(str, metadata.get('evals_run', [])))}",
        "",
        "## Summary",
        "",
        "| Configuration | Pass Rate | Time | Tokens |",
        "| --- | ---: | ---: | ---: |",
    ]
    for config in configs:
        item = summary.get(config, {})
        pass_rate = item.get("pass_rate", {})
        time_seconds = item.get("time_seconds", {})
        tokens = item.get("tokens", {})
        lines.append(
            "| "
            f"{config} | "
            f"{pass_rate.get('mean', 0.0) * 100:.1f}% +/- "
            f"{pass_rate.get('stddev', 0.0) * 100:.1f}% | "
            f"{time_seconds.get('mean', 0.0):.1f}s +/- "
            f"{time_seconds.get('stddev', 0.0):.1f}s | "
            f"{tokens.get('mean', 0.0):.0f} +/- {tokens.get('stddev', 0.0):.0f} |"
        )

    if delta:
        lines.extend(
            [
                "",
                "## Delta",
                "",
                f"- Pass rate: {delta.get('pass_rate', '+0.00')}",
                f"- Time: {delta.get('time_seconds', '+0.0')}s",
                f"- Tokens: {delta.get('tokens', '+0')}",
            ]
        )

    notes = benchmark.get("notes", [])
    if notes:
        lines.extend(["", "## Notes", ""])
        lines.extend(f"- {note}" for note in notes)
    return "\n".join(lines) + "\n"


def write_benchmark(
    iteration_dir: str | Path,
    skill_name: str = "",
    skill_path: str = "",
    output: str | Path | None = None,
) -> tuple[Path, Path, dict[str, Any]]:
    iteration_dir = Path(iteration_dir)
    benchmark = generate_benchmark(iteration_dir, skill_name, skill_path)
    output_json = Path(output) if output else iteration_dir / "benchmark.json"
    output_md = output_json.with_suffix(".md")
    output_json.write_text(json.dumps(benchmark, indent=2) + "\n")
    output_md.write_text(generate_markdown(benchmark))
    return output_json, output_md, benchmark


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Aggregate skill eval results")
    parser.add_argument("iteration_dir", type=Path)
    parser.add_argument("--skill-name", default="")
    parser.add_argument("--skill-path", default="")
    parser.add_argument("--output", type=Path)
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    output_json, output_md, benchmark = write_benchmark(
        args.iteration_dir,
        skill_name=args.skill_name,
        skill_path=args.skill_path,
        output=args.output,
    )
    print(f"Generated: {output_json}")
    print(f"Generated: {output_md}")
    summary = benchmark.get("run_summary", {})
    for config in sorted(
        [item for item in summary if item != "delta"], key=config_order
    ):
        rate = summary[config]["pass_rate"]["mean"]
        print(f"{config}: {rate * 100:.1f}% pass rate")


if __name__ == "__main__":
    main()
