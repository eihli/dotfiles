#!/usr/bin/env python3
"""Generate a static HTML review page for skill eval artifacts."""

from __future__ import annotations

import argparse
import base64
import html
import json
import mimetypes
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
IMAGE_EXTENSIONS = {".png", ".jpg", ".jpeg", ".gif", ".svg", ".webp"}
MAX_TEXT_BYTES = 512_000


def load_json(path: Path, default: Any) -> Any:
    if not path or not path.exists():
        return default
    try:
        return json.loads(path.read_text())
    except json.JSONDecodeError:
        return default


def read_text(path: Path) -> str:
    try:
        return path.read_bytes()[:MAX_TEXT_BYTES].decode("utf-8", errors="replace")
    except OSError:
        return ""


def embed_file(path: Path, root: Path) -> dict[str, Any]:
    rel = path.relative_to(root).as_posix()
    suffix = path.suffix.lower()
    if suffix in TEXT_EXTENSIONS:
        return {"name": rel, "kind": "text", "content": read_text(path)}
    if suffix in IMAGE_EXTENSIONS:
        mime = mimetypes.guess_type(path.name)[0] or "application/octet-stream"
        try:
            encoded = base64.b64encode(path.read_bytes()).decode("ascii")
        except OSError:
            encoded = ""
        return {
            "name": rel,
            "kind": "image",
            "data_uri": f"data:{mime};base64,{encoded}",
        }
    return {"name": rel, "kind": "binary", "content": "Binary file not rendered."}


def find_eval_metadata(run_dir: Path) -> dict[str, Any]:
    for candidate in (
        run_dir / "eval_metadata.json",
        run_dir.parent / "eval_metadata.json",
        run_dir.parent.parent / "eval_metadata.json",
    ):
        payload = load_json(candidate, {})
        if isinstance(payload, dict) and payload:
            return payload
    return {}


def discover_runs(iteration_dir: Path) -> list[dict[str, Any]]:
    runs: list[dict[str, Any]] = []
    for outputs_dir in sorted(iteration_dir.rglob("outputs")):
        if not outputs_dir.is_dir():
            continue
        run_dir = outputs_dir.parent
        metadata = find_eval_metadata(run_dir)
        outputs = [
            embed_file(path, outputs_dir)
            for path in sorted(outputs_dir.rglob("*"))
            if path.is_file()
        ]
        try:
            run_id = run_dir.relative_to(iteration_dir).as_posix()
        except ValueError:
            run_id = run_dir.as_posix()
        runs.append(
            {
                "id": run_id,
                "eval_id": metadata.get("eval_id"),
                "eval_name": metadata.get("eval_name"),
                "prompt": metadata.get("prompt", ""),
                "expected_output": metadata.get("expected_output", ""),
                "configuration": run_dir.parent.name,
                "outputs": outputs,
                "grading": load_json(run_dir / "grading.json", {}),
                "timing": load_json(run_dir / "timing.json", {}),
                "transcript": read_text(run_dir / "transcript.md"),
            }
        )
    return sorted(
        runs,
        key=lambda item: (
            item.get("eval_id") if item.get("eval_id") is not None else 999999,
            item.get("configuration", ""),
            item.get("id", ""),
        ),
    )


def html_page(skill_name: str, data: dict[str, Any]) -> str:
    data_json = json.dumps(data).replace("</", "<\\/")
    title = html.escape(f"{skill_name} Eval Review")
    return f"""<!doctype html>
<html lang="en">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>{title}</title>
  <style>
    :root {{
      color-scheme: light dark;
      --bg: #f7f7f4;
      --fg: #191a1c;
      --muted: #666b72;
      --line: #d8d9d4;
      --panel: #ffffff;
      --accent: #1d6f63;
      --fail: #9c2f2f;
      --pass: #1d6f63;
    }}
    @media (prefers-color-scheme: dark) {{
      :root {{
        --bg: #151617;
        --fg: #f4f1ea;
        --muted: #a7abae;
        --line: #35383a;
        --panel: #1f2123;
      }}
    }}
    * {{ box-sizing: border-box; }}
    body {{
      margin: 0;
      background: var(--bg);
      color: var(--fg);
      font: 14px/1.45 system-ui, -apple-system, BlinkMacSystemFont, "Segoe UI", sans-serif;
    }}
    header {{
      position: sticky;
      top: 0;
      z-index: 1;
      padding: 14px 20px;
      border-bottom: 1px solid var(--line);
      background: var(--bg);
      display: flex;
      align-items: center;
      justify-content: space-between;
      gap: 12px;
    }}
    h1 {{ margin: 0; font-size: 18px; }}
    main {{ max-width: 1180px; margin: 0 auto; padding: 20px; }}
    button {{
      border: 1px solid var(--line);
      background: var(--panel);
      color: var(--fg);
      border-radius: 6px;
      padding: 7px 10px;
      cursor: pointer;
    }}
    button.active {{ border-color: var(--accent); color: var(--accent); }}
    .tabs {{ display: flex; gap: 8px; margin-bottom: 16px; }}
    .grid {{ display: grid; grid-template-columns: 300px 1fr; gap: 16px; }}
    .list, .detail, .bench {{
      background: var(--panel);
      border: 1px solid var(--line);
      border-radius: 8px;
    }}
    .list {{ overflow: hidden; }}
    .run-button {{
      display: block;
      width: 100%;
      text-align: left;
      border: 0;
      border-bottom: 1px solid var(--line);
      border-radius: 0;
      background: transparent;
      padding: 10px 12px;
    }}
    .run-button.active {{ background: color-mix(in srgb, var(--accent) 10%, transparent); }}
    .detail {{ padding: 16px; min-width: 0; }}
    .muted {{ color: var(--muted); }}
    pre {{
      white-space: pre-wrap;
      overflow-wrap: anywhere;
      background: color-mix(in srgb, var(--panel) 84%, var(--fg) 6%);
      padding: 12px;
      border-radius: 6px;
      border: 1px solid var(--line);
      max-height: 480px;
      overflow: auto;
    }}
    textarea {{
      width: 100%;
      min-height: 120px;
      resize: vertical;
      border: 1px solid var(--line);
      border-radius: 6px;
      padding: 10px;
      background: var(--bg);
      color: var(--fg);
      font: inherit;
    }}
    table {{ width: 100%; border-collapse: collapse; }}
    th, td {{ border-bottom: 1px solid var(--line); padding: 8px; text-align: left; }}
    .pass {{ color: var(--pass); font-weight: 700; }}
    .fail {{ color: var(--fail); font-weight: 700; }}
    .hidden {{ display: none; }}
    .output-image {{ max-width: 100%; border: 1px solid var(--line); border-radius: 6px; }}
    @media (max-width: 820px) {{
      .grid {{ grid-template-columns: 1fr; }}
      header {{ align-items: flex-start; flex-direction: column; }}
    }}
  </style>
</head>
<body>
  <header>
    <h1>{title}</h1>
    <button id="download-feedback">Download feedback.json</button>
  </header>
  <main>
    <div class="tabs">
      <button class="tab active" data-tab="outputs">Outputs</button>
      <button class="tab" data-tab="benchmark">Benchmark</button>
    </div>
    <section id="outputs-tab" class="grid">
      <div class="list" id="run-list"></div>
      <div class="detail" id="run-detail"></div>
    </section>
    <section id="benchmark-tab" class="bench hidden"></section>
  </main>
  <script>const DATA = {data_json};</script>
  <script>
    const feedbackKey = `skill-eval-feedback:${{DATA.skill_name}}:${{DATA.iteration}}`;
    const feedback = JSON.parse(localStorage.getItem(feedbackKey) || "{{}}");
    let selected = DATA.runs[0]?.id || null;

    function esc(value) {{
      return String(value ?? "").replace(/[&<>"']/g, ch => ({{
        "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;", "'": "&#39;"
      }}[ch]));
    }}

    function renderRuns() {{
      const list = document.getElementById("run-list");
      list.innerHTML = DATA.runs.map(run => `
        <button class="run-button ${{run.id === selected ? "active" : ""}}" data-run="${{esc(run.id)}}">
          <strong>${{esc(run.eval_name || `Eval ${{run.eval_id}}`)}}</strong><br>
          <span class="muted">${{esc(run.configuration)}} · ${{esc(run.id)}}</span>
        </button>
      `).join("");
      list.querySelectorAll("button").forEach(button => {{
        button.addEventListener("click", () => {{
          selected = button.dataset.run;
          renderRuns();
          renderDetail();
        }});
      }});
    }}

    function renderExpectation(item) {{
      const cls = item.passed ? "pass" : "fail";
      const label = item.passed ? "PASS" : "FAIL";
      return `<tr><td class="${{cls}}">${{label}}</td><td>${{esc(item.text)}}</td><td>${{esc(item.evidence)}}</td></tr>`;
    }}

    function renderOutput(file) {{
      if (file.kind === "image") {{
        return `<h4>${{esc(file.name)}}</h4><img class="output-image" src="${{file.data_uri}}" alt="${{esc(file.name)}}">`;
      }}
      return `<h4>${{esc(file.name)}}</h4><pre>${{esc(file.content)}}</pre>`;
    }}

    function renderDetail() {{
      const run = DATA.runs.find(item => item.id === selected);
      const detail = document.getElementById("run-detail");
      if (!run) {{
        detail.innerHTML = "<p>No runs found.</p>";
        return;
      }}
      const expectations = run.grading?.expectations || [];
      const outputs = run.outputs || [];
      detail.innerHTML = `
        <h2>${{esc(run.eval_name || `Eval ${{run.eval_id}}`)}}</h2>
        <p class="muted">${{esc(run.configuration)}} · ${{esc(run.id)}}</p>
        <h3>Prompt</h3>
        <pre>${{esc(run.prompt)}}</pre>
        <h3>Expected Output</h3>
        <pre>${{esc(run.expected_output)}}</pre>
        <h3>Grades</h3>
        <table>
          <thead><tr><th>Result</th><th>Expectation</th><th>Evidence</th></tr></thead>
          <tbody>${{expectations.map(renderExpectation).join("") || "<tr><td colspan='3'>No grades.</td></tr>"}}</tbody>
        </table>
        <h3>Outputs</h3>
        ${{outputs.map(renderOutput).join("") || "<p>No output files.</p>"}}
        <h3>Transcript</h3>
        <pre>${{esc(run.transcript)}}</pre>
        <h3>Feedback</h3>
        <textarea id="feedback-box" placeholder="Notes for this run">${{esc(feedback[run.id] || "")}}</textarea>
      `;
      document.getElementById("feedback-box").addEventListener("input", event => {{
        feedback[run.id] = event.target.value;
        localStorage.setItem(feedbackKey, JSON.stringify(feedback));
      }});
    }}

    function renderBenchmark() {{
      const section = document.getElementById("benchmark-tab");
      const bench = DATA.benchmark || {{}};
      const summary = bench.run_summary || {{}};
      const configs = Object.keys(summary).filter(key => key !== "delta");
      const rows = configs.map(config => {{
        const item = summary[config];
        return `<tr>
          <td>${{esc(config)}}</td>
          <td>${{((item.pass_rate?.mean || 0) * 100).toFixed(1)}}%</td>
          <td>${{(item.time_seconds?.mean || 0).toFixed(1)}}s</td>
          <td>${{(item.tokens?.mean || 0).toFixed(0)}}</td>
        </tr>`;
      }}).join("");
      const notes = (bench.notes || []).map(note => `<li>${{esc(note)}}</li>`).join("");
      section.innerHTML = `
        <div style="padding:16px">
          <h2>Benchmark</h2>
          <table>
            <thead><tr><th>Configuration</th><th>Pass Rate</th><th>Time</th><th>Tokens</th></tr></thead>
            <tbody>${{rows || "<tr><td colspan='4'>No benchmark data.</td></tr>"}}</tbody>
          </table>
          <h3>Delta</h3>
          <pre>${{esc(JSON.stringify(summary.delta || {{}}, null, 2))}}</pre>
          <h3>Notes</h3>
          <ul>${{notes || "<li>No notes.</li>"}}</ul>
        </div>
      `;
    }}

    document.querySelectorAll(".tab").forEach(button => {{
      button.addEventListener("click", () => {{
        document.querySelectorAll(".tab").forEach(item => item.classList.remove("active"));
        button.classList.add("active");
        const showBench = button.dataset.tab === "benchmark";
        document.getElementById("outputs-tab").classList.toggle("hidden", showBench);
        document.getElementById("benchmark-tab").classList.toggle("hidden", !showBench);
      }});
    }});

    document.getElementById("download-feedback").addEventListener("click", () => {{
      const payload = {{
        status: "complete",
        reviews: DATA.runs.map(run => ({{
          run_id: run.id,
          feedback: feedback[run.id] || "",
          timestamp: new Date().toISOString()
        }}))
      }};
      const blob = new Blob([JSON.stringify(payload, null, 2) + "\\n"], {{type: "application/json"}});
      const url = URL.createObjectURL(blob);
      const link = document.createElement("a");
      link.href = url;
      link.download = "feedback.json";
      link.click();
      URL.revokeObjectURL(url);
    }});

    renderRuns();
    renderDetail();
    renderBenchmark();
  </script>
</body>
</html>
"""


def generate_review(
    iteration_dir: str | Path,
    skill_name: str = "",
    benchmark_path: str | Path | None = None,
    output_path: str | Path | None = None,
) -> Path:
    iteration_dir = Path(iteration_dir).resolve()
    if output_path is None:
        output_path = iteration_dir / "review.html"
    output_path = Path(output_path)
    benchmark = load_json(Path(benchmark_path), {}) if benchmark_path else {}
    data = {
        "skill_name": skill_name or iteration_dir.parent.name.replace("-workspace", ""),
        "iteration": iteration_dir.name,
        "runs": discover_runs(iteration_dir),
        "benchmark": benchmark,
    }
    output_path.parent.mkdir(parents=True, exist_ok=True)
    output_path.write_text(html_page(data["skill_name"], data))
    return output_path


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Generate skill eval review HTML")
    parser.add_argument("iteration_dir", type=Path)
    parser.add_argument("--skill-name", default="")
    parser.add_argument("--benchmark", type=Path)
    parser.add_argument(
        "--static",
        type=Path,
        help="Output HTML path. Defaults to <iteration_dir>/review.html.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    output = generate_review(
        args.iteration_dir,
        skill_name=args.skill_name,
        benchmark_path=args.benchmark,
        output_path=args.static,
    )
    print(f"Review HTML written to: {output}")


if __name__ == "__main__":
    main()
