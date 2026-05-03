import importlib.util
import io
import json
import sys
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCRIPTS = ROOT / "scripts"
if str(SCRIPTS) not in sys.path:
    sys.path.insert(0, str(SCRIPTS))


def load_script(name):
    path = SCRIPTS / f"{name}.py"
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    assert spec and spec.loader
    sys.modules[name] = module
    spec.loader.exec_module(module)
    return module


def load_file(path):
    spec = importlib.util.spec_from_file_location(path.stem, path)
    module = importlib.util.module_from_spec(spec)
    assert spec and spec.loader
    sys.modules[path.stem] = module
    spec.loader.exec_module(module)
    return module


init_skill = load_script("init_skill")
quick_validate = load_script("quick_validate")
grade_run = load_script("grade_run")
aggregate_benchmark = load_script("aggregate_benchmark")
run_skill_evals = load_script("run_skill_evals")
generate_review = load_file(ROOT / "eval-viewer" / "generate_review.py")


class InitSkillTests(unittest.TestCase):
    def init_quietly(self, *args, **kwargs):
        with redirect_stdout(io.StringIO()):
            return init_skill.init_skill(*args, **kwargs)

    def test_default_scaffold_is_common_denominator(self):
        with tempfile.TemporaryDirectory() as tmp:
            skill_dir = self.init_quietly(
                "demo-skill",
                tmp,
                resources=set(),
                targets={"common"},
                include_evals=False,
            )

            self.assertIsNotNone(skill_dir)
            assert skill_dir is not None
            text = (skill_dir / "SKILL.md").read_text()
            self.assertIn("name: demo-skill", text)
            self.assertIn("description:", text)
            self.assertNotIn("disable-model-invocation", text)
            self.assertFalse((skill_dir / "scripts").exists())
            self.assertFalse((skill_dir / "references").exists())
            self.assertFalse((skill_dir / "assets").exists())
            self.assertFalse((skill_dir / "agents" / "openai.yaml").exists())

            valid, message = quick_validate.validate_skill(skill_dir, target="common")
            self.assertTrue(valid, message)

    def test_targeted_scaffold_adds_requested_files(self):
        with tempfile.TemporaryDirectory() as tmp:
            skill_dir = self.init_quietly(
                "demo-skill",
                tmp,
                resources={"scripts", "references"},
                targets={"claude", "codex", "opencode"},
                include_evals=True,
            )

            self.assertIsNotNone(skill_dir)
            assert skill_dir is not None
            text = (skill_dir / "SKILL.md").read_text()
            self.assertIn("## Claude Code Notes", text)
            self.assertIn("## Codex Notes", text)
            self.assertIn("## OpenCode Notes", text)
            self.assertTrue((skill_dir / "scripts" / "example.py").exists())
            self.assertTrue((skill_dir / "references" / "reference.md").exists())
            self.assertTrue((skill_dir / "agents" / "openai.yaml").exists())
            self.assertTrue((skill_dir / "evals" / "evals.json").exists())
            self.assertTrue((skill_dir / "evals" / "trigger-evals.json").exists())

            valid, message = quick_validate.validate_skill(skill_dir, target="all")
            self.assertTrue(valid, message)
            valid, message = quick_validate.validate_skill(skill_dir, target="common")
            self.assertTrue(valid, message)


class QuickValidateTests(unittest.TestCase):
    def write_skill(self, root, name, frontmatter, body="Body"):
        skill_dir = Path(root) / name
        skill_dir.mkdir()
        (skill_dir / "SKILL.md").write_text(f"---\n{frontmatter}\n---\n\n{body}\n")
        return skill_dir

    def test_target_validation_distinguishes_claude_from_opencode(self):
        with tempfile.TemporaryDirectory() as tmp:
            skill_dir = self.write_skill(
                tmp,
                "manual-skill",
                "\n".join(
                    [
                        "name: manual-skill",
                        "description: Manual workflow",
                        "disable-model-invocation: true",
                    ]
                ),
            )

            valid, message = quick_validate.validate_skill(skill_dir, target="claude")
            self.assertTrue(valid, message)
            valid, message = quick_validate.validate_skill(skill_dir, target="all")
            self.assertTrue(valid, message)
            valid, message = quick_validate.validate_skill(skill_dir, target="opencode")
            self.assertFalse(valid)
            self.assertIn("disable-model-invocation", message)

    def test_eval_schema_is_validated(self):
        with tempfile.TemporaryDirectory() as tmp:
            skill_dir = self.write_skill(
                tmp,
                "eval-skill",
                "name: eval-skill\ndescription: Skill with evals",
            )
            evals_dir = skill_dir / "evals"
            evals_dir.mkdir()
            (evals_dir / "evals.json").write_text(
                '{"skill_name": "other-skill", "evals": []}'
            )

            valid, message = quick_validate.validate_skill(skill_dir)
            self.assertFalse(valid)
            self.assertIn("skill_name must match", message)

    def test_legacy_eval_schema_is_rejected_with_migration_hint(self):
        with tempfile.TemporaryDirectory() as tmp:
            skill_dir = self.write_skill(
                tmp,
                "eval-skill",
                "name: eval-skill\ndescription: Skill with evals",
            )
            evals_dir = skill_dir / "evals"
            evals_dir.mkdir()
            (evals_dir / "evals.json").write_text(
                '{"version": 1, "skill": "eval-skill", "cases": []}'
            )

            valid, message = quick_validate.validate_skill(skill_dir)
            self.assertFalse(valid)
            self.assertIn("old local schema", message)

    def test_eval_id_boolean_is_rejected(self):
        with tempfile.TemporaryDirectory() as tmp:
            skill_dir = self.write_skill(
                tmp,
                "eval-skill",
                "name: eval-skill\ndescription: Skill with evals",
            )
            evals_dir = skill_dir / "evals"
            evals_dir.mkdir()
            (evals_dir / "evals.json").write_text(
                '{"skill_name": "eval-skill", "evals": ['
                '{"id": true, "prompt": "Do it", '
                '"expected_output": "Done", "files": [], "expectations": []}'
                "]}"
            )

            valid, message = quick_validate.validate_skill(skill_dir)
            self.assertFalse(valid)
            self.assertIn("id must be an integer", message)

    def test_trigger_eval_schema_is_validated(self):
        with tempfile.TemporaryDirectory() as tmp:
            skill_dir = self.write_skill(
                tmp,
                "trigger-skill",
                "name: trigger-skill\ndescription: Skill with trigger evals",
            )
            evals_dir = skill_dir / "evals"
            evals_dir.mkdir()
            (evals_dir / "trigger-evals.json").write_text(
                '[{"query": "Use it", "should_trigger": "yes"}]'
            )

            valid, message = quick_validate.validate_skill(skill_dir)
            self.assertFalse(valid)
            self.assertIn("should_trigger must be boolean", message)


class EvalRunnerTests(unittest.TestCase):
    def write_eval_skill(self, root):
        skill_dir = Path(root) / "eval-skill"
        skill_dir.mkdir()
        (skill_dir / "SKILL.md").write_text(
            "---\n"
            "name: eval-skill\n"
            "description: Skill with runnable evals\n"
            "---\n\n"
            "# Eval Skill\n"
        )
        evals_dir = skill_dir / "evals"
        evals_dir.mkdir()
        (evals_dir / "evals.json").write_text(
            json.dumps(
                {
                    "skill_name": "eval-skill",
                    "evals": [
                        {
                            "id": 1,
                            "prompt": "Return a compact answer.",
                            "expected_output": "A response file is created.",
                            "files": [],
                            "expectations": [
                                "file_exists: response.md",
                                "output_contains: Mock response",
                            ],
                        }
                    ],
                },
                indent=2,
            )
        )
        return skill_dir

    def test_deterministic_grader_checks_outputs(self):
        with tempfile.TemporaryDirectory() as tmp:
            run_dir = Path(tmp) / "eval-1" / "with_skill" / "run-1"
            outputs = run_dir / "outputs"
            outputs.mkdir(parents=True)
            (run_dir.parent.parent / "eval_metadata.json").write_text(
                json.dumps(
                    {
                        "eval_id": 1,
                        "expectations": [
                            "file_exists: response.md",
                            "output_contains: hello",
                        ],
                    }
                )
            )
            (run_dir / "transcript.md").write_text("wrote response")
            (outputs / "response.md").write_text("hello world\n")

            payload = grade_run.grade_run(run_dir)
            self.assertEqual(payload["summary"]["pass_rate"], 1.0)
            self.assertTrue((run_dir / "grading.json").exists())

    def test_file_expectations_cannot_escape_outputs_dir(self):
        with tempfile.TemporaryDirectory() as tmp:
            run_dir = Path(tmp) / "eval-1" / "with_skill" / "run-1"
            outputs = run_dir / "outputs"
            outputs.mkdir(parents=True)
            (run_dir.parent.parent / "eval_metadata.json").write_text(
                json.dumps(
                    {
                        "eval_id": 1,
                        "expectations": ["file_exists: ../transcript.md"],
                    }
                )
            )
            (run_dir / "transcript.md").write_text("outside outputs")

            payload = grade_run.grade_run(run_dir)
            self.assertEqual(payload["summary"]["pass_rate"], 0.0)
            self.assertIn(
                "must stay under",
                payload["expectations"][0]["evidence"],
            )

    def test_external_grader_summary_is_recomputed(self):
        with tempfile.TemporaryDirectory() as tmp:
            run_dir = Path(tmp) / "eval-1" / "with_skill" / "run-1"
            outputs = run_dir / "outputs"
            outputs.mkdir(parents=True)
            (run_dir.parent.parent / "eval_metadata.json").write_text(
                json.dumps({"eval_id": 1, "expectations": ["external check"]})
            )
            helper = Path(tmp) / "write_bad_summary.py"
            helper.write_text(
                "import json, pathlib, sys\n"
                "pathlib.Path(sys.argv[1]).write_text(json.dumps({\n"
                "  'expectations': ["
                "{'text': 'external check', 'passed': True, 'evidence': 'ok'}],\n"
                "  'summary': {'passed': 0, 'failed': 1, 'total': 1, "
                "'pass_rate': 0.0}\n"
                "}))\n"
            )

            payload = grade_run.grade_run(
                run_dir,
                command_template=f"python3 {helper} {{grading_json}}",
            )
            self.assertEqual(payload["summary"]["passed"], 1)
            self.assertEqual(payload["summary"]["pass_rate"], 1.0)

    def test_aggregate_benchmark_reads_grading_artifacts(self):
        with tempfile.TemporaryDirectory() as tmp:
            iteration = Path(tmp) / "iteration-1"
            run_dir = iteration / "eval-1-sample" / "with_skill" / "run-1"
            run_dir.mkdir(parents=True)
            (iteration / "eval-1-sample" / "eval_metadata.json").write_text(
                json.dumps({"eval_id": 1, "eval_name": "sample"})
            )
            (run_dir / "grading.json").write_text(
                json.dumps(
                    {
                        "expectations": [
                            {"text": "file_exists: response.md", "passed": True}
                        ],
                        "summary": {
                            "passed": 1,
                            "failed": 0,
                            "total": 1,
                            "pass_rate": 1.0,
                        },
                    }
                )
            )
            (run_dir / "timing.json").write_text(
                json.dumps({"total_duration_seconds": 0.5, "total_tokens": 10})
            )

            output_json, output_md, benchmark = aggregate_benchmark.write_benchmark(
                iteration, skill_name="eval-skill", skill_path="/tmp/eval-skill"
            )
            self.assertTrue(output_json.exists())
            self.assertTrue(output_md.exists())
            self.assertEqual(
                benchmark["run_summary"]["with_skill"]["pass_rate"]["mean"], 1.0
            )

    def test_run_skill_evals_mock_pipeline_creates_review(self):
        with tempfile.TemporaryDirectory() as tmp:
            skill_dir = self.write_eval_skill(tmp)
            workspace = Path(tmp) / "workspace"

            result = run_skill_evals.run_skill_evals(
                skill_dir,
                workspace=workspace,
                executor="mock",
                grader="deterministic",
            )

            self.assertTrue(result["benchmark_json"].exists())
            self.assertTrue(result["benchmark_md"].exists())
            self.assertTrue(result["review_html"].exists())
            self.assertTrue(
                (
                    result["iteration"]
                    / "eval-1-return-a-compact-answer"
                    / "with_skill"
                    / "run-1"
                    / "grading.json"
                ).exists()
            )

    def test_run_skill_evals_command_executor_creates_outputs(self):
        with tempfile.TemporaryDirectory() as tmp:
            skill_dir = self.write_eval_skill(tmp)
            workspace = Path(tmp) / "workspace"
            helper = Path(tmp) / "helper.py"
            helper.write_text(
                "import os, pathlib, sys\n"
                "pathlib.Path(os.environ['SKILL_EVAL_OUTPUTS_DIR'], "
                "'response.md').write_text('command executor output\\n')\n"
                "print('wrote output from', sys.argv[1])\n"
            )
            helper.chmod(0o755)

            result = run_skill_evals.run_skill_evals(
                skill_dir,
                workspace=workspace,
                executor="command",
                command_template=f"python3 {helper} {{prompt_file}}",
                grader="deterministic",
            )

            grading = json.loads(
                (
                    result["iteration"]
                    / "eval-1-return-a-compact-answer"
                    / "with_skill"
                    / "run-1"
                    / "grading.json"
                ).read_text()
            )
            self.assertGreater(grading["summary"]["pass_rate"], 0.0)

    def test_generate_review_writes_static_html(self):
        with tempfile.TemporaryDirectory() as tmp:
            iteration = Path(tmp) / "iteration-1"
            run_dir = iteration / "eval-1-sample" / "with_skill" / "run-1"
            outputs = run_dir / "outputs"
            outputs.mkdir(parents=True)
            (iteration / "eval-1-sample" / "eval_metadata.json").write_text(
                json.dumps({"eval_id": 1, "eval_name": "sample", "prompt": "Do it"})
            )
            (outputs / "response.md").write_text("done\n")
            (run_dir / "grading.json").write_text(
                json.dumps({"expectations": [], "summary": {}})
            )
            output = generate_review.generate_review(iteration, skill_name="sample")
            self.assertTrue(output.exists())
            self.assertIn("sample Eval Review", output.read_text())


if __name__ == "__main__":
    unittest.main()
