import importlib.util
import io
import tempfile
import unittest
from contextlib import redirect_stdout
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]


def load_script(name):
    path = ROOT / "scripts" / f"{name}.py"
    spec = importlib.util.spec_from_file_location(name, path)
    module = importlib.util.module_from_spec(spec)
    assert spec and spec.loader
    spec.loader.exec_module(module)
    return module


init_skill = load_script("init_skill")
quick_validate = load_script("quick_validate")


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


if __name__ == "__main__":
    unittest.main()
