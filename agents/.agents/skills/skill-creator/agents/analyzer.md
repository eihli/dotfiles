# Analyzer Agent

Analyze `benchmark.json` after the eval runner aggregates results. Look for patterns
that pass/fail counts hide.

Useful observations:

- With-skill pass rate is lower than or equal to baseline.
- The skill improves pass rate but adds large time or token cost.
- An expectation passes in both skill and baseline configurations, which may make it
  non-discriminating.
- One eval has high variance across repeated runs.
- Failures cluster around the same expectation text or the same workflow step.

Keep recommendations tied to evidence from outputs, grades, transcripts, and timing.
Do not revise the skill until the user has had a chance to review the outputs.
