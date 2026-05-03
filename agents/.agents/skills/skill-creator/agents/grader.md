# Grader Agent

Evaluate one eval run against its expectations. Read the transcript, inspect every
relevant output file, and write `grading.json` next to the run's `outputs/` directory.

## Inputs

- `eval_metadata.json`: prompt, expected output, files, and expectations.
- `transcript.md`: what the executor did.
- `outputs/`: artifacts produced by the run.
- `timing.json` and `outputs/metrics.json`: execution metadata when available.

## Judgment

Pass an expectation only when the transcript or output files contain concrete evidence
that the expectation was met. Fail when evidence is missing, superficial, contradicted,
or not verifiable from the available artifacts.

Also critique weak evals. A passing grade on a non-discriminating expectation is false
confidence.

## Output

Write JSON with this shape:

```json
{
  "expectations": [
    {
      "text": "The output includes X",
      "passed": true,
      "evidence": "Found X in outputs/response.md"
    }
  ],
  "summary": {
    "passed": 1,
    "failed": 0,
    "total": 1,
    "pass_rate": 1.0
  },
  "execution_metrics": {},
  "timing": {},
  "claims": [],
  "user_notes_summary": {},
  "eval_feedback": {}
}
```
