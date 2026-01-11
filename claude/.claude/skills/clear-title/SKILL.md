---
name: clear-title
description: |
  Clear manually-set title, reverting to auto-generated task summary.
  Triggers: "/clear-title", "clear title", "reset title"
---

# Clear Title

Remove the manually-set title so notifications fall back to task summary.

## Process

1. Run: `rm -f ~/.cache/claude-code/titles/$(tty | tr '/' '_')`
2. Confirm: "Title cleared, notifications will use task summary."
