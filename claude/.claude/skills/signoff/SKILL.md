---
name: signoff
description: |
  Capture end-of-day notes for tomorrow's session.
  Triggers: "/signoff", "signing off", "end of day", "take note for tomorrow"
---

# Signoff

Capture session state before signing off.

## Process

1. Ask user: "What should I note for tomorrow?" (unless they already provided it)

2. Append to `claude-progress.txt` (create if missing):
```
## Session: [YYYY-MM-DD HH:MM]
- Notes: [user's notes]
- CWD: [current directory]
- Last commit: [git log --oneline -1]
```

3. Confirm: "Notes saved. See you tomorrow."
