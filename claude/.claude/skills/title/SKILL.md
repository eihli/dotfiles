---
name: title
description: |
  Set terminal title to 2-word summary of current conversation.
  Triggers: "/title", "update title", "set title"
---

# Title

Set terminal title via escape sequence.

## Process

1. Reflect on conversation: What's the main topic or current task?
2. Generate exactly 2 words (Title Case) summarizing it
3. Run: `mkdir -p ~/.cache/claude-code/titles && TITLE="Two Words" && printf '\033]0;%s\007' "$TITLE" && echo "$TITLE" > ~/.cache/claude-code/titles/$(pwd | tr '/' '_')`

## Examples

- Debugging auth flow → "Auth Debugging"
- Setting up CI pipeline → "CI Setup"
- Refactoring database queries → "Query Refactor"
- General chat, no clear focus → "Claude Chat"
