#!/bin/bash
# Notification hook for Claude permission requests
# Adds pane context (title or project) to the notification

set -e

INPUT=$(cat)
MSG=$(echo "$INPUT" | jq -r '.message // "Claude needs attention"')
CWD=$(echo "$INPUT" | jq -r '.cwd // empty')

# Project name from cwd
PROJECT=$(basename "$CWD" 2>/dev/null || echo "")

# Title from /title skill (keyed by TTY)
TITLE=""
TTY_KEY=$(tty 2>/dev/null | tr '/' '_' || true)
if [ -n "$TTY_KEY" ] && [ -f "$HOME/.cache/claude-code/titles/$TTY_KEY" ]; then
    TITLE=$(cat "$HOME/.cache/claude-code/titles/$TTY_KEY" 2>/dev/null || true)
fi

# Build context prefix
CTX=""
if [ -n "$TITLE" ]; then
    CTX="[$TITLE] "
elif [ -n "$PROJECT" ]; then
    CTX="[$PROJECT] "
fi

# Send notification
case $(uname) in
    Darwin) terminal-notifier -title 'Claude Code' -message "$CTX$MSG" -sound Glass ;;
    *) [ -n "$DISPLAY" ] && notify-send 'Claude Code' "$CTX$MSG" 2>/dev/null || true ;;
esac
