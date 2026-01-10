#!/bin/bash
# Notification hook for Claude Stop event
# Extracts project, Zellij title, and first user message (the task)

set -e

INPUT=$(cat)
CWD=$(echo "$INPUT" | jq -r '.cwd // empty')
TRANSCRIPT=$(echo "$INPUT" | jq -r '.transcript_path // empty')

# Project name from cwd
PROJECT=$(basename "$CWD" 2>/dev/null || echo "unknown")

# Zellij tab title (if in Zellij)
TITLE=""
if [ -n "$ZELLIJ" ]; then
    # Query current tab name via zellij action
    TITLE=$(zellij action query-tab-names 2>/dev/null | head -1 || true)
fi

# First user message = the task (efficient: head + first match)
TASK=""
if [ -n "$TRANSCRIPT" ] && [ -f "$TRANSCRIPT" ]; then
    TASK=$(head -100 "$TRANSCRIPT" | jq -r 'select(.type == "user") | .message.content[0].text // empty' 2>/dev/null | head -1 | cut -c1-80)
fi

# Build message
MSG="$PROJECT"
[ -n "$TITLE" ] && MSG="$TITLE | $MSG"
[ -n "$TASK" ] && MSG="$MSG: $TASK"
[ -z "$TASK" ] && MSG="$MSG: Ready"

# Send notification
case $(uname) in
    Darwin) terminal-notifier -title 'Claude Code' -message "$MSG" -sound Glass ;;
    *) [ -n "$DISPLAY" ] && notify-send 'Claude Code' "$MSG" 2>/dev/null || true ;;
esac
