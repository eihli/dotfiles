#!/bin/bash
# Notification hook for Claude Stop event
# Extracts project, title (from /title skill), and first user message (the task)

set -e

INPUT=$(cat)
CWD=$(echo "$INPUT" | jq -r '.cwd // empty')
TRANSCRIPT=$(echo "$INPUT" | jq -r '.transcript_path // empty')

# Project name from cwd
PROJECT=$(basename "$CWD" 2>/dev/null || echo "unknown")

# Title from /title skill (keyed by CWD)
TITLE=""
CWD_KEY=$(echo "$CWD" | tr '/' '_')
if [ -n "$CWD_KEY" ] && [ -f "$HOME/.cache/claude-code/titles/$CWD_KEY" ]; then
    TITLE=$(cat "$HOME/.cache/claude-code/titles/$CWD_KEY" 2>/dev/null || true)
fi

# First user message = the task (efficient: head + first match)
TASK=""
if [ -n "$TRANSCRIPT" ] && [ -f "$TRANSCRIPT" ]; then
    TASK=$(head -100 "$TRANSCRIPT" | jq -r 'select(.type == "user") | .message.content[0].text // empty' 2>/dev/null | head -1 | cut -c1-80)
fi

# Build message: TITLE overrides TASK
MSG="$PROJECT"
if [ -n "$TITLE" ]; then
    MSG="$TITLE | $MSG"
elif [ -n "$TASK" ]; then
    MSG="$MSG: $TASK"
else
    MSG="$MSG: Ready"
fi

# Send notification via FIFO (cross-user) or direct (same-user fallback)
NOTIFY_PIPE="/run/claude-notify/pipe"

# Format: TITLE|BODY for watcher to parse
NOTIFY_TITLE="${TITLE:-Claude Code}"

if [ -p "$NOTIFY_PIPE" ]; then
    echo "${NOTIFY_TITLE}|${MSG}" > "$NOTIFY_PIPE" 2>/dev/null || true
else
    case $(uname) in
        Darwin) terminal-notifier -title 'Claude Code' -message "$MSG" -sound Glass ;;
        *) [ -n "$DISPLAY" ] && notify-send 'Claude Code' "$MSG" 2>/dev/null || true ;;
    esac
fi
