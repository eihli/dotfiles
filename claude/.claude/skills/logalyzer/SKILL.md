---
name: logalyzer
description: |
  Token-efficient analysis of large log files. Produces structured summaries for debugging.
  Use when: analyzing logs, debugging from logs, investigating errors/failures/performance issues,
  understanding what happened in a system, or when user provides log files to examine.
  Triggers: "analyze these logs", "what went wrong", "debug this", "look at the logs",
  file types: .log, server logs, application output, stderr captures.
  Accepts shortcuts: xdg, journalctl, docker, project, system (or no arg to auto-discover).
---

# Logalyzer

Analyze large logs without exhausting context. Extract signal, summarize findings, enable next-step debugging.

## Quick Start

Run the analyzer script for a complete summary:

```bash
${CLAUDE_SKILL_DIR}/scripts/analyze.sh /path/to/logfile.log
```

### Shortcut Arguments

When invoked with a keyword instead of a path, use these discovery strategies:

| Argument | Action |
|----------|--------|
| `/path/to/file.log` | Analyze specific log file directly |
| `/path/a.log /path/b.log` | Compare two logs (before/after, good/bad) |
| `xdg` | Search `~/.local/state/` for recent logs |
| `journalctl` | Query systemd journal for recent errors |
| `docker` | Run `docker compose logs` or `docker logs` |
| `project` | Find `.log` files in current working directory |
| `system` | Check `/var/log/` for recent logs |
| (no arg) | Search codebase for log paths, then try XDG |

Examples:
- `/logalyzer /var/log/app.log` → analyze specific file
- `/logalyzer good.log bad.log` → diff analysis between two logs
- `/logalyzer journalctl` → `journalctl --priority=err --since "1 hour ago"`

### Options
- `--errors-only` - skip warnings, focus on errors
- `--max-samples N` - number of sample excerpts (default: 3)
- `--context N` - lines of context around samples (default: 5)

The script outputs: metadata, severity counts, normalized error patterns, temporal analysis, stack traces, samples, and common issue detection.

## Log Discovery

When no specific log file is provided, use these strategies to find relevant logs.

### Search Codebase for Log Paths

```bash
# Logging configuration patterns
rg -i "log_file|logfile|log_path|LOG_DIR" --type py --type js --type toml --type yaml
rg -i "FileHandler|RotatingFileHandler|logging\.config" --type py
rg -i "winston|pino|bunyan" --type js  # Node logging libs

# Hardcoded paths
rg -o '"/[^"]*\.log"' .
rg -o "'/[^']*\.log'" .
```

### Standard Log Locations

```bash
# XDG state directory (preferred for user apps)
ls -lt ~/.local/state/*/  2>/dev/null | head -20
ls -lt ~/.local/state/**/*.log 2>/dev/null | head -10

# Legacy home locations
ls -lt ~/.*/*.log 2>/dev/null | head -10

# System logs (may need sudo)
ls -lt /var/log/*.log 2>/dev/null | head -10
ls -lt /var/log/*/*.log 2>/dev/null | head -10

# Systemd journal (for services)
journalctl -u SERVICE_NAME --since "1 hour ago" --no-pager
```

### Find Most Recent Logs

```bash
# Most recently modified .log files in project
find . -name "*.log" -type f -mtime -1 -exec ls -lt {} + 2>/dev/null | head -10

# Most recently modified anywhere in state dir
find ~/.local/state -name "*.log" -type f -mtime -1 -exec ls -lt {} + 2>/dev/null | head -5

# By size (large logs often have recent activity)
find . -name "*.log" -type f -size +1M -exec ls -lhS {} + 2>/dev/null | head -5
```

### Common App-Specific Locations

```bash
# Docker/containers
docker logs CONTAINER_NAME --tail 100
docker compose logs --tail 100

# Python/Node debug output
ls -lt /tmp/*.log 2>/dev/null | head -5

# IDE/editor logs
ls -lt ~/.config/Code/logs/ 2>/dev/null | head -5
```

## Manual Commands

Use these for targeted follow-up or when the script output needs refinement.

### Metadata

```bash
wc -l "$LOG" && ls -lh "$LOG"
head -1 "$LOG" && tail -1 "$LOG"
```

### Severity Counts

```bash
rg -c -i "error|exception|fail" "$LOG"
rg -c -i "warn" "$LOG"
```

### Unique Error Patterns (normalized)

```bash
rg -i "error|exception|fail" "$LOG" | \
  sed -E 's/[0-9]{4}-[0-9]{2}-[0-9]{2}[T ][0-9:.Z-]*//g' | \
  sed -E 's/[0-9a-f]{8}-[0-9a-f-]{27,}/UUID/gi' | \
  sort | uniq -c | sort -rn | head -20
```

### Targeted Sampling

```bash
rg -C5 -m3 "PATTERN" "$LOG"     # 5 lines context, max 3 matches
rg -B10 "fatal|panic" "$LOG"    # 10 lines before fatal errors
```

### Request/Trace ID Following

```bash
rg "request_id.*error" "$LOG" | head -1
rg "REQUEST_ID_HERE" "$LOG"
```

### Diff Analysis (Two Files)

```bash
diff <(rg -o 'pattern' good.log | sort -u) \
     <(rg -o 'pattern' bad.log | sort -u)
```

## Token Efficiency Rules

1. **Run script first** - get structured overview before diving deeper
2. **Counts before content** - know frequency before reading examples
3. **Limit context** - use `-m` (max count) and `-C` (context lines)
4. **Deduplicate aggressively** - unique patterns with counts

## Output Format

After analysis, produce a summary for the user:

```markdown
## Log Analysis: [filename]

### Metadata
Lines: X | Size: Y | Time range: START to END

### Error Summary
| Type | Count |
|------|-------|

### Key Patterns
1. [Pattern + count]

### Hypotheses
1. [Likely root cause based on patterns]

### Suggested Investigation
- Compare [function/file] to [error pattern]
```

## Common Issue Patterns

```bash
rg -i "out.of.memory|oom|heap" "$LOG"           # OOM
rg -i "timeout|timed.out|deadline" "$LOG"       # Timeouts
rg -i "connection.refused|econnreset" "$LOG"    # Connection
rg -i "unauthorized|forbidden|401|403" "$LOG"   # Auth
```
