# Claude Code Skill/Subagent Ideas

## Context
Pain points: Claude jumps to code too soon, introduces bugs, over-complex solutions, stale API knowledge, context pollution when things go wrong. Domain: Polymarket trading (backtesting, market making, arbitrage), Python-heavy, lots of websockets/APIs.

---

## 1. `/quant-design` - Domain-aware planning subagent

Before any trading system work:
- Web searches current best practices (regime detection, bias prevention, etc.)
- Asks structured questions about goals/constraints
- Explicitly surfaces risks you might not have named (look-ahead bias, survivorship bias, overfitting, data leakage)
- Proposes data structures and architecture *before* code
- Checkpoints understanding in a lightweight spec file

**Why:** Exploratory domain where you don't know what you don't know. Need Claude as knowledgeable collaborator, not just code executor.

---

## 2. `/api-bootstrap <service>` - Fresh API integration

- Fetches current docs via WebFetch
- Writes minimal "hello world" integration test to verify connectivity
- Builds thin wrapper only after verification passes
- Documents any gotchas discovered

**Why:** Training data has stale API versions. Verify reality before building on assumptions.

---

## 3. `/checkpoint` - Context hygiene

When things are working well:
- Summarizes current state to a file
- Can be used to "reset" a new session with clean context
- Prevents the pollution spiral when debugging goes sideways

**Why:** Once context gets polluted with failed attempts, it compounds. Checkpoint good state.

---

## 4. Project-local context files

Instead of generic skills, each project gets a `CLAUDE-<project>.md` with:
- Logging locations/formats
- Test patterns
- Key domain concepts
- Common pitfalls for that specific codebase

**Why:** Generic skills (like logalyzer) struggle with project-specific details. Front-load the context.

---

## Next steps
Pick one, make it solid, iterate.
