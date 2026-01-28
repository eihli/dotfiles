---
name: research-polymarket
description: "Scripts and references for Polymarket analysis. NOTE: For full trader analysis, use the polymarket-analyst AGENT instead (via Task tool). This skill provides direct script access for: (1) manual data fetching, (2) running individual analysis steps, (3) accessing API endpoint references."
---

# Polymarket Analyst

Research and analyze Polymarket trading activity to understand strategies, discover opportunities, and produce narrative reports.

## CRITICAL: API Limitations

**DO NOT use this tool to calculate PnL.** The Polymarket APIs that return individual trades/activities have hard pagination limits (~100k records). For active traders, this means:

- We see only recent activity, not full history
- Capital flow calculations are **wildly wrong** (can show -$800k for a +$3.6M trader)
- Even "win rate" is unreliable - we're missing resolved markets

**For PnL**: Use the profile page directly (polymarket.com/profile/{wallet}) - it shows accurate lifetime PnL.

**This tool is useful for**: understanding *how* someone trades (patterns, timing, price targeting, market types) - NOT *how much* they've made.

## Quick Start

```bash
# Fetch all trader data (paginated, saved to XDG)
python scripts/fetch_trader.py 0x1234...

# Analyze strategy
python scripts/analyze_trader.py 0x1234...
```

Data stored at: `~/.local/share/research-polymarket/{wallet}.json`

## Scripts

### fetch_trader.py

Fetches complete trader data with full pagination. Saves to XDG data directory.

```bash
python scripts/fetch_trader.py 0x1234...           # Fetch all data, save to XDG
python scripts/fetch_trader.py 0x1234... --refresh # Force full refresh
python scripts/fetch_trader.py 0x1234... --stdout  # Print to stdout instead
python scripts/fetch_trader.py 0x1234... --no-proxy # Skip SOCKS5 proxy
python scripts/fetch_trader.py <condition_id> --holders  # Fetch market holders
```

Requires: `pip install httpx[socks]`

### analyze_trader.py

Comprehensive strategy analysis from stored data.

```bash
python scripts/analyze_trader.py 0x1234...         # Full analysis
python scripts/analyze_trader.py 0x1234... --brief # Summary only
python scripts/analyze_trader.py 0x1234... --json  # JSON output
```

**Analysis dimensions:**

| Category | Metrics |
|----------|---------|
| Side Balance | YES/NO ratio, hedged market count, value distribution |
| Price Targeting | Underdog/favorite %, extreme positions (<5%, >95%), avg entry |
| Hold Behavior | Hold-to-resolution %, redemptions, merges, sells |
| Market Diversity | Unique markets, concentration (top 1/5/10), category breakdown |
| Execution Patterns | Scaling ratio, order splitting, **trade size distribution** |
| Timing Analysis | Peak hours, US/Asia %, trading bursts, day-of-week |
| Risk Profile | Capital flow PnL, win rate (markets redeemed), top winners |
| Market Maker Signals | Both-sides %, spread captured, volume/PnL ratio |
| Category Performance | **Capital flow PnL** by category (crypto, politics, sports, esports, economics) |
| **Market Types** | Structural breakdown: O/U, spreads, match winners, win markets |
| **Top Positions** | Largest positions by current value |

**PnL Calculation**: Uses capital flow method: `(redeems + merges + current) - buys`. This is more accurate than mark-to-market `cashPnl` for hold-to-resolution traders.

**Win Rate**: Derived from REDEEM activities (markets resolved in your favor) vs total unique markets traded.

**Data Limits**: API pagination is capped at ~100k records. For very active traders, older activity may be missing.

### Interpreting Output

**When API limits are hit** (indicated by "HIT LIMIT" in fetch output):

| Still useful | IGNORE completely |
|--------------|-------------------|
| Price targeting % | Total PnL (WRONG) |
| Trade size buckets | Capital deployed (WRONG) |
| Timing patterns (hours, days) | Win rate (WRONG) |
| Category breakdown % | Category PnL (WRONG) |
| Scaling ratio | Markets traded count |
| Market maker signals | Any dollar amounts |

**The analyze script will hide PnL sections when data is incomplete.** If you need PnL, scrape it from the profile page.

## Workflow

### 1. Identify Target

Extract wallet address from URL or search for traders matching criteria.

- Profile URL: `polymarket.com/profile/{wallet}`
- Leaderboard: polymarketanalytics.com/traders

### 2. Fetch Data

```bash
python scripts/fetch_trader.py 0x1234...
```

Incrementally updates existing data. Use `--refresh` for full re-fetch.

### 3. Run Analysis

```bash
python scripts/analyze_trader.py 0x1234...
```

### 4. Interpret Results

Key questions to answer:

- **How do they pick markets?** (category performance, market diversity)
- **What prices do they target?** (price targeting: underdog vs favorite)
- **Do they hedge?** (side balance: hedged market count)
- **Do they hold or trade?** (hold behavior, execution patterns)
- **Are they a market maker?** (MM signals, both-sides %)
- **Is success skill or luck?** (top-3 wins concentration, profit factor)

### 5. Cross-Reference Context

Web search for:
- News/events coinciding with large trades
- Market resolution outcomes
- Trader's public presence (if any)

### 6. Edge Analysis (Recommended Follow-up)

After initial pattern identification, dig deeper: **"How are they finding their edge? Does this match known advantage gambling concepts?"**

Connect observed patterns to explanatory frameworks:

| Pattern | Likely Edge |
|---------|-------------|
| Low hold-to-resolution + active exits | Futures scalping / CLV capture |
| Portfolio of long-shots | Reverse longshot bias exploitation |
| Single category dominance | Domain expertise |
| Both-sides + tight spreads | Market making |

See [edge-analysis.md](references/edge-analysis.md) for full pattern→concept mapping and literature references.

**Output**: Produce an edge hypothesis explaining *why* the strategy works, not just *what* it does.

## Strategy Archetypes

| Archetype | Signals |
|-----------|---------|
| **Arbitrageur** | Balanced YES/NO, tight spreads, high volume |
| **Conviction Trader** | Few large positions, holds to resolution, low scaling |
| **News Trader** | Trades cluster around events, quick exits, high burst count |
| **Long-shot Hunter** | High underdog %, many small positions |
| **Market Maker** | High both-sides %, spread capture, high volume/PnL ratio |
| **Whale** | High concentration, large avg trade size |

## API Reference

See [api-endpoints.md](references/api-endpoints.md) for direct API access.

## Example Queries

- "Analyze trader 0x1234..."
- "What strategy does this whale use? [profile URL]"
- "Find successful traders in crypto markets"
- "Compare these two traders' approaches"
