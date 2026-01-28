# Edge Analysis Framework

Connect observed trading patterns to known advantage gambling concepts. This transforms descriptive analysis ("they buy long-shots") into explanatory analysis ("they're exploiting reverse longshot bias via futures scalping").

## Key Question

**"How are they finding their edge? Does this pattern match anything in advantage gambling literature?"**

## Pattern → Concept Mapping

| Observed Pattern | Likely Edge | Literature |
|-----------------|-------------|------------|
| Low hold-to-resolution + active trading | Futures scalping / CLV capture | Closing Line Value theory |
| Portfolio of mutually exclusive long-shots | Dutch book / reverse longshot bias | Snowberg & Wolfers (2010) |
| Both-sides trading + tight spreads | Market making | Bid-ask spread capture |
| Trades cluster around news events | Information edge / news trading | Event-driven strategies |
| High favorite %, holds to resolution | Favorite-longshot bias exploitation | Thaler & Ziemba (1988) |
| Single category dominance + high win rate | Domain expertise | Specialization advantage |
| Balanced YES/NO across correlated markets | Hedging / arbitrage | Risk-free profit extraction |

## Core Concepts

### Closing Line Value (CLV)

Gold standard for measuring betting skill. If entry prices consistently beat closing prices, edge exists regardless of outcome variance.

**Signals**: Low hold-to-resolution, sells before resolution, prices move in trader's direction after entry.

### Reverse Longshot Bias

Traditional markets: bettors overbet long-shots (longshot bias). Prediction markets often show the opposite—long-shots underpriced because:
- Retail prefers favorites
- Capital efficiency punishes long-shots (5¢ tied up for months)
- Market makers avoid illiquid tails

**Signals**: High underdog %, portfolio approach across multiple long-shots, active exits when prices appreciate.

### Futures Scalping

Buy outcome early at stale odds. As information arrives (season progresses, news breaks), prices adjust. Sell the price movement, not the final outcome.

**Signals**: Low hold-to-resolution (10-30%), large positions in futures markets, exits when prices move favorably.

### Dutch Book / Portfolio Construction

Buy YES on multiple mutually exclusive outcomes. If prices sum to <100¢, structural edge exists. Even without arbitrage, diversification across correlated outcomes smooths variance.

**Signals**: Positions in multiple outcomes of same event (all NBA championship contenders), hedged market count > 0.

### Market Making

Provide liquidity on both sides, capture bid-ask spread. Requires high volume, balanced positions, quick turnover.

**Signals**: High both-sides %, spread captured > 0, high volume/PnL ratio, MM likelihood = high.

## Analysis Template

After initial strategy breakdown, produce edge analysis:

```
## Edge Hypothesis

**Primary edge**: [CLV capture / reverse longshot bias / domain expertise / etc.]

**Supporting evidence**:
- [Pattern 1] → suggests [concept]
- [Pattern 2] → suggests [concept]

**Mechanism**: [How they extract value - be specific]

**Required skills**:
- [Skill 1]
- [Skill 2]

**Literature connection**: [Relevant papers/concepts]
```

## Key References

- Thaler & Ziemba (1988) - "Anomalies: Parimutuel Betting Markets"
- Snowberg & Wolfers (2010) - "Explaining the Favorite-Long Shot Bias"
- Levitt (2004) - "Why Are Gambling Markets Organised So Differently from Financial Markets?"
- Ottaviani & Sorensen (2008) - "The Favorite-Longshot Bias: An Overview"
