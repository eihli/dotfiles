# Polymarket API Reference

## Base URLs

| API | URL |
|-----|-----|
| Data API | `https://data-api.polymarket.com` |
| Gamma API | `https://gamma-api.polymarket.com` |
| CLOB API | `https://clob.polymarket.com` |

## Data API Endpoints

### GET /positions

User holdings with PnL metrics.

**Parameters:**
- `user` (required): Wallet address
- `market`: Condition ID(s), comma-separated
- `sizeThreshold`: Min position size (default: 1.0)
- `limit`: Max 500 (default: 100)
- `sortBy`: TOKENS, CURRENT, INITIAL, CASHPNL, PERCENTPNL, TITLE, PRICE

**Response fields:** size, avgPrice, currentValue, initialValue, cashPnl, percentPnl, title, outcome, redeemed

### GET /trades

Transaction history.

**Parameters:**
- `user`: Wallet address
- `market`: Condition ID(s)
- `limit`: Max 500
- `takerOnly`: Boolean (default: true)
- `side`: BUY or SELL

**Response fields:** side, asset, size, price, timestamp, txHash, userProfile

### GET /activity

Onchain operations: trades, splits, merges, redeems, rewards.

**Parameters:**
- `user` (required): Wallet address
- `type`: Activity type filter (comma-separated)
- `start`/`end`: Timestamp filters
- `sortBy`: TIMESTAMP, TOKENS, CASH

**Response fields:** type, size, usdValue, txHash, market details

### GET /holders

Top holders for a market.

**Parameters:**
- `market` (required): Condition ID
- `limit`: Max holders

**Response fields:** wallet, amount, userProfile, outcomeIndex

### GET /value

Total portfolio value.

**Parameters:**
- `user` (required): Wallet address
- `market`: Optional condition ID(s)

**Response:** USD value number

## Gamma API Endpoints

### GET /markets

List markets with metadata.

**Parameters:**
- `closed`: Boolean
- `active`: Boolean
- `limit`, `offset`: Pagination

### GET /markets/{id}

Single market details including resolution info.

## Leaderboard

### GET /v1/leaderboard

Trader rankings.

**Parameters:**
- `window`: Time period
- `limit`: Max results

## Third-Party Sources

- **polymarketanalytics.com/traders**: Leaderboard with clickable profiles, 5-min refresh
- **Profile URL pattern**: `polymarket.com/profile/{wallet_address}`
