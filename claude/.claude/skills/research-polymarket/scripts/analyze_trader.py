#!/usr/bin/env python3
"""Analyze Polymarket trader strategy from stored data.

Usage:
    python analyze_trader.py 0x1234...           # Full analysis
    python analyze_trader.py 0x1234... --brief   # Summary only
    python analyze_trader.py 0x1234... --json    # JSON output

Reads from: ~/.local/share/research-polymarket/{wallet}.json
Run fetch_trader.py first to collect data.
"""

import argparse
import json
import os
import sys
from collections import defaultdict
from dataclasses import dataclass, field
from datetime import datetime
from pathlib import Path
from statistics import mean, median, stdev


def get_data_dir() -> Path:
    """Get XDG data directory for research-polymarket."""
    xdg_data = os.environ.get("XDG_DATA_HOME", os.path.expanduser("~/.local/share"))
    return Path(xdg_data) / "research-polymarket"


def categorize_market(title: str) -> str:
    """Categorize market by topic.

    Categories: crypto, politics, sports, economics, esports, other.
    Sports detection is aggressive since many markets lack explicit keywords.
    """
    t = title.lower()

    # Crypto
    if any(k in t for k in ["bitcoin", "btc", "eth", "ethereum", "crypto", "token", "solana", "doge"]):
        return "crypto"

    # Politics
    if any(k in t for k in ["trump", "biden", "election", "president", "congress", "senate", "governor", "vote", "republican", "democrat"]):
        return "politics"

    # Economics
    if any(k in t for k in ["fed", "rate", "inflation", "gdp", "economy", "unemployment", "fomc", "cpi", "recession"]):
        return "economics"

    # Esports (before sports, more specific)
    if any(k in t for k in ["counter-strike", "dota", "lol:", "valorant", "csgo", "esports", "league of legends"]):
        return "esports"

    # Sports - comprehensive detection
    sports_patterns = [
        # Match structure
        "vs.", "vs ", " v ",
        # Betting markets
        "o/u", "over", "under", "spread",
        # Tennis
        "set ", "game ", "atp", "wta", "open:", "australian open", "french open", "wimbledon", "us open",
        # Soccer/Football
        " fc", "fc ", "united", "city", "real madrid", "barcelona", "juventus", "bayern",
        "la liga", "premier league", "serie a", "bundesliga", "ligue 1", "champions league",
        "saudi", "al ", "el ",  # Saudi Pro League teams
        # US Sports
        "nfl", "nba", "nhl", "mlb", "ncaa",
        "lakers", "celtics", "warriors", "bulls", "knicks",
        "cowboys", "patriots", "chiefs", "eagles",
        "yankees", "dodgers", "red sox",
        # General
        "winner:", "match", "will .* win on",
    ]
    if any(k in t for k in sports_patterns):
        return "sports"

    return "other"


def load_trader(wallet: str) -> dict:
    """Load trader data from XDG directory."""
    path = get_data_dir() / f"{wallet.lower()}.json"
    if not path.exists():
        print(f"No data found for {wallet}. Run fetch_trader.py first.", file=sys.stderr)
        sys.exit(1)
    with open(path) as f:
        return json.load(f)


@dataclass
class AnalysisResult:
    """Container for all analysis results."""

    wallet: str
    fetched_at: str
    data_summary: dict = field(default_factory=dict)
    side_balance: dict = field(default_factory=dict)
    price_targeting: dict = field(default_factory=dict)
    hold_behavior: dict = field(default_factory=dict)
    market_diversity: dict = field(default_factory=dict)
    execution_patterns: dict = field(default_factory=dict)
    timing_analysis: dict = field(default_factory=dict)
    risk_profile: dict = field(default_factory=dict)
    market_maker_signals: dict = field(default_factory=dict)
    category_performance: dict = field(default_factory=dict)
    top_positions: list = field(default_factory=list)
    market_types: dict = field(default_factory=dict)

    def to_dict(self) -> dict:
        return {
            "wallet": self.wallet,
            "fetched_at": self.fetched_at,
            "data_summary": self.data_summary,
            "side_balance": self.side_balance,
            "price_targeting": self.price_targeting,
            "hold_behavior": self.hold_behavior,
            "market_diversity": self.market_diversity,
            "execution_patterns": self.execution_patterns,
            "timing_analysis": self.timing_analysis,
            "risk_profile": self.risk_profile,
            "market_maker_signals": self.market_maker_signals,
            "category_performance": self.category_performance,
            "top_positions": self.top_positions,
            "market_types": self.market_types,
        }


def analyze_side_balance(positions: list, trades: list) -> dict:
    """Analyze YES/NO balance and same-market hedging."""
    yes_count = 0
    no_count = 0
    yes_value = 0.0
    no_value = 0.0

    # Track positions by market for hedging detection
    market_sides: dict[str, set] = defaultdict(set)

    for pos in positions:
        outcome = pos.get("outcome", "").upper()
        value = float(pos.get("currentValue", 0) or 0)
        market_id = pos.get("conditionId") or pos.get("market", {}).get("conditionId")

        if "YES" in outcome:
            yes_count += 1
            yes_value += value
            if market_id:
                market_sides[market_id].add("YES")
        elif "NO" in outcome:
            no_count += 1
            no_value += value
            if market_id:
                market_sides[market_id].add("NO")

    total_count = yes_count + no_count
    total_value = yes_value + no_value

    # Hedged markets = markets where trader holds both YES and NO
    hedged_markets = [m for m, sides in market_sides.items() if len(sides) > 1]

    return {
        "yes_positions": yes_count,
        "no_positions": no_count,
        "yes_pct": round(100 * yes_count / total_count, 1) if total_count else 0,
        "yes_value": round(yes_value, 2),
        "no_value": round(no_value, 2),
        "yes_value_pct": round(100 * yes_value / total_value, 1) if total_value else 0,
        "hedged_market_count": len(hedged_markets),
        "hedged_pct": round(100 * len(hedged_markets) / len(market_sides), 1) if market_sides else 0,
    }


def analyze_price_targeting(positions: list, trades: list) -> dict:
    """Analyze entry price preferences: underdogs, mid-range, favorites, extremes."""
    entry_prices = []
    buckets = {"underdog_5": 0, "underdog_20": 0, "mid": 0, "favorite_80": 0, "favorite_95": 0}

    for trade in trades:
        if trade.get("side", "").upper() == "BUY":
            price = float(trade.get("price", 0) or 0)
            if 0 < price < 1:
                entry_prices.append(price)
                if price < 0.05:
                    buckets["underdog_5"] += 1
                elif price < 0.20:
                    buckets["underdog_20"] += 1
                elif price > 0.95:
                    buckets["favorite_95"] += 1
                elif price > 0.80:
                    buckets["favorite_80"] += 1
                else:
                    buckets["mid"] += 1

    total = len(entry_prices)
    return {
        "avg_entry_price": round(mean(entry_prices), 3) if entry_prices else None,
        "median_entry_price": round(median(entry_prices), 3) if entry_prices else None,
        "entry_price_stdev": round(stdev(entry_prices), 3) if len(entry_prices) > 1 else None,
        "extreme_underdog_pct": round(100 * buckets["underdog_5"] / total, 1) if total else 0,
        "underdog_pct": round(100 * (buckets["underdog_5"] + buckets["underdog_20"]) / total, 1) if total else 0,
        "mid_range_pct": round(100 * buckets["mid"] / total, 1) if total else 0,
        "favorite_pct": round(100 * (buckets["favorite_80"] + buckets["favorite_95"]) / total, 1) if total else 0,
        "extreme_favorite_pct": round(100 * buckets["favorite_95"] / total, 1) if total else 0,
        "total_buys": total,
    }


def analyze_hold_behavior(positions: list, activity: list) -> dict:
    """Analyze hold-to-resolution vs early exit patterns.

    Note: positions API doesn't reliably include 'redeemed' field.
    We derive redemption stats from activity data instead.

    MERGE = combining YES+NO shares to extract $1 (arbitrage/hedging exit).
    """
    # Count from activity data (more reliable)
    redemptions = [a for a in activity if a.get("type") == "REDEEM"]
    merges = [a for a in activity if a.get("type") == "MERGE"]
    sells = [a for a in activity if a.get("type") == "SELL" or
             (a.get("type") == "TRADE" and a.get("side") == "SELL")]
    buys = [a for a in activity if a.get("type") == "BUY" or
            (a.get("type") == "TRADE" and a.get("side") == "BUY")]

    redeem_count = len(redemptions)
    merge_count = len(merges)
    sell_count = len(sells)
    buy_count = len(buys)

    # Value calculations
    redemption_values = [float(r.get("usdcSize", 0) or 0) for r in redemptions]
    merge_values = [float(m.get("usdcSize", 0) or 0) for m in merges]

    # Estimate hold-to-resolution: redemptions vs total exits (redemptions + sells + merges)
    # Note: merges are like "early exits" since you're not holding to resolution
    total_exits = redeem_count + sell_count + merge_count
    hold_to_resolution_pct = round(100 * redeem_count / total_exits, 1) if total_exits else 0

    return {
        "redemption_count": redeem_count,
        "merge_count": merge_count,
        "sell_count": sell_count,
        "buy_count": buy_count,
        "hold_to_resolution_pct": hold_to_resolution_pct,
        "total_redemption_value": round(sum(redemption_values), 2) if redemption_values else 0,
        "total_merge_value": round(sum(merge_values), 2) if merge_values else 0,
        "active_positions": len(positions),
    }


def analyze_market_diversity(positions: list, trades: list) -> dict:
    """Analyze market type diversity and concentration."""
    market_titles = defaultdict(float)
    categories = defaultdict(int)

    for pos in positions:
        title = pos.get("title", "") or pos.get("market", {}).get("question", "")
        value = float(pos.get("currentValue", 0) or 0)
        market_titles[title] += value
        categories[categorize_market(title)] += 1

    # Concentration: top N markets as % of total
    total_value = sum(market_titles.values())
    sorted_markets = sorted(market_titles.values(), reverse=True)

    top_1_pct = round(100 * sorted_markets[0] / total_value, 1) if sorted_markets and total_value else 0
    top_5_pct = round(100 * sum(sorted_markets[:5]) / total_value, 1) if sorted_markets and total_value else 0
    top_10_pct = round(100 * sum(sorted_markets[:10]) / total_value, 1) if sorted_markets and total_value else 0

    return {
        "unique_markets": len(market_titles),
        "top_1_concentration_pct": top_1_pct,
        "top_5_concentration_pct": top_5_pct,
        "top_10_concentration_pct": top_10_pct,
        "categories": dict(categories),
        "primary_category": max(categories, key=categories.get) if categories else None,
    }


def analyze_execution_patterns(trades: list) -> dict:
    """Analyze scaling, order splitting, and execution sophistication."""
    # Group trades by market + side to detect scaling
    market_trades: dict[str, list] = defaultdict(list)

    for trade in trades:
        market = trade.get("asset") or trade.get("conditionId", "")
        side = trade.get("side", "")
        key = f"{market}_{side}"
        market_trades[key].append(trade)

    # Detect scaling (multiple trades same direction same market)
    scaled_positions = sum(1 for trades_list in market_trades.values() if len(trades_list) > 1)
    single_entry = sum(1 for trades_list in market_trades.values() if len(trades_list) == 1)

    # Analyze trade sizes
    sizes = [float(t.get("size", 0) or 0) for t in trades if t.get("size")]
    usd_values = [float(t.get("usdValue", 0) or t.get("price", 0) * t.get("size", 0) or 0) for t in trades]
    usd_values = [v for v in usd_values if v > 0]

    # Detect order splitting: multiple trades within short time window
    # Sort by timestamp and look for clusters
    timestamped = [(t.get("timestamp"), t) for t in trades if t.get("timestamp")]
    timestamped.sort(key=lambda x: x[0])

    split_clusters = 0
    i = 0
    while i < len(timestamped):
        cluster_count = 1
        while i + cluster_count < len(timestamped):
            time_diff = timestamped[i + cluster_count][0] - timestamped[i][0]
            if time_diff < 60:  # Within 60 seconds
                cluster_count += 1
            else:
                break
        if cluster_count >= 3:
            split_clusters += 1
        i += cluster_count

    # Trade size distribution
    size_buckets = {
        "under_100": sum(1 for v in usd_values if v < 100),
        "100_500": sum(1 for v in usd_values if 100 <= v < 500),
        "500_1000": sum(1 for v in usd_values if 500 <= v < 1000),
        "1000_5000": sum(1 for v in usd_values if 1000 <= v < 5000),
        "over_5000": sum(1 for v in usd_values if v >= 5000),
    }
    total_usd = len(usd_values)
    size_distribution = {
        bucket: {
            "count": count,
            "pct": round(100 * count / total_usd, 1) if total_usd else 0
        }
        for bucket, count in size_buckets.items()
    }

    return {
        "total_trades": len(trades),
        "scaled_positions": scaled_positions,
        "single_entry_positions": single_entry,
        "scaling_ratio": round(scaled_positions / (scaled_positions + single_entry), 2) if (scaled_positions + single_entry) else 0,
        "avg_trade_size": round(mean(sizes), 2) if sizes else None,
        "median_trade_size": round(median(sizes), 2) if sizes else None,
        "avg_trade_usd": round(mean(usd_values), 2) if usd_values else None,
        "order_split_clusters": split_clusters,
        "size_distribution": size_distribution,
    }


def analyze_timing(trades: list, activity: list) -> dict:
    """Analyze time-of-day patterns, event clustering."""
    hour_dist = defaultdict(int)
    day_dist = defaultdict(int)

    for trade in trades:
        ts = trade.get("timestamp")
        if ts:
            dt = datetime.fromtimestamp(ts) if isinstance(ts, (int, float)) else None
            if dt:
                hour_dist[dt.hour] += 1
                day_dist[dt.strftime("%A")] += 1

    # Find peak hours
    total_trades = sum(hour_dist.values())
    peak_hour = max(hour_dist, key=hour_dist.get) if hour_dist else None
    peak_day = max(day_dist, key=day_dist.get) if day_dist else None

    # US hours (9-17 EST ~ 14-22 UTC) vs other
    us_hours = sum(hour_dist.get(h, 0) for h in range(14, 23))
    asia_hours = sum(hour_dist.get(h, 0) for h in list(range(0, 8)) + [23])

    # Detect event clustering (many trades in short windows)
    timestamped = sorted([t.get("timestamp") for t in trades if t.get("timestamp")])
    burst_count = 0
    i = 0
    while i < len(timestamped) - 5:
        window = timestamped[i:i + 6]
        if window[-1] - window[0] < 3600:  # 6+ trades in 1 hour
            burst_count += 1
            i += 6
        else:
            i += 1

    return {
        "peak_hour_utc": peak_hour,
        "peak_day": peak_day,
        "us_hours_pct": round(100 * us_hours / total_trades, 1) if total_trades else 0,
        "asia_hours_pct": round(100 * asia_hours / total_trades, 1) if total_trades else 0,
        "trading_burst_count": burst_count,
        "hour_distribution": dict(hour_dist) if hour_dist else {},
    }


def analyze_risk_profile(positions: list, trades: list, activity: list, portfolio_value: float) -> dict:
    """Analyze PnL using capital flow: (redeems + merges + current) - buys.

    Note: cashPnl is mark-to-market and misleading for hold-to-resolution traders.
    We calculate true PnL from actual capital flows.

    Win rate is derived from REDEEM activities (positions resolved in your favor)
    vs total unique markets traded.
    """
    # Capital IN: sum of all BUY trades
    buys = [a for a in activity if a.get("type") == "TRADE" and a.get("side") == "BUY"]
    capital_in = sum(float(b.get("usdcSize", 0) or 0) for b in buys)

    # Capital OUT: REDEEMs + MERGEs
    redeems = [a for a in activity if a.get("type") == "REDEEM"]
    merges = [a for a in activity if a.get("type") == "MERGE"]
    redeem_value = sum(float(r.get("usdcSize", 0) or 0) for r in redeems)
    merge_value = sum(float(m.get("usdcSize", 0) or 0) for m in merges)

    # Total value: redeems + merges + current portfolio
    capital_out = redeem_value + merge_value + portfolio_value

    # True PnL
    total_pnl = capital_out - capital_in

    # Win rate based on REDEEM activities (more reliable than position.redeemable)
    # A REDEEM means you held a winning position to resolution
    redeemed_conditions = {r.get("conditionId") or r.get("asset") for r in redeems}
    traded_conditions = {b.get("conditionId") or b.get("asset") for b in buys}
    win_count = len(redeemed_conditions)
    total_resolved = len(traded_conditions)  # Approximation: all traded markets

    # For profit/loss stats, use redeem values vs buy values per condition
    condition_buys: dict[str, float] = defaultdict(float)
    condition_redeems: dict[str, float] = defaultdict(float)
    for b in buys:
        cid = b.get("conditionId") or b.get("asset", "")
        condition_buys[cid] += float(b.get("usdcSize", 0) or 0)
    for r in redeems:
        cid = r.get("conditionId") or r.get("asset", "")
        condition_redeems[cid] += float(r.get("usdcSize", 0) or 0)

    # Calculate per-market profits
    winner_profits = []
    for cid in redeemed_conditions:
        profit = condition_redeems.get(cid, 0) - condition_buys.get(cid, 0)
        if profit > 0:
            winner_profits.append(profit)

    total_winner_profit = sum(winner_profits)

    # Top winners
    winner_profits_sorted = sorted(winner_profits, reverse=True)
    top_3_wins = sum(winner_profits_sorted[:3]) if len(winner_profits_sorted) >= 3 else sum(winner_profits_sorted)
    top_3_pct = round(100 * top_3_wins / total_winner_profit, 1) if total_winner_profit else 0

    return {
        "capital_in": round(capital_in, 2),
        "capital_out_redeems": round(redeem_value, 2),
        "capital_out_merges": round(merge_value, 2),
        "current_portfolio": round(portfolio_value, 2),
        "total_pnl": round(total_pnl, 2),
        "markets_traded": len(traded_conditions),
        "markets_redeemed": win_count,
        "win_rate": round(100 * win_count / total_resolved, 1) if total_resolved else 0,
        "avg_redeem_profit": round(mean(winner_profits), 2) if winner_profits else None,
        "largest_win": round(max(winner_profits), 2) if winner_profits else None,
        "top_3_wins_pct": top_3_pct,
        "profit_factor": None,  # Removed - hard to calculate accurately with incomplete data
    }


def analyze_market_maker_signals(positions: list, trades: list) -> dict:
    """Detect market maker behavior: spread capture, inventory management."""
    # Group positions by market
    market_positions: dict[str, list] = defaultdict(list)

    for pos in positions:
        market = pos.get("conditionId") or pos.get("market", {}).get("conditionId", "")
        if market:
            market_positions[market].append(pos)

    # Detect both-sides positions (potential MM)
    both_sides_markets = 0
    spread_captures = []

    for market, pos_list in market_positions.items():
        outcomes = set()
        prices = {"YES": [], "NO": []}

        for p in pos_list:
            outcome = p.get("outcome", "").upper()
            if "YES" in outcome:
                outcomes.add("YES")
                prices["YES"].append(float(p.get("avgPrice", 0) or 0))
            elif "NO" in outcome:
                outcomes.add("NO")
                prices["NO"].append(float(p.get("avgPrice", 0) or 0))

        if len(outcomes) == 2:
            both_sides_markets += 1
            # Calculate effective spread captured
            if prices["YES"] and prices["NO"]:
                yes_avg = mean(prices["YES"])
                no_avg = mean(prices["NO"])
                spread = 1 - yes_avg - no_avg  # Should be positive if capturing spread
                spread_captures.append(spread)

    # Volume vs PnL ratio (high volume, low PnL = likely MM)
    total_volume = sum(float(t.get("size", 0) or 0) * float(t.get("price", 0) or 0) for t in trades)
    total_pnl = sum(float(p.get("cashPnl", 0) or 0) for p in positions)
    volume_pnl_ratio = round(total_volume / abs(total_pnl), 2) if total_pnl else None

    return {
        "both_sides_market_count": both_sides_markets,
        "both_sides_pct": round(100 * both_sides_markets / len(market_positions), 1) if market_positions else 0,
        "avg_spread_captured": round(mean(spread_captures), 4) if spread_captures else None,
        "volume_to_pnl_ratio": volume_pnl_ratio,
        "mm_likelihood": "high" if both_sides_markets > 5 and (volume_pnl_ratio and volume_pnl_ratio > 50) else "low",
    }


def analyze_top_positions(positions: list, n: int = 15) -> list[dict]:
    """Return top N positions by current value."""
    sorted_positions = sorted(
        positions,
        key=lambda p: float(p.get("currentValue", 0) or 0),
        reverse=True
    )

    result = []
    for p in sorted_positions[:n]:
        title = p.get("title", "") or p.get("market", {}).get("question", "")
        result.append({
            "title": title[:60] if len(title) > 60 else title,
            "outcome": p.get("outcome", "?"),
            "value": round(float(p.get("currentValue", 0) or 0), 2),
            "pnl": round(float(p.get("cashPnl", 0) or 0), 2),
            "cur_price": round(float(p.get("curPrice", 0) or 0), 2),
        })
    return result


def analyze_market_types(positions: list) -> dict:
    """Categorize positions by market structure (not topic).

    Market types:
    - over_under: O/U, Over, Under markets
    - spread: Point spread markets
    - match_winner: vs., vs markets (who wins)
    - win_market: "Will X win on Y" style
    - set_game: Tennis set/game markets
    - other: Everything else
    """
    type_counts = defaultdict(int)
    type_value = defaultdict(float)

    for pos in positions:
        title = (pos.get("title", "") or pos.get("market", {}).get("question", "")).lower()
        value = float(pos.get("currentValue", 0) or 0)

        if any(k in title for k in ["o/u", "over", "under"]):
            mtype = "over_under"
        elif "spread" in title:
            mtype = "spread"
        elif any(k in title for k in ["set ", "game "]):
            mtype = "set_game"
        elif any(k in title for k in ["vs.", "vs "]):
            mtype = "match_winner"
        elif "win on" in title or "will" in title and "win" in title:
            mtype = "win_market"
        else:
            mtype = "other"

        type_counts[mtype] += 1
        type_value[mtype] += value

    total = sum(type_counts.values())
    return {
        mtype: {
            "count": count,
            "pct": round(100 * count / total, 1) if total else 0,
            "value": round(type_value[mtype], 2),
        }
        for mtype, count in type_counts.items()
    }


def analyze_category_performance(positions: list, activity: list) -> dict:
    """Analyze performance by market category using capital flow.

    Uses (redeems + merges) - buys instead of mark-to-market cashPnl.
    This gives accurate realized PnL, not unrealized paper gains/losses.
    """
    # Build conditionId -> category map from positions
    condition_to_category: dict[str, str] = {}
    condition_to_title: dict[str, str] = {}
    for pos in positions:
        cid = pos.get("conditionId") or pos.get("asset", "")
        title = pos.get("title", "") or pos.get("market", {}).get("question", "")
        if cid:
            condition_to_category[cid] = categorize_market(title)
            condition_to_title[cid] = title

    # Group activities by category
    category_flows: dict[str, dict] = defaultdict(lambda: {
        "buys": 0.0, "redeems": 0.0, "merges": 0.0,
        "buy_count": 0, "redeem_count": 0, "merge_count": 0,
        "conditions": set()
    })

    for act in activity:
        cid = act.get("conditionId") or act.get("asset", "")
        # Look up category, or try to categorize from title in activity
        cat = condition_to_category.get(cid)
        if not cat:
            title = act.get("title", "")
            cat = categorize_market(title) if title else "other"

        value = float(act.get("usdcSize", 0) or 0)
        act_type = act.get("type", "")

        if act_type == "TRADE" and act.get("side") == "BUY":
            category_flows[cat]["buys"] += value
            category_flows[cat]["buy_count"] += 1
            category_flows[cat]["conditions"].add(cid)
        elif act_type == "REDEEM":
            category_flows[cat]["redeems"] += value
            category_flows[cat]["redeem_count"] += 1
            category_flows[cat]["conditions"].add(cid)
        elif act_type == "MERGE":
            category_flows[cat]["merges"] += value
            category_flows[cat]["merge_count"] += 1
            category_flows[cat]["conditions"].add(cid)

    # Calculate PnL per category
    result = {}
    for cat, flows in category_flows.items():
        capital_out = flows["redeems"] + flows["merges"]
        capital_in = flows["buys"]
        pnl = capital_out - capital_in

        # Win rate = conditions with redeems / conditions with buys
        redeemed_conditions = {cid for cid in flows["conditions"]
                               if any(a.get("conditionId") == cid and a.get("type") == "REDEEM"
                                      for a in activity)}
        traded_conditions = flows["conditions"]
        win_rate = round(100 * len(redeemed_conditions) / len(traded_conditions), 1) if traded_conditions else 0

        result[cat] = {
            "positions": len(flows["conditions"]),
            "capital_in": round(capital_in, 2),
            "capital_out": round(capital_out, 2),
            "pnl": round(pnl, 2),
            "win_rate": win_rate,
            "redeems": flows["redeem_count"],
            "merges": flows["merge_count"],
        }

    return result


def analyze_trader(data: dict) -> AnalysisResult:
    """Run full analysis on trader data."""
    positions = data.get("positions", [])
    trades = data.get("trades", [])
    activity = data.get("activity", [])

    # Extract portfolio value (API returns list or number)
    value_raw = data.get("value")
    if isinstance(value_raw, list) and value_raw:
        portfolio_value = float(value_raw[0].get("value", 0) if isinstance(value_raw[0], dict) else value_raw[0])
    elif isinstance(value_raw, (int, float)):
        portfolio_value = float(value_raw)
    else:
        portfolio_value = 0.0

    # Check for data limits (incomplete data warning)
    data_limits = data.get("data_limits", {})
    hit_any_limit = any(data_limits.values())

    result = AnalysisResult(
        wallet=data["wallet"],
        fetched_at=data.get("fetched_at", "unknown"),
    )

    result.data_summary = {
        "positions": len(positions),
        "trades": len(trades),
        "activity": len(activity),
        "portfolio_value": portfolio_value,
        "data_incomplete": hit_any_limit,
        "limits_hit": [k for k, v in data_limits.items() if v],
    }

    result.side_balance = analyze_side_balance(positions, trades)
    result.price_targeting = analyze_price_targeting(positions, trades)
    result.hold_behavior = analyze_hold_behavior(positions, activity)
    result.market_diversity = analyze_market_diversity(positions, trades)
    result.execution_patterns = analyze_execution_patterns(trades)
    result.timing_analysis = analyze_timing(trades, activity)
    result.risk_profile = analyze_risk_profile(positions, trades, activity, portfolio_value)
    result.market_maker_signals = analyze_market_maker_signals(positions, trades)
    result.category_performance = analyze_category_performance(positions, activity)
    result.top_positions = analyze_top_positions(positions)
    result.market_types = analyze_market_types(positions)

    return result


def format_brief(result: AnalysisResult) -> str:
    """Format brief summary."""
    data_incomplete = result.data_summary.get("data_incomplete", False)
    warning = ""
    if data_incomplete:
        limits = result.data_summary.get("limits_hit", [])
        warning = f"\n!! DATA INCOMPLETE ({', '.join(limits)}) - PnL HIDDEN (check profile page) !!\n"

    lines = [
        f"Trader: {result.wallet[:10]}...",
        f"Data: {result.data_summary['positions']} positions, {result.data_summary['trades']} trades",
        warning,
        "Strategy Profile:",
        f"  Side bias: {result.side_balance['yes_pct']}% YES",
        f"  Price targeting: {result.price_targeting['underdog_pct']}% underdog, {result.price_targeting['favorite_pct']}% favorite",
        f"  Hold to resolution: {result.hold_behavior['hold_to_resolution_pct']}%",
        f"  Scaling ratio: {result.execution_patterns['scaling_ratio']}",
        f"  MM signals: {result.market_maker_signals['mm_likelihood']}",
        "",
        f"  Primary category: {result.market_diversity['primary_category']}",
    ]

    if not data_incomplete:
        lines.extend([
            "",
            "Performance:",
            f"  Win rate: {result.risk_profile['win_rate']}% ({result.risk_profile['markets_redeemed']} of {result.risk_profile['markets_traded']} markets)",
            f"  Total PnL: ${result.risk_profile['total_pnl']:,.2f}",
            f"  Top 3 wins = {result.risk_profile['top_3_wins_pct']}% of profits",
        ])

    return "\n".join(lines)


def format_full(result: AnalysisResult) -> str:
    """Format full analysis report."""
    # Data completeness warning
    data_warning = ""
    data_incomplete = result.data_summary.get("data_incomplete", False)
    if data_incomplete:
        limits = result.data_summary.get("limits_hit", [])
        data_warning = (
            f"\n{'!'*60}\n"
            f"!! DATA INCOMPLETE - API LIMITS HIT ({', '.join(limits)})\n"
            f"!! PnL/win-rate HIDDEN - values would be WRONG\n"
            f"!! For actual PnL: check polymarket.com/profile/{{wallet}}\n"
            f"{'!'*60}\n"
        )

    portfolio_value = result.data_summary.get('portfolio_value', 0)
    sections = [
        f"{'='*60}",
        f"POLYMARKET TRADER ANALYSIS",
        f"{'='*60}",
        f"Wallet: {result.wallet}",
        f"Fetched: {result.fetched_at}",
        f"Data: {result.data_summary['positions']} positions, {result.data_summary['trades']} trades, {result.data_summary['activity']} activities",
        f"Portfolio value: ${portfolio_value:,.2f}" if portfolio_value else "",
        data_warning,
        f"{'-'*40}",
        "SIDE BALANCE",
        f"{'-'*40}",
        f"  YES positions: {result.side_balance['yes_positions']} ({result.side_balance['yes_pct']}%)",
        f"  NO positions: {result.side_balance['no_positions']}",
        f"  YES value: ${result.side_balance['yes_value']:,.2f} ({result.side_balance['yes_value_pct']}%)",
        f"  Hedged markets: {result.side_balance['hedged_market_count']} ({result.side_balance['hedged_pct']}%)",
        "",
        f"{'-'*40}",
        "PRICE TARGETING",
        f"{'-'*40}",
        f"  Avg entry price: {result.price_targeting['avg_entry_price']}",
        f"  Median entry: {result.price_targeting['median_entry_price']}",
        f"  Extreme underdog (<5%): {result.price_targeting['extreme_underdog_pct']}%",
        f"  Underdog (<20%): {result.price_targeting['underdog_pct']}%",
        f"  Mid-range (20-80%): {result.price_targeting['mid_range_pct']}%",
        f"  Favorite (>80%): {result.price_targeting['favorite_pct']}%",
        f"  Extreme favorite (>95%): {result.price_targeting['extreme_favorite_pct']}%",
        "",
        f"{'-'*40}",
        "HOLD BEHAVIOR",
        f"{'-'*40}",
        f"  Hold to resolution: {result.hold_behavior['hold_to_resolution_pct']}%",
        f"  Redemptions: {result.hold_behavior['redemption_count']} (${result.hold_behavior['total_redemption_value']:,.2f})",
        f"  Merges: {result.hold_behavior['merge_count']} (${result.hold_behavior['total_merge_value']:,.2f})",
        f"  Sells: {result.hold_behavior['sell_count']}",
        f"  Buys: {result.hold_behavior['buy_count']}",
        f"  Active positions: {result.hold_behavior['active_positions']}",
        "",
        f"{'-'*40}",
        "MARKET DIVERSITY",
        f"{'-'*40}",
        f"  Unique markets: {result.market_diversity['unique_markets']}",
        f"  Top 1 concentration: {result.market_diversity['top_1_concentration_pct']}%",
        f"  Top 5 concentration: {result.market_diversity['top_5_concentration_pct']}%",
        f"  Primary category: {result.market_diversity['primary_category']}",
        f"  Categories: {result.market_diversity['categories']}",
        "",
        f"{'-'*40}",
        "EXECUTION PATTERNS",
        f"{'-'*40}",
        f"  Total trades: {result.execution_patterns['total_trades']}",
        f"  Scaling ratio: {result.execution_patterns['scaling_ratio']} (0=all-in, 1=always scale)",
        f"  Avg trade size: {result.execution_patterns['avg_trade_size']}",
        f"  Avg trade USD: ${result.execution_patterns['avg_trade_usd']:,.2f}" if result.execution_patterns['avg_trade_usd'] else "",
        f"  Order split clusters: {result.execution_patterns['order_split_clusters']}",
        "",
        f"{'-'*40}",
        "TIMING ANALYSIS",
        f"{'-'*40}",
        f"  Peak hour (UTC): {result.timing_analysis['peak_hour_utc']}",
        f"  Peak day: {result.timing_analysis['peak_day']}",
        f"  US hours: {result.timing_analysis['us_hours_pct']}%",
        f"  Asia hours: {result.timing_analysis['asia_hours_pct']}%",
        f"  Trading bursts: {result.timing_analysis['trading_burst_count']}",
        "",
        f"{'-'*40}",
        "RISK PROFILE (Capital Flow)" + (" [HIDDEN - DATA INCOMPLETE]" if data_incomplete else ""),
        f"{'-'*40}",
    ]

    if data_incomplete:
        sections.append("  (Skipped - would show wrong values. Check profile page for actual PnL.)")
    else:
        sections.extend([
            f"  Capital deployed: ${result.risk_profile['capital_in']:,.2f}",
            f"  Redeemed: ${result.risk_profile['capital_out_redeems']:,.2f}",
            f"  Merged: ${result.risk_profile['capital_out_merges']:,.2f}",
            f"  Current portfolio: ${result.risk_profile['current_portfolio']:,.2f}",
            f"  Total PnL: ${result.risk_profile['total_pnl']:,.2f}",
            "",
            f"  Markets traded: {result.risk_profile['markets_traded']}",
            f"  Markets redeemed (wins): {result.risk_profile['markets_redeemed']}",
            f"  Win rate: {result.risk_profile['win_rate']}%",
            f"  Avg redeem profit: ${result.risk_profile['avg_redeem_profit']:,.2f}" if result.risk_profile.get('avg_redeem_profit') else "",
            f"  Largest win: ${result.risk_profile['largest_win']:,.2f}" if result.risk_profile.get('largest_win') else "",
            f"  Top 3 wins = {result.risk_profile['top_3_wins_pct']}% of profits",
        ])

    sections.extend([
        "",
        f"{'-'*40}",
        "MARKET MAKER SIGNALS",
        f"{'-'*40}",
        f"  Both-sides markets: {result.market_maker_signals['both_sides_market_count']} ({result.market_maker_signals['both_sides_pct']}%)",
        f"  Avg spread captured: {result.market_maker_signals['avg_spread_captured']}",
        f"  Volume/PnL ratio: {result.market_maker_signals['volume_to_pnl_ratio']}",
        f"  MM likelihood: {result.market_maker_signals['mm_likelihood']}",
        "",
        f"{'-'*40}",
        "CATEGORY PERFORMANCE (Capital Flow)" + (" [HIDDEN - DATA INCOMPLETE]" if data_incomplete else ""),
        f"{'-'*40}",
    ])

    if data_incomplete:
        # Show category distribution only (no PnL)
        for cat, stats in result.category_performance.items():
            sections.append(f"  {cat}: {stats['positions']} markets (PnL hidden)")
    else:
        for cat, stats in result.category_performance.items():
            sections.append(f"  {cat}: {stats['positions']} markets, ${stats['pnl']:,.2f} PnL (in: ${stats['capital_in']:,.0f}, out: ${stats['capital_out']:,.0f})")

    # Add trade size distribution
    sections.extend([
        "",
        f"{'-'*40}",
        "TRADE SIZE DISTRIBUTION",
        f"{'-'*40}",
    ])
    size_dist = result.execution_patterns.get('size_distribution', {})
    bucket_labels = {
        "under_100": "<$100",
        "100_500": "$100-500",
        "500_1000": "$500-1k",
        "1000_5000": "$1k-5k",
        "over_5000": ">$5k",
    }
    for bucket, data in size_dist.items():
        label = bucket_labels.get(bucket, bucket)
        sections.append(f"  {label}: {data['count']} ({data['pct']}%)")

    # Add market types
    sections.extend([
        "",
        f"{'-'*40}",
        "MARKET TYPE BREAKDOWN",
        f"{'-'*40}",
    ])
    for mtype, data in result.market_types.items():
        sections.append(f"  {mtype}: {data['count']} ({data['pct']}%) - ${data['value']:,.0f}")

    # Add top positions
    sections.extend([
        "",
        f"{'-'*40}",
        "TOP POSITIONS BY VALUE",
        f"{'-'*40}",
    ])
    for pos in result.top_positions[:10]:
        price_pct = f"{pos['cur_price']*100:.0f}%" if pos['cur_price'] else "?"
        sections.append(f"  ${pos['value']:,.0f} | {pos['outcome']} @ {price_pct} | PnL: ${pos['pnl']:,.0f} | {pos['title'][:45]}")

    return "\n".join(sections)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Analyze Polymarket trader strategy")
    parser.add_argument("wallet", help="Wallet address (0x...)")
    parser.add_argument("--brief", action="store_true", help="Show brief summary only")
    parser.add_argument("--json", action="store_true", help="Output as JSON")
    args = parser.parse_args()

    data = load_trader(args.wallet)
    result = analyze_trader(data)

    if args.json:
        print(json.dumps(result.to_dict(), indent=2))
    elif args.brief:
        print(format_brief(result))
    else:
        print(format_full(result))
