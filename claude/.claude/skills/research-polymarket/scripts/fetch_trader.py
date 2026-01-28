#!/usr/bin/env python3
"""Fetch and store Polymarket trader data with full pagination.

Usage:
    python fetch_trader.py 0x1234...           # Fetch all data, save to XDG
    python fetch_trader.py 0x1234... --refresh # Force full refresh
    python fetch_trader.py 0x1234... --stdout  # Print to stdout instead of saving

Data stored at: ~/.local/share/research-polymarket/{wallet}.json
"""

import argparse
import json
import os
import sys
import time
from datetime import datetime, timezone
from pathlib import Path
from typing import Any

try:
    import httpx
except ImportError:
    print("Error: httpx not installed. Run: pip install httpx[socks]", file=sys.stderr)
    sys.exit(1)

DATA_API = "https://data-api.polymarket.com"
DEFAULT_PROXY = "socks5://localhost:1080"
PAGE_SIZE = 500  # Max allowed by API


def get_data_dir() -> Path:
    """Get XDG data directory for research-polymarket."""
    xdg_data = os.environ.get("XDG_DATA_HOME", os.path.expanduser("~/.local/share"))
    data_dir = Path(xdg_data) / "research-polymarket"
    data_dir.mkdir(parents=True, exist_ok=True)
    return data_dir


def get_trader_path(wallet: str) -> Path:
    """Get path for trader data file."""
    return get_data_dir() / f"{wallet.lower()}.json"


def load_existing(wallet: str) -> dict | None:
    """Load existing trader data if available."""
    path = get_trader_path(wallet)
    if path.exists():
        with open(path) as f:
            return json.load(f)
    return None


def save_trader(data: dict) -> Path:
    """Save trader data to XDG directory."""
    path = get_trader_path(data["wallet"])
    with open(path, "w") as f:
        json.dump(data, f, indent=2)
    return path


def _make_client(use_proxy: bool) -> httpx.Client:
    """Create httpx client, handling version differences."""
    if not use_proxy:
        return httpx.Client(timeout=30.0)
    try:
        return httpx.Client(proxy=DEFAULT_PROXY, timeout=30.0)
    except TypeError:
        return httpx.Client(proxies={"all://": DEFAULT_PROXY}, timeout=30.0)


def _paginate(
    client: httpx.Client,
    endpoint: str,
    params: dict,
    page_size: int = PAGE_SIZE,
    max_pages: int = 200,
) -> tuple[list[dict], bool]:
    """Fetch all pages from an endpoint using offset pagination.

    Returns:
        Tuple of (results, hit_limit) where hit_limit is True if we
        stopped due to max_pages rather than exhausting data.
    """
    all_results = []
    offset = 0
    hit_limit = False

    for page_num in range(max_pages):
        page_params = {**params, "limit": page_size, "offset": offset}
        resp = client.get(f"{DATA_API}{endpoint}", params=page_params)

        if not resp.is_success:
            print(f"Warning: {endpoint} returned {resp.status_code}", file=sys.stderr)
            break

        page = resp.json()
        if not page:
            break

        all_results.extend(page)
        if len(page) < page_size:
            break

        offset += page_size
        time.sleep(0.1)  # Rate limiting

        if page_num == max_pages - 1:
            hit_limit = True

    return all_results, hit_limit


def _paginate_by_timestamp(
    client: httpx.Client,
    endpoint: str,
    params: dict,
    page_size: int = PAGE_SIZE,
    max_pages: int = 200,
    since_ts: int | None = None,
) -> tuple[list[dict], bool]:
    """Fetch all pages using timestamp-based pagination (for activity endpoint).

    Returns:
        Tuple of (results, hit_limit) where hit_limit is True if we
        stopped due to max_pages rather than exhausting data.
    """
    all_results = []
    end_ts = None
    hit_limit = False

    base_params = {**params, "limit": page_size}
    if since_ts:
        base_params["start"] = since_ts

    for page_num in range(max_pages):
        page_params = {**base_params}
        if end_ts:
            page_params["end"] = end_ts

        resp = client.get(f"{DATA_API}{endpoint}", params=page_params)

        if not resp.is_success:
            print(f"Warning: {endpoint} returned {resp.status_code}", file=sys.stderr)
            break

        page = resp.json()
        if not page:
            break

        all_results.extend(page)
        if len(page) < page_size:
            break

        # Get oldest timestamp for next page
        timestamps = [r.get("timestamp") for r in page if r.get("timestamp")]
        if timestamps:
            end_ts = min(timestamps) - 1
        else:
            break

        time.sleep(0.1)

        if page_num == max_pages - 1:
            hit_limit = True

    return all_results, hit_limit


def fetch_trader(
    wallet: str,
    use_proxy: bool = True,
    incremental: bool = True,
) -> dict:
    """Fetch all positions, trades, activity for a wallet with full pagination.

    Args:
        wallet: Ethereum wallet address (0x...)
        use_proxy: Whether to use SOCKS5 proxy (for US access)
        incremental: If True and existing data exists, only fetch new activity

    Returns:
        Dict with wallet, positions, trades, activity, value, fetched_at keys
    """
    existing = load_existing(wallet) if incremental else None
    last_fetch = existing.get("fetched_at") if existing else None

    try:
        client = _make_client(use_proxy)
    except Exception as e:
        if use_proxy:
            print(f"Proxy error: {e}. Try --no-proxy if not in US.", file=sys.stderr)
            sys.exit(1)
        raise

    result = {
        "wallet": wallet,
        "fetched_at": datetime.now(timezone.utc).isoformat(),
        "data_limits": {},  # Track if we hit pagination limits
    }

    try:
        print(f"Fetching positions for {wallet[:10]}...", file=sys.stderr)
        positions, pos_limit = _paginate(
            client, "/positions", {"user": wallet, "sizeThreshold": 0}
        )
        result["positions"] = positions
        result["data_limits"]["positions"] = pos_limit
        print(f"  Found {len(positions)} positions{' (HIT LIMIT)' if pos_limit else ''}", file=sys.stderr)

        print("Fetching trades...", file=sys.stderr)
        trades, trades_limit = _paginate(client, "/trades", {"user": wallet})
        result["trades"] = trades
        result["data_limits"]["trades"] = trades_limit
        print(f"  Found {len(trades)} trades{' (HIT LIMIT)' if trades_limit else ''}", file=sys.stderr)

        # For activity, use incremental if we have existing data
        since_ts = None
        if existing and last_fetch:
            # Get newest activity timestamp from existing data
            existing_activity = existing.get("activity", [])
            if existing_activity:
                timestamps = [a.get("timestamp") for a in existing_activity if a.get("timestamp")]
                if timestamps:
                    since_ts = max(timestamps)

        print("Fetching activity...", file=sys.stderr)
        new_activity, activity_limit = _paginate_by_timestamp(
            client, "/activity", {"user": wallet}, since_ts=since_ts
        )

        if existing and since_ts:
            # Merge with existing, dedup by txHash
            existing_hashes = {a.get("txHash") for a in existing.get("activity", [])}
            new_unique = [a for a in new_activity if a.get("txHash") not in existing_hashes]
            result["activity"] = new_unique + existing.get("activity", [])
            print(f"  Found {len(new_unique)} new, {len(result['activity'])} total", file=sys.stderr)
        else:
            result["activity"] = new_activity
            print(f"  Found {len(result['activity'])} activities{' (HIT LIMIT)' if activity_limit else ''}", file=sys.stderr)
        result["data_limits"]["activity"] = activity_limit

        # Portfolio value (single request)
        resp = client.get(f"{DATA_API}/value", params={"user": wallet})
        result["value"] = resp.json() if resp.is_success else None

    finally:
        client.close()

    return result


def fetch_holders(market_id: str, use_proxy: bool = True, limit: int = 50) -> dict:
    """Fetch top holders for a market."""
    client = _make_client(use_proxy)
    try:
        resp = client.get(
            f"{DATA_API}/holders", params={"market": market_id, "limit": limit}
        )
        holders = resp.json() if resp.is_success else []
    finally:
        client.close()
    return {"market_id": market_id, "holders": holders}


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Fetch Polymarket trader data")
    parser.add_argument("wallet", help="Wallet address (0x...) or market ID with --holders")
    parser.add_argument("--no-proxy", action="store_true", help="Disable SOCKS5 proxy")
    parser.add_argument("--refresh", action="store_true", help="Force full refresh (ignore existing data)")
    parser.add_argument("--stdout", action="store_true", help="Print to stdout instead of saving")
    parser.add_argument("--holders", action="store_true", help="Fetch market holders instead")
    parser.add_argument("--limit", type=int, default=50, help="Limit for holders query")
    args = parser.parse_args()

    use_proxy = not args.no_proxy

    if args.holders:
        data = fetch_holders(args.wallet, use_proxy=use_proxy, limit=args.limit)
        print(json.dumps(data, indent=2))
    else:
        data = fetch_trader(args.wallet, use_proxy=use_proxy, incremental=not args.refresh)

        if args.stdout:
            print(json.dumps(data, indent=2))
        else:
            path = save_trader(data)
            print(f"Saved to {path}", file=sys.stderr)
            print(json.dumps({
                "wallet": data["wallet"],
                "positions": len(data["positions"]),
                "trades": len(data["trades"]),
                "activity": len(data["activity"]),
                "value": data["value"],
                "path": str(path),
            }, indent=2))
