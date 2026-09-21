#!/usr/bin/env python3
from __future__ import annotations

import argparse
import hashlib
import json
from pathlib import Path
from urllib.parse import urlsplit, urlunsplit


def prolog_string(value: str) -> str:
    return json.dumps(value, ensure_ascii=False)


def normalize_url(value: str) -> str:
    parts = urlsplit(value.strip())
    if parts.scheme not in {"http", "https"} or not parts.netloc:
        raise ValueError("result URL must be absolute http(s)")
    host = parts.hostname.lower() if parts.hostname else ""
    port = f":{parts.port}" if parts.port else ""
    netloc = host + port
    return urlunsplit((parts.scheme.lower(), netloc, parts.path or "/", parts.query, ""))


def extract_results(payload: dict) -> list[dict]:
    web = payload.get("web")
    if isinstance(web, dict) and isinstance(web.get("results"), list):
        return web["results"]
    if isinstance(payload.get("results"), list):
        return payload["results"]
    raise ValueError("Brave payload has no results list")


def convert(payload: dict, query: str, retrieved_at: str, limit: int) -> str:
    if limit < 1 or limit > 50:
        raise ValueError("limit must be between 1 and 50")
    query = query.strip()
    if not query:
        raise ValueError("query must not be empty")
    if not retrieved_at.strip():
        raise ValueError("retrieved_at must not be empty")

    query_id = hashlib.sha256(query.encode()).hexdigest()[:16]
    rows = []
    seen = set()
    for raw in extract_results(payload):
        if len(rows) >= limit:
            break
        if not isinstance(raw, dict):
            continue
        url = normalize_url(str(raw.get("url", "")))
        if url in seen:
            continue
        seen.add(url)
        title = str(raw.get("title") or "").strip()
        snippet = str(raw.get("description") or raw.get("snippet") or "").strip()
        published = str(
            raw.get("page_age") or raw.get("age") or raw.get("published") or ""
        ).strip()
        domain = urlsplit(url).hostname or ""
        digest = hashlib.sha256(
            "\n".join((url, title, snippet, published)).encode("utf-8")
        ).hexdigest()
        rows.append((len(rows) + 1, url, title, snippet, domain, published, digest))

    lines = [
        "% Generated Brave Search evidence. Untrusted evidence only.",
        f"search_query({prolog_string(query_id)}, {prolog_string(query)}, {prolog_string(retrieved_at)}).",
    ]
    for rank, url, title, snippet, domain, published, digest in rows:
        lines.append(
            "search_evidence("
            f"{prolog_string(query_id)}, {rank}, "
            f"{prolog_string(url)}, {prolog_string(title)}, {prolog_string(snippet)}, "
            f"{prolog_string(domain)}, {prolog_string(published)}, {prolog_string(retrieved_at)}, "
            f"{prolog_string(digest)})."
        )
    return "\n".join(lines) + "\n"


def main() -> int:
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", type=Path, required=True)
    parser.add_argument("--query", required=True)
    parser.add_argument("--retrieved-at", required=True)
    parser.add_argument("--limit", type=int, default=20)
    parser.add_argument("--output", type=Path, required=True)
    args = parser.parse_args()

    payload = json.loads(args.input.read_text(encoding="utf-8"))
    if not isinstance(payload, dict):
        raise ValueError("input must be a JSON object")
    text = convert(payload, args.query, args.retrieved_at, args.limit)
    args.output.parent.mkdir(parents=True, exist_ok=True)
    args.output.write_text(text, encoding="utf-8")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
