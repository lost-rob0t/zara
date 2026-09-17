from __future__ import annotations

import json
import urllib.parse
from types import SimpleNamespace

import pytest

from zara.plugins.builtin.market_data import (
    MarketDataClient,
    MarketDataError,
    MarketDataPlugin,
    MarketProviderConfig,
)


def quote_payload() -> dict:
    return {
        "Global Quote": {
            "01. symbol": "AAPL",
            "02. open": "225.00",
            "03. high": "230.00",
            "04. low": "224.00",
            "05. price": "228.50",
            "06. volume": "1234567",
            "07. latest trading day": "2026-09-16",
            "08. previous close": "226.00",
            "09. change": "2.50",
            "10. change percent": "1.1062%",
        }
    }


def test_provider_config_defaults() -> None:
    config = MarketProviderConfig.from_mapping({})
    assert config.provider == "alpha_vantage"
    assert config.api_key_env == "ALPHAVANTAGE_API_KEY"
    assert config.timeout_seconds == 15.0


def test_quote_normalizes_alpha_vantage_response() -> None:
    seen: list[tuple[str, float]] = []

    def request_json(url: str, timeout: float) -> dict:
        seen.append((url, timeout))
        return quote_payload()

    client = MarketDataClient(
        MarketProviderConfig(api_key="secret", timeout_seconds=7.0),
        request_json=request_json,
    )
    quote = client.quote(" aapl ")

    assert quote == {
        "provider": "alpha_vantage",
        "symbol": "AAPL",
        "price": 228.5,
        "open": 225.0,
        "high": 230.0,
        "low": 224.0,
        "volume": 1234567,
        "trading_day": "2026-09-16",
        "previous_close": 226.0,
        "change": 2.5,
        "change_percent": 1.1062,
    }
    assert len(seen) == 1
    url, timeout = seen[0]
    query = urllib.parse.parse_qs(urllib.parse.urlparse(url).query)
    assert query["function"] == ["GLOBAL_QUOTE"]
    assert query["symbol"] == ["AAPL"]
    assert query["apikey"] == ["secret"]
    assert timeout == 7.0


def test_search_normalizes_matches() -> None:
    payload = {
        "bestMatches": [
            {
                "1. symbol": "IBM",
                "2. name": "International Business Machines",
                "3. type": "Equity",
                "4. region": "United States",
                "8. currency": "USD",
                "9. matchScore": "1.0000",
            }
        ]
    }
    client = MarketDataClient(
        MarketProviderConfig(api_key="secret"),
        request_json=lambda _url, _timeout: payload,
    )

    assert client.search("IBM") == [
        {
            "provider": "alpha_vantage",
            "symbol": "IBM",
            "name": "International Business Machines",
            "type": "Equity",
            "region": "United States",
            "currency": "USD",
            "score": 1.0,
        }
    ]


def test_daily_bars_are_newest_first_and_limited() -> None:
    payload = {
        "Time Series (Daily)": {
            "2026-09-14": {
                "1. open": "10",
                "2. high": "12",
                "3. low": "9",
                "4. close": "11",
                "5. volume": "100",
            },
            "2026-09-16": {
                "1. open": "12",
                "2. high": "14",
                "3. low": "11",
                "4. close": "13",
                "5. volume": "300",
            },
            "2026-09-15": {
                "1. open": "11",
                "2. high": "13",
                "3. low": "10",
                "4. close": "12",
                "5. volume": "200",
            },
        }
    }
    client = MarketDataClient(
        MarketProviderConfig(api_key="secret"),
        request_json=lambda _url, _timeout: payload,
    )

    bars = client.daily_bars("AAPL", 2)
    assert [bar["date"] for bar in bars] == ["2026-09-16", "2026-09-15"]
    assert [bar["close"] for bar in bars] == [13.0, 12.0]


def test_missing_api_key_fails_before_network(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.delenv("ALPHAVANTAGE_API_KEY", raising=False)

    def unexpected_request(_url: str, _timeout: float) -> dict:
        raise AssertionError("network requester must not be called without a key")

    client = MarketDataClient(
        MarketProviderConfig(),
        request_json=unexpected_request,
    )
    with pytest.raises(MarketDataError, match="ALPHAVANTAGE_API_KEY"):
        client.quote("AAPL")


def test_provider_rate_limit_is_typed() -> None:
    client = MarketDataClient(
        MarketProviderConfig(api_key="secret"),
        request_json=lambda _url, _timeout: {"Note": "rate limited"},
    )
    with pytest.raises(MarketDataError, match="rate limit"):
        client.quote("AAPL")


def test_plugin_exposes_market_tools_before_lifecycle_start() -> None:
    plugin = MarketDataPlugin()
    tools = {tool.name: tool for tool in plugin.tools()}
    assert plugin.enabled_by_default is False
    assert set(tools) == {
        "market_quote",
        "market_symbol_search",
        "market_daily_bars",
    }

    plugin.start(SimpleNamespace(configuration={"api_key": "secret"}))
    assert plugin._client is not None
    plugin._client._request_json = lambda _url, _timeout: quote_payload()
    rendered = tools["market_quote"].invoke({"symbol": "AAPL"})
    result = json.loads(rendered)
    assert result["symbol"] == "AAPL"
    assert result["price"] == 228.5
    plugin.stop()
