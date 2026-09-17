from __future__ import annotations

import json
import os
import socket
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass
from typing import Any, Callable, Mapping, Optional

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field

from zara.plugins import PluginMetadata, ServicePlugin


DEFAULT_ALPHA_VANTAGE_ENDPOINT = "https://www.alphavantage.co/query"
DEFAULT_TIMEOUT_SECONDS = 15.0
MAX_BARS = 100


class MarketDataError(RuntimeError):
    pass


class MarketQuoteArgs(BaseModel):
    symbol: str = Field(..., min_length=1, max_length=32)


class MarketSearchArgs(BaseModel):
    query: str = Field(..., min_length=1, max_length=120)


class MarketBarsArgs(BaseModel):
    symbol: str = Field(..., min_length=1, max_length=32)
    limit: int = Field(20, ge=1, le=MAX_BARS)


@dataclass(frozen=True)
class MarketProviderConfig:
    provider: str = "alpha_vantage"
    endpoint: str = DEFAULT_ALPHA_VANTAGE_ENDPOINT
    api_key_env: str = "ALPHAVANTAGE_API_KEY"
    api_key: str = ""
    timeout_seconds: float = DEFAULT_TIMEOUT_SECONDS

    @classmethod
    def from_mapping(cls, configuration: Mapping[str, Any]) -> "MarketProviderConfig":
        provider = str(configuration.get("provider", "alpha_vantage")).strip().lower()
        if provider != "alpha_vantage":
            raise ValueError(f"unsupported built-in market provider: {provider!r}")

        endpoint = str(
            configuration.get("endpoint", DEFAULT_ALPHA_VANTAGE_ENDPOINT)
        ).strip()
        if not endpoint.startswith(("https://", "http://")):
            raise ValueError("market-data endpoint must be an http(s) URL")

        api_key_env = str(
            configuration.get("api_key_env", "ALPHAVANTAGE_API_KEY")
        ).strip()
        if not api_key_env:
            raise ValueError("market-data api_key_env must not be empty")

        api_key = str(configuration.get("api_key", "")).strip()
        timeout_seconds = float(
            configuration.get("timeout_seconds", DEFAULT_TIMEOUT_SECONDS)
        )
        if not 0.1 <= timeout_seconds <= 120.0:
            raise ValueError("market-data timeout_seconds must be between 0.1 and 120")

        return cls(
            provider=provider,
            endpoint=endpoint,
            api_key_env=api_key_env,
            api_key=api_key,
            timeout_seconds=timeout_seconds,
        )


class MarketDataClient:
    def __init__(
        self,
        config: MarketProviderConfig,
        *,
        request_json: Optional[Callable[[str, float], dict[str, Any]]] = None,
    ) -> None:
        self.config = config
        self._request_json = request_json or self._default_request_json

    def quote(self, symbol: str) -> dict[str, Any]:
        normalized = self._normalize_symbol(symbol)
        payload = self._alpha_vantage_request(
            function="GLOBAL_QUOTE",
            symbol=normalized,
        )
        global_quote = payload.get("Global Quote")
        if not isinstance(global_quote, dict) or not global_quote.get("01. symbol"):
            raise MarketDataError("Alpha Vantage quote response is malformed")
        return {
            "provider": "alpha_vantage",
            "symbol": str(global_quote["01. symbol"]),
            "price": self._number(global_quote.get("05. price")),
            "open": self._number(global_quote.get("02. open")),
            "high": self._number(global_quote.get("03. high")),
            "low": self._number(global_quote.get("04. low")),
            "volume": self._integer_or_none(global_quote.get("06. volume")),
            "trading_day": str(global_quote.get("07. latest trading day", "")),
            "previous_close": self._number(global_quote.get("08. previous close")),
            "change": self._number(global_quote.get("09. change")),
            "change_percent": self._percent(global_quote.get("10. change percent")),
        }

    def search(self, query: str) -> list[dict[str, Any]]:
        query = " ".join(str(query).split()).strip()
        if not query:
            raise ValueError("market search query must not be empty")
        payload = self._alpha_vantage_request(
            function="SYMBOL_SEARCH",
            keywords=query,
        )
        matches = payload.get("bestMatches")
        if not isinstance(matches, list):
            raise MarketDataError("Alpha Vantage symbol-search response is malformed")
        result: list[dict[str, Any]] = []
        for item in matches:
            if not isinstance(item, dict):
                continue
            result.append(
                {
                    "provider": "alpha_vantage",
                    "symbol": str(item.get("1. symbol", "")),
                    "name": str(item.get("2. name", "")),
                    "type": str(item.get("3. type", "")),
                    "region": str(item.get("4. region", "")),
                    "currency": str(item.get("8. currency", "")),
                    "score": self._number(item.get("9. matchScore")),
                }
            )
        return result

    def daily_bars(self, symbol: str, limit: int = 20) -> list[dict[str, Any]]:
        normalized = self._normalize_symbol(symbol)
        limit = int(limit)
        if not 1 <= limit <= MAX_BARS:
            raise ValueError(f"market bar limit must be between 1 and {MAX_BARS}")
        payload = self._alpha_vantage_request(
            function="TIME_SERIES_DAILY",
            symbol=normalized,
            outputsize="compact",
        )
        series = payload.get("Time Series (Daily)")
        if not isinstance(series, dict):
            raise MarketDataError("Alpha Vantage daily-bars response is malformed")

        bars: list[dict[str, Any]] = []
        for date in sorted(series, reverse=True)[:limit]:
            item = series[date]
            if not isinstance(item, dict):
                continue
            bars.append(
                {
                    "provider": "alpha_vantage",
                    "date": date,
                    "open": self._number(item.get("1. open")),
                    "high": self._number(item.get("2. high")),
                    "low": self._number(item.get("3. low")),
                    "close": self._number(item.get("4. close")),
                    "volume": self._integer_or_none(item.get("5. volume")),
                }
            )
        return bars

    def _alpha_vantage_request(self, **parameters: str) -> dict[str, Any]:
        key = self.config.api_key or os.getenv(self.config.api_key_env, "").strip()
        if not key:
            raise MarketDataError(
                f"market-data API key is not configured; set {self.config.api_key_env}"
            )
        query = urllib.parse.urlencode({**parameters, "apikey": key})
        payload = self._request_json(
            f"{self.config.endpoint}?{query}",
            self.config.timeout_seconds,
        )
        self._raise_provider_error(payload)
        return payload

    @staticmethod
    def _default_request_json(url: str, timeout: float) -> dict[str, Any]:
        request = urllib.request.Request(
            url,
            headers={"User-Agent": "Zara/market-data"},
            method="GET",
        )
        try:
            with urllib.request.urlopen(request, timeout=timeout) as response:
                status = int(getattr(response, "status", 200))
                body = response.read().decode("utf-8")
        except urllib.error.HTTPError as error:
            raise MarketDataError(f"market-data HTTP error {error.code}") from error
        except (urllib.error.URLError, TimeoutError, socket.timeout) as error:
            raise MarketDataError(f"market-data request failed: {error}") from error
        if not 200 <= status < 300:
            raise MarketDataError(f"market-data HTTP error {status}")
        try:
            payload = json.loads(body)
        except json.JSONDecodeError as error:
            raise MarketDataError("market-data provider returned invalid JSON") from error
        if not isinstance(payload, dict):
            raise MarketDataError("market-data provider returned a non-object JSON value")
        return payload

    @staticmethod
    def _raise_provider_error(payload: Mapping[str, Any]) -> None:
        if "Error Message" in payload:
            raise MarketDataError(f"Alpha Vantage invalid request: {payload['Error Message']}")
        if "Note" in payload:
            raise MarketDataError(f"Alpha Vantage rate limit: {payload['Note']}")
        if "Information" in payload:
            raise MarketDataError(f"Alpha Vantage information: {payload['Information']}")

    @staticmethod
    def _normalize_symbol(symbol: str) -> str:
        normalized = "".join(str(symbol).split()).upper()
        if not normalized or len(normalized) > 32:
            raise ValueError("market symbol must contain 1 to 32 non-space characters")
        return normalized

    @staticmethod
    def _number(value: Any) -> float:
        if value in (None, ""):
            raise MarketDataError("market-data provider omitted a required numeric field")
        try:
            return float(value)
        except (TypeError, ValueError) as error:
            raise MarketDataError(f"invalid numeric market-data field: {value!r}") from error

    @classmethod
    def _percent(cls, value: Any) -> float:
        text = str(value or "").strip()
        if text.endswith("%"):
            text = text[:-1]
        return cls._number(text)

    @staticmethod
    def _integer_or_none(value: Any) -> Optional[int]:
        if value in (None, ""):
            return None
        try:
            return int(value)
        except (TypeError, ValueError) as error:
            raise MarketDataError(f"invalid integer market-data field: {value!r}") from error


class MarketDataPlugin(ServicePlugin):
    enabled_by_default = False
    metadata = PluginMetadata(
        name="market-data",
        version="0.1.0",
        api_version="1",
        description="Stock quotes, symbol search, and daily bars for Zara and executable Prolog config.",
    )

    def __init__(self) -> None:
        self._client: Optional[MarketDataClient] = None

    def start(self, runtime) -> None:
        config = MarketProviderConfig.from_mapping(runtime.configuration)
        self._client = MarketDataClient(config)

    def stop(self) -> None:
        self._client = None

    def tools(self):
        def market_quote(symbol: str) -> str:
            return json.dumps(self._require_client().quote(symbol), sort_keys=True)

        def market_symbol_search(query: str) -> str:
            return json.dumps(self._require_client().search(query), sort_keys=True)

        def market_daily_bars(symbol: str, limit: int = 20) -> str:
            return json.dumps(
                self._require_client().daily_bars(symbol, limit),
                sort_keys=True,
            )

        return (
            StructuredTool.from_function(
                market_quote,
                name="market_quote",
                description="Get a normalized current stock quote from the configured market-data provider.",
                args_schema=MarketQuoteArgs,
            ),
            StructuredTool.from_function(
                market_symbol_search,
                name="market_symbol_search",
                description="Search stock/instrument symbols using the configured market-data provider.",
                args_schema=MarketSearchArgs,
            ),
            StructuredTool.from_function(
                market_daily_bars,
                name="market_daily_bars",
                description="Get recent normalized daily OHLCV bars for a stock symbol.",
                args_schema=MarketBarsArgs,
            ),
        )

    def _require_client(self) -> MarketDataClient:
        if self._client is None:
            raise RuntimeError("market-data plugin is not running")
        return self._client


def create_plugin():
    return MarketDataPlugin()
