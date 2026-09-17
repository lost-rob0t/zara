from __future__ import annotations

import base64
import json
import math
import os
import random
import socket
import threading
import time
import urllib.error
import urllib.parse
import urllib.request
import uuid
from copy import deepcopy
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Mapping, Optional

from langchain_core.tools import StructuredTool

from zara.plugins import PluginMetadata, ServicePlugin


STATE_VERSION = 1
MODE_PAPER = "paper"
MODE_ACTIVE = "active"
DEFAULT_PRODUCTION_BASE_URL = "https://external-api.kalshi.com/trade-api/v2"
DEFAULT_DEMO_BASE_URL = "https://external-api.demo.kalshi.co/trade-api/v2"
GENE_BOUNDS = {
    "momentum_weight": (-2.0, 2.0),
    "liquidity_weight": (-1.0, 1.0),
    "volume_weight": (-1.0, 1.0),
    "spread_penalty": (0.0, 3.0),
    "bias": (-0.15, 0.15),
    "min_edge": (0.005, 0.25),
    "max_contract_risk": (0.05, 0.95),
    "hold_cycles": (1.0, 24.0),
}


class KalshiBotError(RuntimeError):
    pass


@dataclass(frozen=True)
class KalshiBotConfig:
    mode: str = MODE_PAPER
    active_enabled: bool = False
    auto_run: bool = True
    base_url: str = DEFAULT_PRODUCTION_BASE_URL
    api_key_id_env: str = "KALSHI_API_KEY_ID"
    private_key_path_env: str = "KALSHI_PRIVATE_KEY_PATH"
    timeout_seconds: float = 15.0
    poll_seconds: float = 60.0
    market_limit: int = 100
    paper_bankroll_dollars: float = 1000.0
    paper_max_order_dollars: float = 10.0
    paper_daily_spend_cap_dollars: float = 100.0
    paper_max_open_exposure_dollars: float = 250.0
    active_bankroll_cap_dollars: float = 25.0
    active_max_order_dollars: float = 1.0
    active_daily_spend_cap_dollars: float = 5.0
    active_max_open_exposure_dollars: float = 10.0
    population_size: int = 12
    evolution_cycles_per_generation: int = 20
    mutation_rate: float = 0.30
    mutation_scale: float = 0.12
    random_seed: int = 545
    state_path: str = ""

    @classmethod
    def from_mapping(cls, configuration: Mapping[str, Any]) -> "KalshiBotConfig":
        mode = str(configuration.get("mode", MODE_PAPER)).strip().lower()
        if mode not in {MODE_PAPER, MODE_ACTIVE}:
            raise ValueError("kalshi-bot mode must be 'paper' or 'active'")

        base_url = str(
            configuration.get("base_url", DEFAULT_PRODUCTION_BASE_URL)
        ).strip().rstrip("/")
        if not base_url.startswith(("https://", "http://")):
            raise ValueError("kalshi-bot base_url must be an http(s) URL")

        timeout_seconds = float(configuration.get("timeout_seconds", 15.0))
        poll_seconds = float(configuration.get("poll_seconds", 60.0))
        market_limit = int(configuration.get("market_limit", 100))
        population_size = int(configuration.get("population_size", 12))
        cycles = int(configuration.get("evolution_cycles_per_generation", 20))
        mutation_rate = float(configuration.get("mutation_rate", 0.30))
        mutation_scale = float(configuration.get("mutation_scale", 0.12))
        random_seed = int(configuration.get("random_seed", 545))

        if not 0.1 <= timeout_seconds <= 120.0:
            raise ValueError("kalshi-bot timeout_seconds must be between 0.1 and 120")
        if not 5.0 <= poll_seconds <= 86400.0:
            raise ValueError("kalshi-bot poll_seconds must be between 5 and 86400")
        if not 1 <= market_limit <= 200:
            raise ValueError("kalshi-bot market_limit must be between 1 and 200")
        if not 4 <= population_size <= 64:
            raise ValueError("kalshi-bot population_size must be between 4 and 64")
        if not 2 <= cycles <= 10000:
            raise ValueError("kalshi-bot evolution_cycles_per_generation must be >= 2")
        if not 0.0 <= mutation_rate <= 1.0:
            raise ValueError("kalshi-bot mutation_rate must be between 0 and 1")
        if not 0.0 <= mutation_scale <= 1.0:
            raise ValueError("kalshi-bot mutation_scale must be between 0 and 1")

        paper_bankroll = _positive(configuration, "paper_bankroll_dollars", 1000.0)
        paper_order = _positive(configuration, "paper_max_order_dollars", 10.0)
        paper_daily = _positive(configuration, "paper_daily_spend_cap_dollars", 100.0)
        paper_exposure = _positive(
            configuration, "paper_max_open_exposure_dollars", 250.0
        )
        active_bankroll = _positive(configuration, "active_bankroll_cap_dollars", 25.0)
        active_order = _positive(configuration, "active_max_order_dollars", 1.0)
        active_daily = _positive(configuration, "active_daily_spend_cap_dollars", 5.0)
        active_exposure = _positive(
            configuration, "active_max_open_exposure_dollars", 10.0
        )

        if paper_order > paper_bankroll or paper_exposure > paper_bankroll:
            raise ValueError("paper order/exposure caps may not exceed paper bankroll")
        if active_order > active_daily:
            raise ValueError("active_max_order_dollars may not exceed active daily spend cap")
        if active_order > active_bankroll or active_exposure > active_bankroll:
            raise ValueError("active order/exposure caps may not exceed active bankroll cap")

        state_path = str(configuration.get("state_path", "")).strip()
        return cls(
            mode=mode,
            active_enabled=bool(configuration.get("active_enabled", False)),
            auto_run=bool(configuration.get("auto_run", True)),
            base_url=base_url,
            api_key_id_env=str(
                configuration.get("api_key_id_env", "KALSHI_API_KEY_ID")
            ).strip(),
            private_key_path_env=str(
                configuration.get("private_key_path_env", "KALSHI_PRIVATE_KEY_PATH")
            ).strip(),
            timeout_seconds=timeout_seconds,
            poll_seconds=poll_seconds,
            market_limit=market_limit,
            paper_bankroll_dollars=paper_bankroll,
            paper_max_order_dollars=paper_order,
            paper_daily_spend_cap_dollars=paper_daily,
            paper_max_open_exposure_dollars=paper_exposure,
            active_bankroll_cap_dollars=active_bankroll,
            active_max_order_dollars=active_order,
            active_daily_spend_cap_dollars=active_daily,
            active_max_open_exposure_dollars=active_exposure,
            population_size=population_size,
            evolution_cycles_per_generation=cycles,
            mutation_rate=mutation_rate,
            mutation_scale=mutation_scale,
            random_seed=random_seed,
            state_path=state_path,
        )

    def resolved_state_path(self) -> Path:
        if self.state_path:
            return Path(self.state_path).expanduser()
        state_home = os.getenv("XDG_STATE_HOME", "").strip()
        if state_home:
            return Path(state_home) / "zarathushtra" / "kalshi-bot.json"
        return Path.home() / ".local" / "state" / "zarathushtra" / "kalshi-bot.json"


def _positive(configuration: Mapping[str, Any], key: str, default: float) -> float:
    value = float(configuration.get(key, default))
    if not math.isfinite(value) or value <= 0:
        raise ValueError(f"kalshi-bot {key} must be a positive finite number")
    return value


class KalshiClient:
    """Small REST V2 client with injectable I/O for deterministic tests."""

    def __init__(
        self,
        config: KalshiBotConfig,
        *,
        request_json: Optional[
            Callable[[str, str, Optional[dict[str, Any]], bool], dict[str, Any]]
        ] = None,
    ) -> None:
        self.config = config
        self._request_json = request_json or self._default_request_json
        self._private_key = None

    @property
    def has_credentials(self) -> bool:
        return bool(self._api_key_id() and self._private_key_path())

    def list_markets(self) -> list[dict[str, Any]]:
        payload = self._request_json(
            "GET",
            f"/markets?limit={self.config.market_limit}&status=open",
            None,
            False,
        )
        markets = payload.get("markets", [])
        if not isinstance(markets, list):
            raise KalshiBotError("Kalshi markets response is malformed")
        return [dict(item) for item in markets if isinstance(item, dict)]

    def get_market(self, ticker: str) -> dict[str, Any]:
        safe = urllib.parse.quote(str(ticker), safe="")
        payload = self._request_json("GET", f"/markets/{safe}", None, False)
        market = payload.get("market")
        if not isinstance(market, dict):
            raise KalshiBotError(f"Kalshi market response is malformed for {ticker}")
        return dict(market)

    def get_balance(self) -> dict[str, Any]:
        return self._request_json("GET", "/portfolio/balance", None, True)

    def create_order(
        self,
        *,
        ticker: str,
        side: str,
        price: float,
        count: float,
        client_order_id: Optional[str] = None,
    ) -> dict[str, Any]:
        if side not in {"bid", "ask"}:
            raise ValueError("Kalshi V2 order side must be bid or ask")
        if not 0.01 <= price <= 0.99:
            raise ValueError("Kalshi order price must be between 0.01 and 0.99")
        if count <= 0:
            raise ValueError("Kalshi order count must be positive")
        body = {
            "ticker": str(ticker),
            "client_order_id": client_order_id or uuid.uuid4().hex,
            "side": side,
            "count": f"{count:.2f}",
            "price": f"{price:.4f}",
            "time_in_force": "fill_or_kill",
            "self_trade_prevention_type": "taker_at_cross",
            "post_only": False,
            "cancel_order_on_pause": True,
            "reduce_only": False,
            "subaccount": 0,
            "exchange_index": 0,
        }
        return self._request_json(
            "POST", "/portfolio/events/orders", body, True
        )

    def _default_request_json(
        self,
        method: str,
        path: str,
        body: Optional[dict[str, Any]],
        authenticated: bool,
    ) -> dict[str, Any]:
        method = method.upper()
        headers = {"User-Agent": "Zara/kalshi-bot", "Accept": "application/json"}
        payload_bytes: Optional[bytes] = None
        if body is not None:
            payload_bytes = json.dumps(body, separators=(",", ":")).encode("utf-8")
            headers["Content-Type"] = "application/json"
        if authenticated:
            headers.update(self._auth_headers(method, path))
        request = urllib.request.Request(
            self.config.base_url + path,
            headers=headers,
            method=method,
            data=payload_bytes,
        )
        try:
            with urllib.request.urlopen(
                request, timeout=self.config.timeout_seconds
            ) as response:
                status = int(getattr(response, "status", 200))
                text = response.read().decode("utf-8")
        except urllib.error.HTTPError as error:
            detail = error.read().decode("utf-8", errors="replace")[:500]
            raise KalshiBotError(
                f"Kalshi HTTP {error.code}: {detail or error.reason}"
            ) from error
        except (urllib.error.URLError, TimeoutError, socket.timeout) as error:
            raise KalshiBotError(f"Kalshi request failed: {error}") from error
        if not 200 <= status < 300:
            raise KalshiBotError(f"Kalshi HTTP error {status}")
        try:
            decoded = json.loads(text) if text else {}
        except json.JSONDecodeError as error:
            raise KalshiBotError("Kalshi returned invalid JSON") from error
        if not isinstance(decoded, dict):
            raise KalshiBotError("Kalshi returned a non-object JSON response")
        return decoded

    def _auth_headers(self, method: str, path: str) -> dict[str, str]:
        api_key_id = self._api_key_id()
        key_path = self._private_key_path()
        if not api_key_id or not key_path:
            raise KalshiBotError(
                "Kalshi credentials are not configured; set "
                f"{self.config.api_key_id_env} and {self.config.private_key_path_env}"
            )
        timestamp = str(int(time.time() * 1000))
        base_path = urllib.parse.urlsplit(self.config.base_url).path.rstrip("/")
        signed_path = (base_path + path).split("?", 1)[0]
        signature = self._sign(timestamp + method.upper() + signed_path, key_path)
        return {
            "KALSHI-ACCESS-KEY": api_key_id,
            "KALSHI-ACCESS-TIMESTAMP": timestamp,
            "KALSHI-ACCESS-SIGNATURE": signature,
        }

    def _api_key_id(self) -> str:
        return os.getenv(self.config.api_key_id_env, "").strip()

    def _private_key_path(self) -> str:
        return os.getenv(self.config.private_key_path_env, "").strip()

    def _sign(self, text: str, key_path: str) -> str:
        try:
            from cryptography.hazmat.primitives import hashes, serialization
            from cryptography.hazmat.primitives.asymmetric import padding
        except ImportError as error:
            raise KalshiBotError(
                "cryptography is required for active Kalshi request signing"
            ) from error
        if self._private_key is None:
            try:
                self._private_key = serialization.load_pem_private_key(
                    Path(key_path).expanduser().read_bytes(), password=None
                )
            except (OSError, ValueError, TypeError) as error:
                raise KalshiBotError(f"cannot load Kalshi private key: {error}") from error
        signature = self._private_key.sign(
            text.encode("utf-8"),
            padding.PSS(
                mgf=padding.MGF1(hashes.SHA256()),
                salt_length=padding.PSS.DIGEST_LENGTH,
            ),
            hashes.SHA256(),
        )
        return base64.b64encode(signature).decode("ascii")


class KalshiStateStore:
    def __init__(self, path: Path, config: KalshiBotConfig) -> None:
        self.path = Path(path)
        self.config = config
        self._lock = threading.RLock()
        self.state = self._load_or_default()

    def snapshot(self) -> dict[str, Any]:
        with self._lock:
            return deepcopy(self.state)

    def save(self) -> None:
        with self._lock:
            self.path.parent.mkdir(parents=True, exist_ok=True, mode=0o700)
            try:
                os.chmod(self.path.parent, 0o700)
            except OSError:
                pass
            temporary = self.path.with_name(f".{self.path.name}.tmp")
            descriptor = os.open(
                temporary, os.O_WRONLY | os.O_CREAT | os.O_TRUNC, 0o600
            )
            with os.fdopen(descriptor, "w", encoding="utf-8") as output:
                json.dump(self.state, output, indent=2, sort_keys=True)
                output.write("\n")
                output.flush()
                os.fsync(output.fileno())
            os.replace(temporary, self.path)

    def _load_or_default(self) -> dict[str, Any]:
        if not self.path.exists():
            return _default_state(self.config)
        try:
            raw = json.loads(self.path.read_text(encoding="utf-8"))
        except (OSError, json.JSONDecodeError) as error:
            raise KalshiBotError(f"cannot read Kalshi bot state: {error}") from error
        if raw.get("version") != STATE_VERSION:
            raise KalshiBotError(
                f"unsupported Kalshi bot state version {raw.get('version')!r}"
            )
        raw.setdefault("paper", _account_state(self.config.paper_bankroll_dollars))
        raw.setdefault("active", _account_state(self.config.active_bankroll_cap_dollars))
        raw.setdefault("evolution", _evolution_state(self.config))
        raw.setdefault("errors", 0)
        raw.setdefault("last_error", "")
        raw.setdefault("cycle", 0)
        return raw


def _default_state(config: KalshiBotConfig) -> dict[str, Any]:
    return {
        "version": STATE_VERSION,
        "cycle": 0,
        "paper": _account_state(config.paper_bankroll_dollars),
        "active": _account_state(config.active_bankroll_cap_dollars),
        "evolution": _evolution_state(config),
        "errors": 0,
        "last_error": "",
        "last_cycle_at": 0.0,
    }


def _account_state(bankroll: float) -> dict[str, Any]:
    return {
        "bankroll": float(bankroll),
        "cash": float(bankroll),
        "positions": [],
        "trades": [],
        "orders_attempted": 0,
        "fills": 0,
        "closed_trades": 0,
        "wins": 0,
        "losses": 0,
        "pushes": 0,
        "gross_spend": 0.0,
        "realized_pnl": 0.0,
        "fees": 0.0,
        "peak_equity": float(bankroll),
        "max_drawdown": 0.0,
        "daily_spend": {},
        "reported_balance": None,
        "balance_snapshots": [],
    }


def _evolution_state(config: KalshiBotConfig) -> dict[str, Any]:
    rng = random.Random(config.random_seed)
    population = []
    for index in range(config.population_size):
        population.append(_new_member(0, index, _random_genes(rng)))
    return {
        "generation": 0,
        "generation_cycle": 0,
        "population": population,
        "history": [],
    }


def _new_member(generation: int, index: int, genes: dict[str, float]) -> dict[str, Any]:
    return {
        "id": f"g{generation:04d}-{index:03d}-{uuid.uuid4().hex[:6]}",
        "generation": generation,
        "genes": genes,
        "positions": [],
        "pnl": 0.0,
        "trades": 0,
        "wins": 0,
        "losses": 0,
        "peak_pnl": 0.0,
        "max_drawdown": 0.0,
        "fitness": 0.0,
    }


def _random_genes(rng: random.Random) -> dict[str, float]:
    return {
        "momentum_weight": rng.uniform(-0.8, 0.8),
        "liquidity_weight": rng.uniform(-0.4, 0.4),
        "volume_weight": rng.uniform(-0.4, 0.4),
        "spread_penalty": rng.uniform(0.2, 1.5),
        "bias": rng.uniform(-0.05, 0.05),
        "min_edge": rng.uniform(0.01, 0.08),
        "max_contract_risk": rng.uniform(0.30, 0.85),
        "hold_cycles": float(rng.randint(1, 8)),
    }


class EvolutionEngine:
    def __init__(self, config: KalshiBotConfig, evolution: dict[str, Any]) -> None:
        self.config = config
        self.evolution = evolution
        seed = config.random_seed + int(evolution.get("generation", 0)) * 100003
        self.rng = random.Random(seed)

    def observe(self, markets: list[dict[str, Any]], cycle: int) -> None:
        by_ticker = {str(item.get("ticker", "")): item for item in markets}
        for member in self.evolution["population"]:
            self._mark_member(member, by_ticker, cycle)
            if len(member["positions"]) >= 3:
                continue
            candidate = best_candidate(markets, member["genes"])
            if candidate is None:
                continue
            if any(p["ticker"] == candidate["ticker"] for p in member["positions"]):
                continue
            member["positions"].append(
                {
                    "ticker": candidate["ticker"],
                    "side": candidate["side"],
                    "entry_price": candidate["price"],
                    "opened_cycle": cycle,
                }
            )
        self.evolution["generation_cycle"] = int(
            self.evolution.get("generation_cycle", 0)
        ) + 1
        if self.evolution["generation_cycle"] >= self.config.evolution_cycles_per_generation:
            self.evolve()

    def champion(self) -> dict[str, Any]:
        return max(
            self.evolution["population"],
            key=lambda member: (float(member.get("fitness", 0.0)), float(member.get("pnl", 0.0))),
        )

    def evolve(self) -> None:
        population = list(self.evolution["population"])
        for member in population:
            self._update_fitness(member)
        population.sort(key=lambda member: member["fitness"], reverse=True)
        generation = int(self.evolution.get("generation", 0))
        champion = deepcopy(population[0])
        self.evolution.setdefault("history", []).append(
            {
                "generation": generation,
                "champion_id": champion["id"],
                "fitness": champion["fitness"],
                "pnl": champion["pnl"],
                "trades": champion["trades"],
                "genes": champion["genes"],
            }
        )
        self.evolution["history"] = self.evolution["history"][-200:]

        elite_count = max(2, len(population) // 4)
        elites = population[:elite_count]
        next_generation = generation + 1
        children = []
        for index in range(self.config.population_size):
            if index < min(2, elite_count):
                genes = deepcopy(elites[index]["genes"])
            else:
                first = self.rng.choice(elites)["genes"]
                second = self.rng.choice(elites)["genes"]
                genes = self._mutate(self._crossover(first, second))
            children.append(_new_member(next_generation, index, genes))
        self.evolution["generation"] = next_generation
        self.evolution["generation_cycle"] = 0
        self.evolution["population"] = children

    def _mark_member(
        self,
        member: dict[str, Any],
        by_ticker: Mapping[str, dict[str, Any]],
        cycle: int,
    ) -> None:
        kept = []
        hold_cycles = max(1, int(round(float(member["genes"]["hold_cycles"]))))
        for position in member["positions"]:
            market = by_ticker.get(position["ticker"])
            if market is None or cycle - int(position["opened_cycle"]) < hold_cycles:
                kept.append(position)
                continue
            pnl = _mark_to_market_pnl(position, market)
            member["pnl"] = float(member.get("pnl", 0.0)) + pnl
            member["trades"] = int(member.get("trades", 0)) + 1
            if pnl > 1e-9:
                member["wins"] = int(member.get("wins", 0)) + 1
            elif pnl < -1e-9:
                member["losses"] = int(member.get("losses", 0)) + 1
            member["peak_pnl"] = max(
                float(member.get("peak_pnl", 0.0)), float(member["pnl"])
            )
            drawdown = float(member["peak_pnl"]) - float(member["pnl"])
            member["max_drawdown"] = max(
                float(member.get("max_drawdown", 0.0)), drawdown
            )
        member["positions"] = kept
        self._update_fitness(member)

    @staticmethod
    def _update_fitness(member: dict[str, Any]) -> None:
        trades = int(member.get("trades", 0))
        pnl = float(member.get("pnl", 0.0))
        drawdown = float(member.get("max_drawdown", 0.0))
        win_rate = float(member.get("wins", 0)) / trades if trades else 0.0
        inactivity_penalty = 0.25 if trades == 0 else 0.0
        member["fitness"] = pnl - 1.5 * drawdown + 0.10 * win_rate - inactivity_penalty

    def _crossover(
        self, first: Mapping[str, float], second: Mapping[str, float]
    ) -> dict[str, float]:
        return {
            key: float(first[key] if self.rng.random() < 0.5 else second[key])
            for key in GENE_BOUNDS
        }

    def _mutate(self, genes: dict[str, float]) -> dict[str, float]:
        mutated = dict(genes)
        for key, (low, high) in GENE_BOUNDS.items():
            if self.rng.random() >= self.config.mutation_rate:
                continue
            span = high - low
            value = float(mutated[key]) + self.rng.gauss(0.0, span * self.config.mutation_scale)
            value = max(low, min(high, value))
            if key == "hold_cycles":
                value = float(max(1, int(round(value))))
            mutated[key] = value
        return mutated


def market_features(market: Mapping[str, Any]) -> Optional[dict[str, float]]:
    try:
        bid = float(market.get("yes_bid_dollars") or 0.0)
        ask = float(market.get("yes_ask_dollars") or 0.0)
        last = float(market.get("last_price_dollars") or 0.0)
        previous = float(market.get("previous_price_dollars") or last)
        liquidity = max(0.0, float(market.get("liquidity_dollars") or 0.0))
        volume = max(0.0, float(market.get("volume_24h_fp") or 0.0))
    except (TypeError, ValueError):
        return None
    if not (0.0 < bid < 1.0 and 0.0 < ask < 1.0 and bid <= ask):
        return None
    if not 0.0 < last < 1.0:
        last = (bid + ask) / 2.0
    if not 0.0 < previous < 1.0:
        previous = last
    return {
        "bid": bid,
        "ask": ask,
        "mid": (bid + ask) / 2.0,
        "spread": ask - bid,
        "momentum": last - previous,
        "liquidity": min(1.0, math.log1p(liquidity) / 12.0),
        "volume": min(1.0, math.log1p(volume) / 12.0),
    }


def score_candidate(
    market: Mapping[str, Any], genes: Mapping[str, float]
) -> Optional[dict[str, Any]]:
    features = market_features(market)
    ticker = str(market.get("ticker", "")).strip()
    if features is None or not ticker:
        return None
    estimate = features["mid"]
    estimate += float(genes["bias"])
    estimate += float(genes["momentum_weight"]) * features["momentum"]
    estimate += float(genes["liquidity_weight"]) * (features["liquidity"] - 0.5) * 0.04
    estimate += float(genes["volume_weight"]) * (features["volume"] - 0.5) * 0.04
    estimate -= float(genes["spread_penalty"]) * features["spread"] * 0.25
    estimate = max(0.01, min(0.99, estimate))

    yes_edge = estimate - features["ask"]
    no_edge = features["bid"] - estimate
    if yes_edge >= no_edge:
        side = "bid"
        price = features["ask"]
        edge = yes_edge
        risk = price
    else:
        side = "ask"
        price = features["bid"]
        edge = no_edge
        risk = 1.0 - price
    if edge < float(genes["min_edge"]):
        return None
    if risk > float(genes["max_contract_risk"]):
        return None
    return {
        "ticker": ticker,
        "title": str(market.get("title", "")),
        "side": side,
        "price": price,
        "edge": edge,
        "estimated_yes_probability": estimate,
        "risk_per_contract": risk,
        "score": edge - features["spread"] * 0.25,
    }


def best_candidate(
    markets: list[dict[str, Any]], genes: Mapping[str, float]
) -> Optional[dict[str, Any]]:
    candidates = [score_candidate(market, genes) for market in markets]
    filtered = [candidate for candidate in candidates if candidate is not None]
    if not filtered:
        return None
    return max(filtered, key=lambda candidate: float(candidate["score"]))


def _mark_to_market_pnl(position: Mapping[str, Any], market: Mapping[str, Any]) -> float:
    entry = float(position["entry_price"])
    count = float(position.get("count", 1.0))
    if position["side"] == "bid":
        exit_price = float(market.get("yes_bid_dollars") or entry)
        return (exit_price - entry) * count
    exit_price = float(market.get("yes_ask_dollars") or entry)
    return (entry - exit_price) * count


def _settlement_pnl(position: Mapping[str, Any], result: str) -> float:
    outcome = 1.0 if str(result).lower() == "yes" else 0.0
    entry = float(position["entry_price"])
    count = float(position.get("count", 1.0))
    gross = (
        (outcome - entry) * count
        if position["side"] == "bid"
        else (entry - outcome) * count
    )
    return gross - float(position.get("fees", 0.0))


class KalshiBotEngine:
    def __init__(
        self,
        config: KalshiBotConfig,
        store: KalshiStateStore,
        client: KalshiClient,
    ) -> None:
        self.config = config
        self.store = store
        self.client = client
        self._lock = threading.RLock()

    def step(self) -> dict[str, Any]:
        with self._lock:
            try:
                markets = self.client.list_markets()
                self._step_with_markets(markets)
                self.store.state["last_error"] = ""
            except Exception as error:
                self.store.state["errors"] = int(self.store.state.get("errors", 0)) + 1
                self.store.state["last_error"] = " ".join(str(error).split())[:500]
                self.store.save()
                raise
            self.store.save()
            return self.status()

    def status(self) -> dict[str, Any]:
        state = self.store.state
        evolution = state["evolution"]
        champion = EvolutionEngine(self.config, evolution).champion()
        selected = state[self.config.mode]
        return {
            "mode": self.config.mode,
            "auto_run": self.config.auto_run,
            "active_enabled": self.config.active_enabled,
            "active_credentials_ready": self.client.has_credentials,
            "active_armed": self._active_armed(),
            "cycle": state["cycle"],
            "generation": evolution["generation"],
            "generation_cycle": evolution["generation_cycle"],
            "champion": {
                "id": champion["id"],
                "fitness": champion["fitness"],
                "pnl": champion["pnl"],
                "trades": champion["trades"],
                "genes": champion["genes"],
            },
            "selected_stats": _stats_summary(selected),
            "paper_stats": _stats_summary(state["paper"]),
            "active_stats": _stats_summary(state["active"]),
            "errors": state.get("errors", 0),
            "last_error": state.get("last_error", ""),
        }

    def force_evolve(self) -> dict[str, Any]:
        with self._lock:
            EvolutionEngine(self.config, self.store.state["evolution"]).evolve()
            self.store.save()
            return self.status()

    def _step_with_markets(self, markets: list[dict[str, Any]]) -> None:
        state = self.store.state
        state["cycle"] = int(state.get("cycle", 0)) + 1
        cycle = state["cycle"]
        state["last_cycle_at"] = time.time()

        evolution = EvolutionEngine(self.config, state["evolution"])
        evolution.observe(markets, cycle)
        champion = evolution.champion()

        self._settle_account_positions(state["paper"], markets, MODE_PAPER)
        self._settle_account_positions(state["active"], markets, MODE_ACTIVE)

        candidate = best_candidate(markets, champion["genes"])
        if candidate is not None:
            if self.config.mode == MODE_PAPER:
                self._paper_execute(candidate, champion["id"])
            elif self._active_armed():
                self._active_execute(candidate, champion["id"])
        if self.config.mode == MODE_ACTIVE and self.client.has_credentials:
            self._sync_active_balance()

    def _paper_execute(self, candidate: Mapping[str, Any], strategy_id: str) -> None:
        account = self.store.state["paper"]
        if any(position["ticker"] == candidate["ticker"] for position in account["positions"]):
            return
        risk = float(candidate["risk_per_contract"])
        max_order = min(self.config.paper_max_order_dollars, account["cash"])
        count = max(0, int(max_order / risk)) if risk > 0 else 0
        if count <= 0:
            return
        count = min(count, 50)
        proposed_risk = risk * count
        if self._today_spend(account) + proposed_risk > self.config.paper_daily_spend_cap_dollars:
            return
        if self._open_exposure(account) + proposed_risk > self.config.paper_max_open_exposure_dollars:
            return
        account["orders_attempted"] += 1
        account["fills"] += 1
        account["gross_spend"] += proposed_risk
        account["cash"] -= proposed_risk
        self._add_daily_spend(account, proposed_risk)
        account["positions"].append(
            {
                "id": uuid.uuid4().hex,
                "ticker": candidate["ticker"],
                "side": candidate["side"],
                "entry_price": candidate["price"],
                "count": float(count),
                "risk": proposed_risk,
                "fees": 0.0,
                "opened_cycle": self.store.state["cycle"],
                "strategy_id": strategy_id,
                "edge": candidate["edge"],
            }
        )
        self._update_account_drawdown(account)

    def _active_execute(self, candidate: Mapping[str, Any], strategy_id: str) -> None:
        account = self.store.state["active"]
        if any(position["ticker"] == candidate["ticker"] for position in account["positions"]):
            return
        risk_per_contract = float(candidate["risk_per_contract"])
        count = max(0, int(self.config.active_max_order_dollars / risk_per_contract))
        count = min(count, 5)
        if count <= 0:
            return
        proposed_risk = risk_per_contract * count
        if proposed_risk > self.config.active_max_order_dollars + 1e-9:
            return
        if self._today_spend(account) + proposed_risk > self.config.active_daily_spend_cap_dollars:
            return
        if self._open_exposure(account) + proposed_risk > self.config.active_max_open_exposure_dollars:
            return
        if self._open_exposure(account) + proposed_risk > self.config.active_bankroll_cap_dollars:
            return

        account["orders_attempted"] += 1
        response = self.client.create_order(
            ticker=str(candidate["ticker"]),
            side=str(candidate["side"]),
            price=float(candidate["price"]),
            count=float(count),
        )
        fill_count = float(response.get("fill_count") or 0.0)
        if fill_count <= 0:
            return
        fill_price = float(response.get("average_fill_price") or candidate["price"])
        fee_per_contract = float(response.get("average_fee_paid") or 0.0)
        fees = fee_per_contract * fill_count
        actual_risk = (
            fill_price * fill_count
            if candidate["side"] == "bid"
            else (1.0 - fill_price) * fill_count
        )
        spend = actual_risk + fees
        if spend > self.config.active_max_order_dollars + max(0.01, fees):
            raise KalshiBotError("Kalshi fill exceeded configured active max-order risk")
        account["fills"] += 1
        account["gross_spend"] += spend
        account["fees"] += fees
        self._add_daily_spend(account, spend)
        account["positions"].append(
            {
                "id": str(response.get("order_id") or uuid.uuid4().hex),
                "client_order_id": str(response.get("client_order_id") or ""),
                "ticker": candidate["ticker"],
                "side": candidate["side"],
                "entry_price": fill_price,
                "count": fill_count,
                "risk": actual_risk,
                "fees": fees,
                "opened_cycle": self.store.state["cycle"],
                "strategy_id": strategy_id,
                "edge": candidate["edge"],
            }
        )

    def _settle_account_positions(
        self,
        account: dict[str, Any],
        markets: list[dict[str, Any]],
        mode: str,
    ) -> None:
        by_ticker = {str(item.get("ticker", "")): item for item in markets}
        kept = []
        for position in account["positions"]:
            market = by_ticker.get(position["ticker"])
            if market is None:
                try:
                    market = self.client.get_market(position["ticker"])
                except Exception:
                    kept.append(position)
                    continue
            status = str(market.get("status", "")).lower()
            result = str(market.get("result", "")).lower()
            if status == "settled" and result in {"yes", "no"}:
                self._close_position(account, position, _settlement_pnl(position, result), mode)
                continue
            if mode == MODE_PAPER:
                hold_cycles = self._strategy_hold_cycles(position.get("strategy_id"))
                if self.store.state["cycle"] - int(position["opened_cycle"]) >= hold_cycles:
                    self._close_position(
                        account, position, _mark_to_market_pnl(position, market), mode
                    )
                    continue
            kept.append(position)
        account["positions"] = kept
        self._update_account_drawdown(account)

    def _close_position(
        self,
        account: dict[str, Any],
        position: Mapping[str, Any],
        pnl: float,
        mode: str,
    ) -> None:
        account["realized_pnl"] += pnl
        account["closed_trades"] += 1
        if pnl > 1e-9:
            account["wins"] += 1
        elif pnl < -1e-9:
            account["losses"] += 1
        else:
            account["pushes"] += 1
        if mode == MODE_PAPER:
            account["cash"] += float(position.get("risk", 0.0)) + pnl
        trade = dict(position)
        trade["closed_at_cycle"] = self.store.state["cycle"]
        trade["pnl"] = pnl
        account["trades"].append(trade)
        account["trades"] = account["trades"][-2000:]

    def _strategy_hold_cycles(self, strategy_id: object) -> int:
        for member in self.store.state["evolution"]["population"]:
            if member["id"] == strategy_id:
                return max(1, int(round(float(member["genes"]["hold_cycles"]))))
        return 4

    def _sync_active_balance(self) -> None:
        payload = self.client.get_balance()
        raw = payload.get("balance")
        if raw is None:
            return
        try:
            balance = float(raw)
        except (TypeError, ValueError):
            return
        account = self.store.state["active"]
        account["reported_balance"] = balance
        account["balance_snapshots"].append(
            {"at": time.time(), "balance": balance}
        )
        account["balance_snapshots"] = account["balance_snapshots"][-500:]

    def _active_armed(self) -> bool:
        return (
            self.config.mode == MODE_ACTIVE
            and self.config.active_enabled
            and self.client.has_credentials
        )

    @staticmethod
    def _today_spend(account: Mapping[str, Any]) -> float:
        return float(account.get("daily_spend", {}).get(time.strftime("%Y-%m-%d"), 0.0))

    @staticmethod
    def _add_daily_spend(account: dict[str, Any], amount: float) -> None:
        key = time.strftime("%Y-%m-%d")
        daily = account.setdefault("daily_spend", {})
        daily[key] = float(daily.get(key, 0.0)) + amount
        for old_key in sorted(daily)[:-31]:
            daily.pop(old_key, None)

    @staticmethod
    def _open_exposure(account: Mapping[str, Any]) -> float:
        return sum(float(position.get("risk", 0.0)) for position in account.get("positions", []))

    def _update_account_drawdown(self, account: dict[str, Any]) -> None:
        equity = (
            float(account["cash"]) + self._open_exposure(account)
            if account is self.store.state["paper"]
            else float(account["bankroll"]) + float(account["realized_pnl"])
        )
        account["peak_equity"] = max(float(account.get("peak_equity", equity)), equity)
        account["max_drawdown"] = max(
            float(account.get("max_drawdown", 0.0)),
            float(account["peak_equity"]) - equity,
        )


def _stats_summary(account: Mapping[str, Any]) -> dict[str, Any]:
    closed = int(account.get("closed_trades", 0))
    wins = int(account.get("wins", 0))
    bankroll = float(account.get("bankroll", 0.0))
    realized = float(account.get("realized_pnl", 0.0))
    return {
        "bankroll": bankroll,
        "cash": account.get("cash"),
        "reported_balance": account.get("reported_balance"),
        "orders_attempted": int(account.get("orders_attempted", 0)),
        "fills": int(account.get("fills", 0)),
        "open_positions": len(account.get("positions", [])),
        "open_exposure": sum(
            float(position.get("risk", 0.0)) for position in account.get("positions", [])
        ),
        "closed_trades": closed,
        "wins": wins,
        "losses": int(account.get("losses", 0)),
        "pushes": int(account.get("pushes", 0)),
        "win_rate": wins / closed if closed else 0.0,
        "gross_spend": float(account.get("gross_spend", 0.0)),
        "today_spend": float(
            account.get("daily_spend", {}).get(time.strftime("%Y-%m-%d"), 0.0)
        ),
        "realized_pnl": realized,
        "fees": float(account.get("fees", 0.0)),
        "roi": realized / bankroll if bankroll else 0.0,
        "max_drawdown": float(account.get("max_drawdown", 0.0)),
    }


class KalshiBotPlugin(ServicePlugin):
    enabled_by_default = False
    metadata = PluginMetadata(
        name="kalshi-bot",
        version="0.1.0",
        api_version="1",
        description=(
            "Evolutionary Kalshi paper/active bot with persistent stats and hard active-mode risk caps."
        ),
    )

    def __init__(self) -> None:
        self._runtime = None
        self._config: Optional[KalshiBotConfig] = None
        self._store: Optional[KalshiStateStore] = None
        self._client: Optional[KalshiClient] = None
        self._engine: Optional[KalshiBotEngine] = None

    def start(self, runtime) -> None:
        self._runtime = runtime
        self._config = KalshiBotConfig.from_mapping(runtime.configuration)
        self._store = KalshiStateStore(self._config.resolved_state_path(), self._config)
        self._client = KalshiClient(self._config)
        self._engine = KalshiBotEngine(self._config, self._store, self._client)
        if self._config.auto_run:
            runtime.start_worker("strategy-loop", self._run_loop)

    def stop(self) -> None:
        self._runtime = None
        self._engine = None
        self._client = None
        self._store = None
        self._config = None

    def tools(self):
        def kalshi_bot_status() -> str:
            return json.dumps(self._require_engine().status(), sort_keys=True)

        def kalshi_bot_stats() -> str:
            status = self._require_engine().status()
            return json.dumps(
                {
                    "mode": status["mode"],
                    "paper": status["paper_stats"],
                    "active": status["active_stats"],
                    "generation": status["generation"],
                    "champion": status["champion"],
                },
                sort_keys=True,
            )

        return (
            StructuredTool.from_function(
                kalshi_bot_status,
                name="kalshi_bot_status",
                description=(
                    "Show Kalshi bot mode, arming state, evolution generation, champion strategy, "
                    "and complete paper/active performance summaries. This tool never places orders."
                ),
            ),
            StructuredTool.from_function(
                kalshi_bot_stats,
                name="kalshi_bot_stats",
                description=(
                    "Show persistent paper and active Kalshi trading statistics. This tool is read-only."
                ),
            ),
        )

    def _run_loop(self, stop_event: threading.Event) -> None:
        config = self._require_config()
        while not stop_event.is_set():
            try:
                self._require_engine().step()
            except Exception:
                # Transient market/auth failures are persisted by the engine. They must not
                # tear down the entire plugin worker and erase long-running paper statistics.
                pass
            stop_event.wait(config.poll_seconds)

    def _require_engine(self) -> KalshiBotEngine:
        if self._engine is None:
            raise RuntimeError("kalshi-bot plugin is not running")
        return self._engine

    def _require_config(self) -> KalshiBotConfig:
        if self._config is None:
            raise RuntimeError("kalshi-bot plugin is not running")
        return self._config


def create_plugin():
    return KalshiBotPlugin()
