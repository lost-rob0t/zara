from __future__ import annotations

from pathlib import Path

from zara.plugins.builtin.kalshi_bot import (
    KalshiBotConfig,
    KalshiBotEngine,
    KalshiClient,
    KalshiStateStore,
)


def market(
    *,
    ticker: str = "TEST-MARKET",
    bid: float = 0.45,
    ask: float = 0.46,
    last: float = 0.55,
    previous: float = 0.45,
    status: str = "open",
    result: str = "",
):
    return {
        "ticker": ticker,
        "title": "Synthetic deterministic test market",
        "status": status,
        "result": result,
        "yes_bid_dollars": f"{bid:.4f}",
        "yes_ask_dollars": f"{ask:.4f}",
        "last_price_dollars": f"{last:.4f}",
        "previous_price_dollars": f"{previous:.4f}",
        "liquidity_dollars": "10000.0000",
        "volume_24h_fp": "5000.00",
    }


class FakeClient:
    def __init__(self, markets, *, credentials=True):
        self.markets = list(markets)
        self.credentials = credentials
        self.orders = []

    @property
    def has_credentials(self):
        return self.credentials

    def list_markets(self):
        return [dict(item) for item in self.markets]

    def get_market(self, ticker):
        for item in self.markets:
            if item["ticker"] == ticker:
                return dict(item)
        return market(ticker=ticker, status="settled", result="yes")

    def get_balance(self):
        return {"balance": "12.3400"}

    def create_order(self, **kwargs):
        self.orders.append(dict(kwargs))
        return {
            "order_id": f"order-{len(self.orders)}",
            "client_order_id": f"client-{len(self.orders)}",
            "fill_count": f"{kwargs['count']:.2f}",
            "remaining_count": "0.00",
            "average_fill_price": f"{kwargs['price']:.4f}",
            "average_fee_paid": "0.0100",
            "ts_ms": 1,
        }


def aggressive_population(store: KalshiStateStore, *, hold_cycles: float = 1.0):
    for member in store.state["evolution"]["population"]:
        member["genes"].update(
            {
                "momentum_weight": 2.0,
                "liquidity_weight": 0.0,
                "volume_weight": 0.0,
                "spread_penalty": 0.0,
                "bias": 0.05,
                "min_edge": 0.005,
                "max_contract_risk": 0.95,
                "hold_cycles": hold_cycles,
            }
        )


def engine(tmp_path: Path, config: KalshiBotConfig, client: FakeClient):
    store = KalshiStateStore(tmp_path / "kalshi.json", config)
    aggressive_population(store)
    return KalshiBotEngine(config, store, client), store


def test_default_active_limits_are_far_smaller_than_paper():
    config = KalshiBotConfig.from_mapping({})
    assert config.mode == "paper"
    assert config.paper_bankroll_dollars == 1000.0
    assert config.paper_max_order_dollars == 10.0
    assert config.active_bankroll_cap_dollars == 25.0
    assert config.active_max_order_dollars == 1.0
    assert config.active_daily_spend_cap_dollars == 5.0
    assert config.active_max_open_exposure_dollars == 10.0
    assert config.active_bankroll_cap_dollars < config.paper_bankroll_dollars
    assert config.active_max_order_dollars < config.paper_max_order_dollars


def test_paper_mode_tracks_positions_pnl_win_rate_and_drawdown(tmp_path):
    config = KalshiBotConfig.from_mapping(
        {
            "mode": "paper",
            "state_path": str(tmp_path / "kalshi.json"),
            "population_size": 4,
            "evolution_cycles_per_generation": 50,
        }
    )
    client = FakeClient([market()])
    bot, store = engine(tmp_path, config, client)

    bot.step()
    first = bot.status()["paper_stats"]
    assert first["orders_attempted"] == 1
    assert first["fills"] == 1
    assert first["open_positions"] == 1
    assert first["gross_spend"] > 0

    client.markets = [market(bid=0.60, ask=0.61, last=0.62, previous=0.55)]
    bot.step()
    stats = bot.status()["paper_stats"]
    assert stats["closed_trades"] >= 1
    assert stats["wins"] >= 1
    assert stats["realized_pnl"] > 0
    assert stats["win_rate"] > 0
    assert stats["max_drawdown"] >= 0
    assert store.path.exists()


def test_active_mode_is_not_armed_without_explicit_active_enabled(tmp_path):
    config = KalshiBotConfig.from_mapping(
        {
            "mode": "active",
            "active_enabled": False,
            "state_path": str(tmp_path / "kalshi.json"),
            "population_size": 4,
        }
    )
    client = FakeClient([market()], credentials=True)
    bot, _ = engine(tmp_path, config, client)
    bot.step()
    assert client.orders == []
    assert bot.status()["active_armed"] is False


def test_active_mode_tracks_live_stats_under_small_caps(tmp_path):
    config = KalshiBotConfig.from_mapping(
        {
            "mode": "active",
            "active_enabled": True,
            "state_path": str(tmp_path / "kalshi.json"),
            "population_size": 4,
            "active_bankroll_cap_dollars": 25.0,
            "active_max_order_dollars": 1.0,
            "active_daily_spend_cap_dollars": 5.0,
            "active_max_open_exposure_dollars": 10.0,
        }
    )
    client = FakeClient([market()], credentials=True)
    bot, _ = engine(tmp_path, config, client)
    bot.step()

    assert len(client.orders) == 1
    order = client.orders[0]
    assert order["count"] * (
        order["price"] if order["side"] == "bid" else 1.0 - order["price"]
    ) <= config.active_max_order_dollars + 1e-9

    status = bot.status()
    stats = status["active_stats"]
    assert status["active_armed"] is True
    assert stats["orders_attempted"] == 1
    assert stats["fills"] == 1
    assert stats["open_positions"] == 1
    assert stats["open_exposure"] <= config.active_max_open_exposure_dollars
    assert stats["today_spend"] <= config.active_daily_spend_cap_dollars
    assert stats["reported_balance"] == 12.34
    assert stats["fees"] > 0


def test_evolution_advances_generation_and_records_history(tmp_path):
    config = KalshiBotConfig.from_mapping(
        {
            "mode": "paper",
            "state_path": str(tmp_path / "kalshi.json"),
            "population_size": 4,
            "evolution_cycles_per_generation": 2,
            "random_seed": 99,
        }
    )
    client = FakeClient([market()])
    bot, store = engine(tmp_path, config, client)
    bot.step()
    client.markets = [market(bid=0.58, ask=0.59, last=0.61, previous=0.55)]
    bot.step()

    assert store.state["evolution"]["generation"] == 1
    assert len(store.state["evolution"]["history"]) == 1
    history = store.state["evolution"]["history"][0]
    assert "genes" in history
    assert "fitness" in history
    assert len(store.state["evolution"]["population"]) == 4


def test_client_uses_current_v2_order_shape_without_network():
    calls = []

    def request_json(method, path, body, authenticated):
        calls.append((method, path, body, authenticated))
        return {
            "order_id": "o1",
            "fill_count": "1.00",
            "remaining_count": "0.00",
            "ts_ms": 1,
        }

    config = KalshiBotConfig.from_mapping({})
    client = KalshiClient(config, request_json=request_json)
    client.create_order(ticker="ABC", side="bid", price=0.56, count=1.0)

    method, path, body, authenticated = calls[0]
    assert method == "POST"
    assert path == "/portfolio/events/orders"
    assert authenticated is True
    assert body["side"] == "bid"
    assert body["count"] == "1.00"
    assert body["price"] == "0.5600"
    assert body["time_in_force"] == "fill_or_kill"
    assert body["self_trade_prevention_type"] == "taker_at_cross"
    assert body["cancel_order_on_pause"] is True


def test_config_rejects_active_order_cap_above_daily_cap():
    try:
        KalshiBotConfig.from_mapping(
            {
                "active_max_order_dollars": 6.0,
                "active_daily_spend_cap_dollars": 5.0,
            }
        )
    except ValueError as error:
        assert "active_max_order_dollars" in str(error)
    else:
        raise AssertionError("invalid active caps were accepted")
