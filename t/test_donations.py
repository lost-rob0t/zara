from decimal import Decimal

import pytest

from zara.donations import (
    DONATION_DOCUMENT_VERSION,
    DonationConfigError,
    DonationLedger,
    donation_config_path,
)


def fixture_document():
    return {
        "version": DONATION_DOCUMENT_VERSION,
        "campaigns": [
            {
                "id": "infra",
                "title": "Infrastructure",
                "goal_usd": "1000.00",
                "raised_usd": "125.50",
                "active": True,
                "wallets": [
                    {
                        "chain": "bitcoin",
                        "network": "mainnet",
                        "asset": "BTC",
                        "address": "bc1qexample",
                        "label": "BTC",
                    }
                ],
            },
            {
                "id": "research",
                "title": "Research",
                "goal_usd": 500,
                "raised_usd": 25,
                "active": False,
                "wallets": [],
            },
        ],
    }


def test_ledger_aggregates_declared_usd_without_float_math():
    ledger = DonationLedger.from_mapping(fixture_document())

    assert ledger.total_goal_usd == Decimal("1500.00")
    assert ledger.total_raised_usd == Decimal("150.50")
    assert ledger.total_remaining_usd == Decimal("1349.50")
    assert ledger.campaigns[0].remaining_usd == Decimal("874.50")


def test_json_projection_is_stable_and_includes_wallets_and_summary():
    ledger = DonationLedger.from_mapping(fixture_document())
    payload = DonationLedger.from_json(ledger.to_json()).to_mapping()

    assert payload["version"] == DONATION_DOCUMENT_VERSION
    assert payload["summary"] == {
        "campaign_count": 2,
        "active_campaign_count": 1,
        "goal_usd": "1500.00",
        "raised_usd": "150.50",
        "remaining_usd": "1349.50",
    }
    assert payload["campaigns"][0]["wallets"][0]["address"] == "bc1qexample"


@pytest.mark.parametrize(
    "mutation, message",
    [
        (lambda doc: doc.update(version="wrong"), "version"),
        (
            lambda doc: doc["campaigns"][0].update(raised_usd="-1"),
            "must not be negative",
        ),
        (
            lambda doc: doc["campaigns"][0].update(goal_usd="1.001"),
            "two decimal places",
        ),
        (
            lambda doc: doc["campaigns"].append(dict(doc["campaigns"][0])),
            "unique",
        ),
        (
            lambda doc: doc["campaigns"][0]["wallets"][0].update(address=""),
            "must not be empty",
        ),
        (
            lambda doc: doc["campaigns"][0]["wallets"][0].update(private_key="nope"),
            "unsupported fields",
        ),
    ],
)
def test_invalid_documents_fail_closed(mutation, message):
    document = fixture_document()
    mutation(document)

    with pytest.raises(DonationConfigError, match=message):
        DonationLedger.from_mapping(document)


def test_missing_config_is_empty_and_uses_xdg(monkeypatch, tmp_path):
    monkeypatch.setenv("XDG_CONFIG_HOME", str(tmp_path))
    assert donation_config_path() == tmp_path / "zarathushtra" / "donations.json"
    assert DonationLedger.load().campaigns == ()
