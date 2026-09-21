"""Receive-only donation campaign data and USD aggregation."""

from __future__ import annotations

import json
import os
import re
from dataclasses import dataclass
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any, Mapping, Sequence


DONATION_DOCUMENT_VERSION = "ZARA-DONATIONS/1"
MAX_USD = Decimal("1000000000000.00")
_CAMPAIGN_ID = re.compile(r"^[a-z0-9][a-z0-9_-]{0,63}$")


class DonationConfigError(ValueError):
    """Raised when a donation document is malformed or unsafe to display."""


def donation_config_path() -> Path:
    root = Path(os.environ.get("XDG_CONFIG_HOME", "~/.config")).expanduser()
    return root / "zarathushtra" / "donations.json"


def _bounded_text(value: Any, label: str, *, maximum: int) -> str:
    if not isinstance(value, str):
        raise DonationConfigError(f"{label} must be a string")
    text = value.strip()
    if not text:
        raise DonationConfigError(f"{label} must not be empty")
    if "\x00" in text:
        raise DonationConfigError(f"{label} must not contain NUL")
    if len(text) > maximum:
        raise DonationConfigError(f"{label} exceeds {maximum} characters")
    return text


def _usd(value: Any, label: str) -> Decimal:
    if isinstance(value, bool) or value is None:
        raise DonationConfigError(f"{label} must be a USD amount")
    try:
        amount = value if isinstance(value, Decimal) else Decimal(str(value))
    except (InvalidOperation, ValueError) as error:
        raise DonationConfigError(f"{label} must be a USD amount") from error
    if not amount.is_finite():
        raise DonationConfigError(f"{label} must be finite")
    if amount < 0:
        raise DonationConfigError(f"{label} must not be negative")
    if amount > MAX_USD:
        raise DonationConfigError(f"{label} exceeds supported maximum")
    if amount.as_tuple().exponent < -2:
        raise DonationConfigError(f"{label} must use at most two decimal places")
    return amount.quantize(Decimal("0.01"))


def _usd_text(value: Decimal) -> str:
    return format(value, ".2f")


@dataclass(frozen=True)
class DonationWallet:
    chain: str
    network: str
    asset: str
    address: str
    label: str | None = None

    @classmethod
    def from_mapping(cls, value: Mapping[str, Any]) -> "DonationWallet":
        if not isinstance(value, Mapping):
            raise DonationConfigError("wallet must be an object")
        label = value.get("label")
        if label is not None:
            label = _bounded_text(label, "wallet label", maximum=80)
        return cls(
            chain=_bounded_text(value.get("chain"), "wallet chain", maximum=40),
            network=_bounded_text(value.get("network"), "wallet network", maximum=40),
            asset=_bounded_text(value.get("asset"), "wallet asset", maximum=24),
            address=_bounded_text(value.get("address"), "wallet address", maximum=256),
            label=label,
        )

    def to_mapping(self) -> dict[str, Any]:
        result: dict[str, Any] = {
            "chain": self.chain,
            "network": self.network,
            "asset": self.asset,
            "address": self.address,
        }
        if self.label is not None:
            result["label"] = self.label
        return result


@dataclass(frozen=True)
class DonationCampaign:
    campaign_id: str
    title: str
    goal_usd: Decimal
    raised_usd: Decimal
    active: bool
    wallets: tuple[DonationWallet, ...]

    @property
    def remaining_usd(self) -> Decimal:
        return max(self.goal_usd - self.raised_usd, Decimal("0.00"))

    @classmethod
    def from_mapping(cls, value: Mapping[str, Any]) -> "DonationCampaign":
        if not isinstance(value, Mapping):
            raise DonationConfigError("campaign must be an object")
        campaign_id = _bounded_text(value.get("id"), "campaign id", maximum=64)
        if not _CAMPAIGN_ID.fullmatch(campaign_id):
            raise DonationConfigError(
                "campaign id must match [a-z0-9][a-z0-9_-]{0,63}"
            )
        active = value.get("active", True)
        if not isinstance(active, bool):
            raise DonationConfigError("campaign active must be boolean")
        raw_wallets = value.get("wallets", [])
        if not isinstance(raw_wallets, Sequence) or isinstance(
            raw_wallets, (str, bytes, bytearray)
        ):
            raise DonationConfigError("campaign wallets must be an array")
        wallets = tuple(DonationWallet.from_mapping(item) for item in raw_wallets)
        return cls(
            campaign_id=campaign_id,
            title=_bounded_text(value.get("title"), "campaign title", maximum=120),
            goal_usd=_usd(value.get("goal_usd", "0"), "campaign goal_usd"),
            raised_usd=_usd(value.get("raised_usd", "0"), "campaign raised_usd"),
            active=active,
            wallets=wallets,
        )

    def to_mapping(self) -> dict[str, Any]:
        return {
            "id": self.campaign_id,
            "title": self.title,
            "goal_usd": _usd_text(self.goal_usd),
            "raised_usd": _usd_text(self.raised_usd),
            "remaining_usd": _usd_text(self.remaining_usd),
            "active": self.active,
            "wallets": [wallet.to_mapping() for wallet in self.wallets],
        }


@dataclass(frozen=True)
class DonationLedger:
    campaigns: tuple[DonationCampaign, ...] = ()

    @property
    def total_goal_usd(self) -> Decimal:
        return sum((campaign.goal_usd for campaign in self.campaigns), Decimal("0.00"))

    @property
    def total_raised_usd(self) -> Decimal:
        return sum((campaign.raised_usd for campaign in self.campaigns), Decimal("0.00"))

    @property
    def total_remaining_usd(self) -> Decimal:
        return sum((campaign.remaining_usd for campaign in self.campaigns), Decimal("0.00"))

    @classmethod
    def from_mapping(cls, value: Mapping[str, Any]) -> "DonationLedger":
        if not isinstance(value, Mapping):
            raise DonationConfigError("donation document must be an object")
        if value.get("version") != DONATION_DOCUMENT_VERSION:
            raise DonationConfigError(
                f"version must be {DONATION_DOCUMENT_VERSION}"
            )
        raw_campaigns = value.get("campaigns", [])
        if not isinstance(raw_campaigns, Sequence) or isinstance(
            raw_campaigns, (str, bytes, bytearray)
        ):
            raise DonationConfigError("campaigns must be an array")
        campaigns = tuple(DonationCampaign.from_mapping(item) for item in raw_campaigns)
        ids = [campaign.campaign_id for campaign in campaigns]
        if len(ids) != len(set(ids)):
            raise DonationConfigError("campaign ids must be unique")
        return cls(campaigns=campaigns)

    @classmethod
    def from_json(cls, payload: str) -> "DonationLedger":
        try:
            value = json.loads(payload, parse_float=Decimal, parse_int=Decimal)
        except (json.JSONDecodeError, InvalidOperation) as error:
            raise DonationConfigError("donation document is not valid JSON") from error
        return cls.from_mapping(value)

    @classmethod
    def load(cls, path: Path | str | None = None) -> "DonationLedger":
        source = donation_config_path() if path is None else Path(path).expanduser()
        try:
            payload = source.read_text(encoding="utf-8")
        except FileNotFoundError:
            return cls()
        except OSError as error:
            raise DonationConfigError(f"cannot read donation document: {error}") from error
        return cls.from_json(payload)

    def to_mapping(self) -> dict[str, Any]:
        return {
            "version": DONATION_DOCUMENT_VERSION,
            "summary": {
                "campaign_count": len(self.campaigns),
                "active_campaign_count": sum(1 for campaign in self.campaigns if campaign.active),
                "goal_usd": _usd_text(self.total_goal_usd),
                "raised_usd": _usd_text(self.total_raised_usd),
                "remaining_usd": _usd_text(self.total_remaining_usd),
            },
            "campaigns": [campaign.to_mapping() for campaign in self.campaigns],
        }

    def to_json(self) -> str:
        return json.dumps(
            self.to_mapping(),
            ensure_ascii=False,
            separators=(",", ":"),
            sort_keys=True,
        )
