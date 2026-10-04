"""Private desktop health database and explicit Org projection commands."""

from __future__ import annotations

import argparse
import os
import sys
import time
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Any, Mapping, Sequence

from .health_store import (
    GOAL_METRICS,
    GOAL_PERIODS,
    ORG_EXPORTABLE_METRICS,
    GpgCipher,
    HealthGoal,
    HealthStoreError,
    PrologHealthStore,
    render_private_org_health_summary,
    write_org_health_summary,
)


def main(
    argv: Sequence[str] | None = None,
    *,
    config: Any = None,
    data_home: Path | None = None,
) -> int:
    parser = _parser()
    args = parser.parse_args(argv)
    if config is None:
        from .config import init_config

        config = init_config()
    health_config = config.get_section("health")
    try:
        cipher = _cipher(health_config)
        encrypted = bool(health_config.get("gpg_enabled", False))
        root = _data_root(data_home)
        filename = "health.pl.gpg" if encrypted else "health.pl"
        store = PrologHealthStore(root / "zarathushtra" / "health" / filename, cipher if encrypted else None)

        if args.command == "status":
            snapshot = store.load()
            print(f"OpenPGP: {'enabled' if encrypted else 'disabled'}")
            print(f"Observations: {len(snapshot.observations)}")
            print(f"Goals: {len(snapshot.goals)}")
            return 0

        if args.command == "set-goal":
            snapshot = store.load()
            try:
                target = Decimal(args.target)
            except InvalidOperation as error:
                raise ValueError("goal target must be a decimal number") from error
            goal = HealthGoal(
                principal=args.principal,
                metric=args.metric,
                target=target,
                unit=args.unit,
                period=args.period,
                updated_epoch_ms=int(time.time() * 1000),
            )
            goals = [
                item
                for item in snapshot.goals
                if (item.principal, item.metric, item.period)
                != (goal.principal, goal.metric, goal.period)
            ]
            store.replace(snapshot.observations, [*goals, goal])
            print(f"Updated {goal.metric} goal in the private health database.")
            return 0

        if args.command == "export-org":
            snapshot = store.load()
            content = render_private_org_health_summary(
                snapshot.observations,
                snapshot.goals,
                approved_metrics=set(args.metric),
            )
            export_cipher = cipher if args.gpg or encrypted else None
            if export_cipher is None and args.gpg:
                raise ValueError("At least one health.gpg_recipients recipient is required")
            write_org_health_summary(Path(args.destination), content, export_cipher)
            print("Wrote an encrypted private Org summary." if export_cipher else "Wrote a private Org summary.")
            return 0
    except (HealthStoreError, OSError, ValueError) as error:
        print(f"Health command failed: {error}", file=sys.stderr)
        return 2
    parser.error("unknown health command")
    return 2


def _parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(prog="zara health", description="Manage Zara's private Prolog health store")
    commands = parser.add_subparsers(dest="command", required=True)
    commands.add_parser("status", help="Show privacy mode and record counts")

    goal = commands.add_parser("set-goal", help="Set a private health goal")
    goal.add_argument("metric", choices=sorted(GOAL_METRICS))
    goal.add_argument("target")
    goal.add_argument("--unit", required=True)
    goal.add_argument("--period", choices=sorted(GOAL_PERIODS), default="daily")
    goal.add_argument("--principal", default="local:owner")

    export = commands.add_parser("export-org", help="Write an explicitly selected private Org summary")
    export.add_argument("destination")
    export.add_argument("--metric", action="append", choices=sorted(ORG_EXPORTABLE_METRICS), required=True)
    export.add_argument("--gpg", action="store_true", help="Require OpenPGP encryption for this export")
    return parser


def _cipher(config: Mapping[str, Any]) -> GpgCipher | None:
    recipients = config.get("gpg_recipients", [])
    enabled = bool(config.get("gpg_enabled", False))
    if not enabled and not recipients:
        return None
    if not recipients:
        raise ValueError("At least one health.gpg_recipients recipient is required")
    homedir = config.get("gpg_homedir", "")
    return GpgCipher(recipients, homedir=Path(homedir).expanduser() if homedir else None)


def _data_root(value: Path | None) -> Path:
    if value is not None:
        return Path(value)
    configured = os.getenv("XDG_DATA_HOME")
    return Path(configured).expanduser() if configured else Path.home() / ".local" / "share"
