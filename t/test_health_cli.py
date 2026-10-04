from __future__ import annotations

from decimal import Decimal

from zara.health_cli import main
from zara.health_store import HealthSnapshot, PrologHealthStore


class Config:
    def __init__(self, health):
        self.health = health

    def get_section(self, section):
        assert section == "health"
        return self.health


def test_health_cli_sets_goal_and_reports_private_store_without_dumping_values(tmp_path, capsys):
    config = Config({"gpg_enabled": False, "gpg_recipients": [], "gpg_homedir": ""})

    assert main(
        ["set-goal", "steps", "12000", "--unit", "steps"],
        config=config,
        data_home=tmp_path,
    ) == 0
    assert main(["status"], config=config, data_home=tmp_path) == 0

    output = capsys.readouterr().out
    assert "OpenPGP: disabled" in output
    assert "Goals: 1" in output
    assert "12000" not in output
    snapshot = PrologHealthStore(tmp_path / "zarathushtra" / "health" / "health.pl").load()
    assert snapshot.goals[0].target == Decimal("12000")


def test_health_cli_exports_only_explicitly_approved_org_metrics(tmp_path):
    config = Config({"gpg_enabled": False, "gpg_recipients": [], "gpg_homedir": ""})
    store = PrologHealthStore(tmp_path / "zarathushtra" / "health" / "health.pl")
    store.replace([], [])
    destination = tmp_path / "health.org"

    assert main(
        ["export-org", str(destination), "--metric", "steps"],
        config=config,
        data_home=tmp_path,
    ) == 0

    assert destination.stat().st_mode & 0o777 == 0o600
    assert ":VISIBILITY: private" in destination.read_text()


def test_health_cli_rejects_gpg_export_when_no_recipient_is_configured(tmp_path, capsys):
    config = Config({"gpg_enabled": False, "gpg_recipients": [], "gpg_homedir": ""})

    result = main(
        ["export-org", str(tmp_path / "health.org.gpg"), "--metric", "steps", "--gpg"],
        config=config,
        data_home=tmp_path,
    )

    assert result == 2
    assert "recipient" in capsys.readouterr().err.lower()
