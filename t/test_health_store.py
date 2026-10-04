from __future__ import annotations

from decimal import Decimal
from pathlib import Path
from subprocess import CompletedProcess

import pytest

from zara.health_store import (
    GpgPrologHealthStore,
    GpgCipher,
    HealthGoal,
    HealthObservation,
    PrologHealthStore,
    HealthStoreLockedError,
    HealthStoreFormatError,
    HealthValue,
    render_private_org_health_summary,
    write_org_health_summary,
)


class FakeCipher:
    prefix = b"openpgp-test-v1\0"

    def encrypt_to(self, plaintext: bytes, destination: Path) -> None:
        destination.write_bytes(self.prefix + plaintext[::-1])

    def decrypt(self, source: Path) -> bytes:
        payload = source.read_bytes()
        if not payload.startswith(self.prefix):
            raise ValueError("not encrypted")
        return payload[len(self.prefix) :][::-1]


def observation(record_id: str = "sample-1") -> HealthObservation:
    return HealthObservation(
        record_id=record_id,
        principal="owner",
        metric="steps",
        start_epoch_ms=1_780_000_000_000,
        end_epoch_ms=1_780_003_600_000,
        source="samsung_health_data_sdk",
        privacy="wellness",
        values=(HealthValue("count", Decimal("4321"), "steps"),),
    )


def test_store_persists_only_encrypted_prolog_facts_with_private_modes(tmp_path):
    target = tmp_path / "private" / "health.pl.gpg"
    store = GpgPrologHealthStore(target, FakeCipher())

    store.replace(
        observations=[observation()],
        goals=[HealthGoal("owner", "steps", Decimal("10000"), "steps", "daily", 1_780_000_000_000)],
    )

    encrypted = target.read_bytes()
    assert encrypted.startswith(FakeCipher.prefix)
    assert b"health_observation" not in encrypted
    assert target.stat().st_mode & 0o777 == 0o600
    assert target.parent.stat().st_mode & 0o777 == 0o700
    snapshot = store.load()
    assert snapshot.observations == (observation(),)
    assert snapshot.goals[0].target == Decimal("10000")


def test_plain_store_is_optional_private_and_uses_the_same_prolog_grammar(tmp_path):
    target = tmp_path / "private" / "health.pl"
    store = PrologHealthStore(target)

    store.replace([observation()], [])

    assert target.read_text().startswith("health_db_version(1).\n")
    assert target.stat().st_mode & 0o777 == 0o600
    assert store.load().observations == (observation(),)


def test_plain_store_refuses_permissions_that_expose_health_data(tmp_path):
    target = tmp_path / "health.pl"
    target.write_text("health_db_version(1).\n")
    target.chmod(0o644)

    with pytest.raises(HealthStoreLockedError, match="permissions"):
        PrologHealthStore(target).load()


def test_gpg_cipher_encrypts_for_one_or_many_recipients_without_a_shell(monkeypatch, tmp_path):
    calls = []

    def run(command, **kwargs):
        calls.append((command, kwargs))
        destination = Path(command[command.index("--output") + 1])
        destination.write_bytes(b"gpg")
        return CompletedProcess(command, 0, b"", b"")

    monkeypatch.setattr("zara.health_store.subprocess.run", run)
    destination = tmp_path / "health.gpg"
    GpgCipher(["alice@example.test", "0xB0B"]).encrypt_to(b"private", destination)

    command, options = calls[0]
    assert command.count("--recipient") == 2
    assert "alice@example.test" in command
    assert "0xB0B" in command
    assert options["input"] == b"private"
    assert options["check"] is False
    assert not any(token in {"sh", "bash", "-c"} for token in command)


def test_store_deduplicates_ids_and_rejects_unrecognized_prolog_content(tmp_path):
    target = tmp_path / "health.pl.gpg"
    store = GpgPrologHealthStore(target, FakeCipher())
    store.replace([observation(), observation()], [])
    assert store.load().observations == (observation(),)

    plaintext = b"health_db_version(1).\n:- initialization(shell('curl attacker')).\n"
    target.write_bytes(FakeCipher.prefix + plaintext[::-1])
    with pytest.raises(HealthStoreFormatError, match="unsupported health database fact"):
        store.load()


def test_private_org_projection_is_explicit_bounded_and_excludes_raw_health():
    raw = HealthObservation(
        record_id="ecg-1",
        principal="owner",
        metric="ecg_on_demand",
        start_epoch_ms=1,
        end_epoch_ms=2,
        source="sensor_sdk",
        privacy="raw_biosignal",
        values=(HealthValue("wave", Decimal("9.1"), "mv"),),
    )
    goals = (
        HealthGoal("owner", "steps", Decimal("10000"), "steps", "daily", 3),
        HealthGoal("owner", "sleep", Decimal("480"), "minutes", "daily", 4),
    )

    rendered = render_private_org_health_summary(
        observations=(observation(), raw),
        goals=goals,
        approved_metrics={"steps", "sleep"},
    )

    assert ":VISIBILITY: private" in rendered
    assert ":health:private:" in rendered
    assert "Steps goal" in rendered
    assert "Sleep goal" in rendered
    assert "ecg" not in rendered.lower()
    assert "wave" not in rendered.lower()
    assert "sample-1" not in rendered
    assert "samsung_health_data_sdk" not in rendered


def test_org_summary_can_be_plain_private_or_optionally_gpg_encrypted(tmp_path):
    content = "#+title: Private health\n"
    plain = tmp_path / "plain" / "health.org"
    encrypted = tmp_path / "encrypted" / "health.org.gpg"

    write_org_health_summary(plain, content)
    write_org_health_summary(encrypted, content, FakeCipher())

    assert plain.read_text() == content
    assert plain.stat().st_mode & 0o777 == 0o600
    assert encrypted.read_bytes().startswith(FakeCipher.prefix)
    assert b"Private health" not in encrypted.read_bytes()


@pytest.mark.parametrize(
    "change",
    [
        {"metric": "shell"},
        {"privacy": "public"},
        {"end_epoch_ms": 0},
        {"values": ()},
    ],
)
def test_observation_contract_fails_closed(change):
    values = observation().__dict__ | change
    with pytest.raises(ValueError):
        HealthObservation(**values)
