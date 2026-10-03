"""Encrypted Prolog health facts and explicit private Org projections.

The canonical database is an OpenPGP-encrypted file whose plaintext is a
strict, data-only subset of Prolog.  Decrypted text is parsed rather than
consulted, so a modified database cannot smuggle directives into SWI-Prolog.
"""

from __future__ import annotations

import base64
import os
import re
import subprocess
import tempfile
from dataclasses import dataclass
from decimal import Decimal, InvalidOperation
from pathlib import Path
from typing import Iterable, Protocol


PHONE_METRICS = frozenset(
    {
        "activity_summary",
        "active_calories_burned_goal",
        "active_time_goal",
        "blood_glucose",
        "blood_oxygen",
        "blood_pressure",
        "body_composition",
        "body_temperature",
        "energy_score",
        "exercise",
        "exercise_location",
        "floors_climbed",
        "heart_rate",
        "irregular_heart_rhythm_notification",
        "nutrition",
        "nutrition_goal",
        "skin_temperature",
        "sleep",
        "sleep_apnea",
        "sleep_goal",
        "steps",
        "step_goal",
        "water_intake",
        "water_intake_goal",
        "user_profile",
    }
)
WATCH_METRICS = frozenset(
    {
        "accelerometer_continuous",
        "eda_continuous",
        "heart_rate_continuous",
        "ppg_continuous",
        "skin_temperature_continuous",
        "bia_on_demand",
        "ecg_on_demand",
        "mf_bia_on_demand",
        "ppg_on_demand",
        "skin_temperature_on_demand",
        "spo2_on_demand",
        "sweat_loss",
    }
)
HEALTH_METRICS = PHONE_METRICS | WATCH_METRICS
GOAL_METRICS = frozenset(
    {
        "active_calories_burned",
        "active_time",
        "nutrition",
        "sleep",
        "steps",
        "water_intake",
    }
)
PRIVACY_CLASSES = frozenset({"wellness", "biometric", "profile", "raw_biosignal"})
GOAL_PERIODS = frozenset({"daily", "weekly"})
ORG_EXPORTABLE_METRICS = frozenset(
    {"active_calories_burned", "active_time", "nutrition", "sleep", "steps", "water_intake"}
)

_TOKEN = r"b64_[A-Za-z0-9_-]+"
_ATOM = r"[a-z][a-z0-9_]*"
_NUMBER = r"-?(?:0|[1-9][0-9]*)(?:\.[0-9]+)?"
_OBSERVATION = re.compile(
    rf"^health_observation\((?P<id>{_TOKEN}),(?P<principal>{_TOKEN}),"
    rf"(?P<metric>{_ATOM}),(?P<start>[0-9]+),(?P<end>[0-9]+),"
    rf"(?P<source>{_TOKEN}),(?P<privacy>{_ATOM}),\[(?P<values>.*)\]\)\.$"
)
_VALUE = re.compile(
    rf"^health_value\((?P<name>{_TOKEN}),(?P<value>{_NUMBER}),(?P<unit>{_TOKEN})\)$"
)
_GOAL = re.compile(
    rf"^health_goal\((?P<principal>{_TOKEN}),(?P<metric>{_ATOM}),"
    rf"(?P<target>{_NUMBER}),(?P<unit>{_TOKEN}),(?P<period>{_ATOM}),"
    rf"(?P<updated>[0-9]+)\)\.$"
)
_VERSION_FACT = "health_db_version(1)."
_MAX_DATABASE_BYTES = 16 * 1024 * 1024
_MAX_FACTS = 250_000


class HealthStoreError(RuntimeError):
    pass


class HealthStoreLockedError(HealthStoreError):
    pass


class HealthStoreFormatError(HealthStoreError):
    pass


class HealthCipher(Protocol):
    def encrypt_to(self, plaintext: bytes, destination: Path) -> None: ...

    def decrypt(self, source: Path) -> bytes: ...


class GpgCipher:
    """Small argv-only GPG boundary; plaintext is passed over stdin/stdout."""

    def __init__(
        self,
        recipients: str | Iterable[str],
        executable: str = "gpg",
        homedir: Path | None = None,
    ) -> None:
        values = [recipients] if isinstance(recipients, str) else list(recipients)
        self.recipients = tuple(dict.fromkeys(_bounded_text("recipient", value, 256) for value in values))
        if not self.recipients:
            raise ValueError("at least one GPG recipient is required")
        if len(self.recipients) > 32:
            raise ValueError("at most 32 GPG recipients are supported")
        self.executable = executable
        self.homedir = Path(homedir) if homedir is not None else None

    def encrypt_to(self, plaintext: bytes, destination: Path) -> None:
        command = self._base_command() + [
            "--yes",
            "--trust-model",
            "always",
        ]
        for recipient in self.recipients:
            command.extend(["--recipient", recipient])
        command.extend(["--output", str(destination), "--encrypt"])
        result = subprocess.run(
            command,
            input=plaintext,
            stdout=subprocess.DEVNULL,
            stderr=subprocess.PIPE,
            check=False,
        )
        if result.returncode != 0:
            raise HealthStoreLockedError("GPG could not encrypt the health database")

    def decrypt(self, source: Path) -> bytes:
        command = self._base_command() + ["--quiet", "--decrypt", str(source)]
        result = subprocess.run(
            command,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            check=False,
        )
        if result.returncode != 0:
            raise HealthStoreLockedError("GPG could not unlock the health database")
        return result.stdout

    def _base_command(self) -> list[str]:
        command = [self.executable, "--batch", "--no-tty"]
        if self.homedir is not None:
            command.extend(["--homedir", str(self.homedir)])
        return command


@dataclass(frozen=True)
class HealthValue:
    name: str
    value: Decimal
    unit: str

    def __post_init__(self) -> None:
        _bounded_text("health value name", self.name, 64)
        _bounded_text("health value unit", self.unit, 32)
        _validated_decimal(self.value)


@dataclass(frozen=True)
class HealthObservation:
    record_id: str
    principal: str
    metric: str
    start_epoch_ms: int
    end_epoch_ms: int
    source: str
    privacy: str
    values: tuple[HealthValue, ...]

    def __post_init__(self) -> None:
        _bounded_text("record id", self.record_id, 128)
        _bounded_text("principal", self.principal, 128)
        _bounded_text("source", self.source, 128)
        if self.metric not in HEALTH_METRICS:
            raise ValueError("unsupported health metric")
        if self.privacy not in PRIVACY_CLASSES:
            raise ValueError("unsupported health privacy class")
        if not isinstance(self.start_epoch_ms, int) or self.start_epoch_ms < 0:
            raise ValueError("health start time must be a non-negative integer")
        if not isinstance(self.end_epoch_ms, int) or self.end_epoch_ms < self.start_epoch_ms:
            raise ValueError("health end time must not precede its start")
        if not self.values or len(self.values) > 32:
            raise ValueError("health observation must contain 1 to 32 values")
        if not all(isinstance(value, HealthValue) for value in self.values):
            raise ValueError("health observation values must be HealthValue instances")


@dataclass(frozen=True)
class HealthGoal:
    principal: str
    metric: str
    target: Decimal
    unit: str
    period: str
    updated_epoch_ms: int

    def __post_init__(self) -> None:
        _bounded_text("principal", self.principal, 128)
        _bounded_text("goal unit", self.unit, 32)
        if self.metric not in GOAL_METRICS:
            raise ValueError("unsupported health goal metric")
        if self.period not in GOAL_PERIODS:
            raise ValueError("unsupported health goal period")
        if _validated_decimal(self.target) <= 0:
            raise ValueError("health goal target must be positive")
        if not isinstance(self.updated_epoch_ms, int) or self.updated_epoch_ms < 0:
            raise ValueError("health goal update time must be a non-negative integer")


@dataclass(frozen=True)
class HealthSnapshot:
    observations: tuple[HealthObservation, ...]
    goals: tuple[HealthGoal, ...]


class PrologHealthStore:
    def __init__(self, path: Path, cipher: HealthCipher | None = None) -> None:
        self.path = Path(path)
        self.cipher = cipher

    def load(self) -> HealthSnapshot:
        if not self.path.exists():
            return HealthSnapshot((), ())
        if self.cipher is None:
            if self.path.stat().st_mode & 0o077:
                raise HealthStoreLockedError("Health database permissions must be owner-only")
            try:
                plaintext = self.path.read_bytes()
            except OSError as error:
                raise HealthStoreLockedError("Health database could not be read") from error
        else:
            try:
                plaintext = self.cipher.decrypt(self.path)
            except HealthStoreError:
                raise
            except Exception as error:
                raise HealthStoreLockedError("Health database could not be unlocked") from error
        return parse_health_facts(plaintext)

    def replace(
        self,
        observations: Iterable[HealthObservation],
        goals: Iterable[HealthGoal],
    ) -> None:
        unique_observations = {item.record_id: item for item in observations}
        unique_goals = {(item.principal, item.metric, item.period): item for item in goals}
        snapshot = HealthSnapshot(
            tuple(sorted(unique_observations.values(), key=lambda item: item.record_id)),
            tuple(sorted(unique_goals.values(), key=lambda item: (item.principal, item.metric, item.period))),
        )
        plaintext = serialize_health_facts(snapshot)
        if self.cipher is None:
            _write_private_atomic(self.path, plaintext)
        else:
            _encrypt_atomic(self.path, plaintext, self.cipher)


class GpgPrologHealthStore(PrologHealthStore):
    def __init__(self, path: Path, cipher: HealthCipher) -> None:
        super().__init__(path, cipher)


def serialize_health_facts(snapshot: HealthSnapshot) -> bytes:
    lines = [_VERSION_FACT]
    lines.extend(_observation_fact(item) for item in snapshot.observations)
    lines.extend(_goal_fact(item) for item in snapshot.goals)
    payload = ("\n".join(lines) + "\n").encode("utf-8")
    if len(payload) > _MAX_DATABASE_BYTES:
        raise HealthStoreFormatError("health database exceeds its size limit")
    return payload


def parse_health_facts(payload: bytes) -> HealthSnapshot:
    if len(payload) > _MAX_DATABASE_BYTES:
        raise HealthStoreFormatError("health database exceeds its size limit")
    try:
        lines = payload.decode("utf-8", errors="strict").splitlines()
    except UnicodeDecodeError as error:
        raise HealthStoreFormatError("health database is not UTF-8") from error
    if not lines or lines[0] != _VERSION_FACT:
        raise HealthStoreFormatError("unsupported health database version")
    if len(lines) - 1 > _MAX_FACTS:
        raise HealthStoreFormatError("health database exceeds its fact limit")
    observations: list[HealthObservation] = []
    goals: list[HealthGoal] = []
    for line in lines[1:]:
        observation_match = _OBSERVATION.fullmatch(line)
        if observation_match is not None:
            observations.append(_parse_observation(observation_match))
            continue
        goal_match = _GOAL.fullmatch(line)
        if goal_match is not None:
            goals.append(_parse_goal(goal_match))
            continue
        raise HealthStoreFormatError("unsupported health database fact")
    if len({item.record_id for item in observations}) != len(observations):
        raise HealthStoreFormatError("duplicate health observation id")
    goal_keys = {(item.principal, item.metric, item.period) for item in goals}
    if len(goal_keys) != len(goals):
        raise HealthStoreFormatError("duplicate health goal")
    return HealthSnapshot(tuple(observations), tuple(goals))


def render_private_org_health_summary(
    observations: Iterable[HealthObservation],
    goals: Iterable[HealthGoal],
    approved_metrics: set[str] | frozenset[str],
) -> str:
    approved = set(approved_metrics) & ORG_EXPORTABLE_METRICS
    selected_goals = sorted(
        (goal for goal in goals if goal.metric in approved),
        key=lambda item: (item.metric, item.period),
    )
    totals: dict[tuple[str, str], Decimal] = {}
    for item in observations:
        if item.metric not in approved or item.privacy != "wellness":
            continue
        for value in item.values:
            key = (item.metric, value.unit)
            totals[key] = totals.get(key, Decimal(0)) + value.value
    lines = [
        "#+title: Zara Health Summary",
        "#+filetags: :health:private:",
        "#+property: ROAM_VISIBILITY private",
        "",
        "* Health goals :health:private:",
        ":PROPERTIES:",
        ":VISIBILITY: private",
        ":HEALTH_SOURCE: gpg-prolog",
        ":END:",
    ]
    for goal in selected_goals:
        label = goal.metric.replace("_", " ").title()
        current = totals.get((goal.metric, goal.unit))
        progress = ""
        if current is not None:
            progress = f"; current {_decimal_text(current)} {goal.unit}"
        lines.append(
            f"** {label} goal\n- Target: {_decimal_text(goal.target)} {goal.unit} {goal.period}{progress}"
        )
    if not selected_goals:
        lines.append("** No approved goals")
    return "\n".join(lines) + "\n"


def write_org_health_summary(
    destination: Path,
    content: str,
    cipher: HealthCipher | None = None,
) -> None:
    payload = content.encode("utf-8")
    if cipher is not None:
        _encrypt_atomic(Path(destination), payload, cipher)
    else:
        _write_private_atomic(Path(destination), payload)


def _observation_fact(item: HealthObservation) -> str:
    values = ",".join(
        f"health_value({_encode_token(value.name)},{_decimal_text(value.value)},{_encode_token(value.unit)})"
        for value in item.values
    )
    return (
        f"health_observation({_encode_token(item.record_id)},{_encode_token(item.principal)},"
        f"{item.metric},{item.start_epoch_ms},{item.end_epoch_ms},{_encode_token(item.source)},"
        f"{item.privacy},[{values}])."
    )


def _goal_fact(item: HealthGoal) -> str:
    return (
        f"health_goal({_encode_token(item.principal)},{item.metric},{_decimal_text(item.target)},"
        f"{_encode_token(item.unit)},{item.period},{item.updated_epoch_ms})."
    )


def _parse_observation(match: re.Match[str]) -> HealthObservation:
    raw_values = match.group("values")
    values: list[HealthValue] = []
    if raw_values:
        for raw_value in raw_values.split(",health_value("):
            candidate = raw_value if raw_value.startswith("health_value(") else "health_value(" + raw_value
            value_match = _VALUE.fullmatch(candidate)
            if value_match is None:
                raise HealthStoreFormatError("invalid health value fact")
            values.append(
                HealthValue(
                    _decode_token(value_match.group("name")),
                    _parse_decimal(value_match.group("value")),
                    _decode_token(value_match.group("unit")),
                )
            )
    try:
        return HealthObservation(
            record_id=_decode_token(match.group("id")),
            principal=_decode_token(match.group("principal")),
            metric=match.group("metric"),
            start_epoch_ms=int(match.group("start")),
            end_epoch_ms=int(match.group("end")),
            source=_decode_token(match.group("source")),
            privacy=match.group("privacy"),
            values=tuple(values),
        )
    except ValueError as error:
        raise HealthStoreFormatError("invalid health observation") from error


def _parse_goal(match: re.Match[str]) -> HealthGoal:
    try:
        return HealthGoal(
            principal=_decode_token(match.group("principal")),
            metric=match.group("metric"),
            target=_parse_decimal(match.group("target")),
            unit=_decode_token(match.group("unit")),
            period=match.group("period"),
            updated_epoch_ms=int(match.group("updated")),
        )
    except ValueError as error:
        raise HealthStoreFormatError("invalid health goal") from error


def _encrypt_atomic(path: Path, plaintext: bytes, cipher: HealthCipher) -> None:
    _prepare_private_parent(path.parent)
    descriptor, temp_name = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    os.close(descriptor)
    temp_path = Path(temp_name)
    temp_path.unlink()
    try:
        cipher.encrypt_to(plaintext, temp_path)
        if not temp_path.is_file() or temp_path.stat().st_size == 0:
            raise HealthStoreLockedError("Health encryption produced no database")
        temp_path.chmod(0o600)
        with temp_path.open("rb") as handle:
            os.fsync(handle.fileno())
        temp_path.replace(path)
        path.chmod(0o600)
    except HealthStoreError:
        temp_path.unlink(missing_ok=True)
        raise
    except Exception as error:
        temp_path.unlink(missing_ok=True)
        raise HealthStoreLockedError("Health database could not be encrypted") from error


def _write_private_atomic(path: Path, payload: bytes) -> None:
    _prepare_private_parent(path.parent)
    descriptor, temp_name = tempfile.mkstemp(prefix=f".{path.name}.", dir=path.parent)
    temp_path = Path(temp_name)
    try:
        with os.fdopen(descriptor, "wb") as handle:
            handle.write(payload)
            handle.flush()
            os.fsync(handle.fileno())
        temp_path.chmod(0o600)
        temp_path.replace(path)
        path.chmod(0o600)
    except Exception:
        temp_path.unlink(missing_ok=True)
        raise


def _prepare_private_parent(path: Path) -> None:
    path.mkdir(parents=True, exist_ok=True, mode=0o700)
    path.chmod(0o700)


def _encode_token(value: str) -> str:
    encoded = base64.urlsafe_b64encode(value.encode("utf-8")).decode("ascii").rstrip("=")
    return "b64_" + encoded


def _decode_token(token: str) -> str:
    encoded = token.removeprefix("b64_")
    padding = "=" * ((4 - len(encoded) % 4) % 4)
    try:
        decoded = base64.b64decode(encoded + padding, altchars=b"-_", validate=True).decode("utf-8")
    except (ValueError, UnicodeDecodeError) as error:
        raise HealthStoreFormatError("invalid encoded health token") from error
    if _encode_token(decoded) != token:
        raise HealthStoreFormatError("non-canonical encoded health token")
    return decoded


def _parse_decimal(value: str) -> Decimal:
    try:
        return _validated_decimal(Decimal(value))
    except InvalidOperation as error:
        raise HealthStoreFormatError("invalid health number") from error


def _validated_decimal(value: Decimal) -> Decimal:
    if not isinstance(value, Decimal) or not value.is_finite() or abs(value) > Decimal("1e18"):
        raise ValueError("health value must be a finite bounded decimal")
    return value


def _decimal_text(value: Decimal) -> str:
    value = _validated_decimal(value)
    rendered = format(value, "f")
    if "." in rendered:
        rendered = rendered.rstrip("0").rstrip(".")
    return rendered or "0"


def _bounded_text(name: str, value: str, maximum: int) -> str:
    if not isinstance(value, str) or not value or len(value) > maximum:
        raise ValueError(f"{name} must contain 1 to {maximum} characters")
    if any(ord(character) < 32 or ord(character) == 127 for character in value):
        raise ValueError(f"{name} contains a control character")
    return value
