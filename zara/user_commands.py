"""Principal-scoped persistence for declarative user-authored commands."""

from __future__ import annotations

import json
import math
import re
from dataclasses import dataclass, field, replace
from datetime import datetime, timezone
from typing import Any, Mapping, Optional

from .database import DatabaseManager, get_database
from .principals import PrincipalContext

_SCHEMA_VERSION = 1
_MIGRATION_VERSION = 3
_COMMAND_ID_RE = re.compile(r"^[a-z0-9][a-z0-9._-]{0,63}$")
_CAPABILITY_RE = re.compile(r"^[a-z][a-z0-9._-]{0,63}$")
_SLOT_NAME_RE = re.compile(r"^[a-z][a-z0-9._-]{0,63}$")
_FORBIDDEN_CAPABILITIES = frozenset(
    {"shell", "exec", "eval", "python", "prolog_goal", "dynamic_import"}
)
_EPHEMERAL_PRINCIPAL_KINDS = frozenset({"guest", "ephemeral", "anonymous", "public"})
_SLOT_VALUE_TYPES = frozenset({"text", "number", "duration", "datetime", "ref", "boolean"})
_ACTION_LOCATIONS = frozenset({"server", "device"})
_DEVICE_TARGET_POLICIES = frozenset({"initiating_device", "explicit_device"})


class CommandConflictError(ValueError):
    """Raised when a command mutation conflicts with current durable state."""


@dataclass(frozen=True)
class CommandSlot:
    """Closed typed slot schema for a persisted semantic command."""

    name: str
    value_type: str
    required: bool = True
    default: Optional[Mapping[str, Any]] = None

    def __post_init__(self) -> None:
        name = _normalize_slot_name(self.name)
        value_type = _normalize_slot_type(self.value_type)
        if not isinstance(self.required, bool):
            raise ValueError("slot required must be a boolean")
        default = _validate_slot_default(value_type, self.default)
        object.__setattr__(self, "name", name)
        object.__setattr__(self, "value_type", value_type)
        object.__setattr__(self, "default", default)


@dataclass(frozen=True)
class SemanticAction:
    capability: str
    arguments: Mapping[str, Any] = field(default_factory=dict)
    location: Optional[str] = None
    target_policy: Optional[str] = None

    def __post_init__(self) -> None:
        capability = _normalize_capability(self.capability)
        arguments = _validate_json_object(self.arguments, "action arguments")
        location = _normalize_action_location(self.location)
        target_policy = _normalize_target_policy(self.target_policy, location)
        object.__setattr__(self, "capability", capability)
        object.__setattr__(self, "arguments", arguments)
        object.__setattr__(self, "location", location)
        object.__setattr__(self, "target_policy", target_policy)


@dataclass(frozen=True)
class UserCommandDefinition:
    command_id: str
    trigger: str
    actions: tuple[SemanticAction, ...]
    aliases: tuple[str, ...] = ()
    slots: tuple[CommandSlot, ...] = ()
    enabled: bool = True
    migration_metadata: Mapping[str, Any] = field(default_factory=dict)
    owner_principal_id: Optional[str] = None
    schema_version: int = _SCHEMA_VERSION
    revision: int = 0
    created_at: Optional[str] = None
    updated_at: Optional[str] = None

    def __post_init__(self) -> None:
        object.__setattr__(self, "command_id", _normalize_command_id(self.command_id))
        object.__setattr__(self, "trigger", _normalize_phrase(self.trigger, "trigger"))
        object.__setattr__(self, "aliases", _normalize_aliases(self.aliases, self.trigger))
        object.__setattr__(self, "slots", _validate_slots(self.slots))
        object.__setattr__(self, "actions", _validate_actions(self.actions))
        object.__setattr__(
            self,
            "migration_metadata",
            _validate_json_object(self.migration_metadata, "migration metadata"),
        )
        if not isinstance(self.enabled, bool):
            raise ValueError("enabled must be a boolean")
        if self.owner_principal_id is not None:
            if not isinstance(self.owner_principal_id, str) or not self.owner_principal_id.strip():
                raise ValueError("owner_principal_id must be a non-empty string")
            if self.owner_principal_id != self.owner_principal_id.strip():
                raise ValueError("owner_principal_id must not contain edge whitespace")
        if self.schema_version != _SCHEMA_VERSION:
            raise ValueError(f"unsupported user command schema version: {self.schema_version}")
        if isinstance(self.revision, bool) or not isinstance(self.revision, int) or self.revision < 0:
            raise ValueError("revision must be a non-negative integer")

    def with_updates(self, **changes: Any) -> "UserCommandDefinition":
        return replace(self, **changes)


class UserCommandStore:
    """SQLite-backed command store permanently bound to one principal."""

    def __init__(
        self,
        db: Optional[DatabaseManager] = None,
        *,
        principal: PrincipalContext,
    ) -> None:
        if not isinstance(principal, PrincipalContext):
            raise TypeError("principal must be a PrincipalContext")
        self._db = db or get_database()
        self._principal = principal
        self._ensure_schema()

    def create(self, definition: UserCommandDefinition) -> UserCommandDefinition:
        self._require_durable_principal()
        definition = _validate_definition(definition)
        if definition.owner_principal_id not in (None, self._principal.principal_id):
            raise PermissionError("command owner is bound by the store principal")
        if definition.revision != 0:
            raise ValueError("new command revision must be zero")

        now = _now_iso()
        saved = replace(
            definition,
            owner_principal_id=self._principal.principal_id,
            revision=1,
            created_at=now,
            updated_at=now,
        )
        with self._db.transaction(immediate=True) as conn:
            existing = conn.execute(
                "SELECT 1 FROM user_commands WHERE principal_id = ? AND command_id = ?",
                (self._principal.principal_id, saved.command_id),
            ).fetchone()
            if existing is not None:
                raise CommandConflictError(f"command id already exists: {saved.command_id}")
            self._assert_trigger_keys_available(conn, saved)
            conn.execute(
                """
                INSERT INTO user_commands (
                    principal_id, command_id, schema_version, revision,
                    enabled, definition_json, created_at, updated_at
                ) VALUES (?, ?, ?, ?, ?, ?, ?, ?)
                """,
                (
                    self._principal.principal_id,
                    saved.command_id,
                    saved.schema_version,
                    saved.revision,
                    1 if saved.enabled else 0,
                    _serialize_definition(saved),
                    saved.created_at,
                    saved.updated_at,
                ),
            )
            self._insert_trigger_keys(conn, saved)
        return saved

    def get(self, command_id: str) -> Optional[UserCommandDefinition]:
        normalized_id = _normalize_command_id(command_id)
        row = self._db.fetch_one(
            """
            SELECT principal_id, command_id, schema_version, revision,
                   enabled, definition_json, created_at, updated_at
              FROM user_commands
             WHERE principal_id = ? AND command_id = ?
            """,
            (self._principal.principal_id, normalized_id),
        )
        if row is None:
            return None
        return _deserialize_row(row)

    def list(self) -> list[UserCommandDefinition]:
        rows = self._db.fetch_all(
            """
            SELECT principal_id, command_id, schema_version, revision,
                   enabled, definition_json, created_at, updated_at
              FROM user_commands
             WHERE principal_id = ?
             ORDER BY command_id
            """,
            (self._principal.principal_id,),
        )
        return [_deserialize_row(row) for row in rows]

    def search(self, query: str) -> list[UserCommandDefinition]:
        needle = _normalize_search_query(query)
        results: list[UserCommandDefinition] = []
        for definition in self.list():
            haystack = (
                definition.command_id,
                definition.trigger,
                *definition.aliases,
            )
            if any(needle in value.casefold() for value in haystack):
                results.append(definition)
        return results

    def export(self) -> list[UserCommandDefinition]:
        """Return validated definitions owned by this store's principal only."""

        return self.list()

    def update(
        self,
        definition: UserCommandDefinition,
        *,
        expected_revision: int,
    ) -> UserCommandDefinition:
        self._require_durable_principal()
        definition = _validate_definition(definition)
        expected_revision = _validate_expected_revision(expected_revision)
        with self._db.transaction(immediate=True) as conn:
            row = conn.execute(
                """
                SELECT principal_id, command_id, schema_version, revision,
                       enabled, definition_json, created_at, updated_at
                  FROM user_commands
                 WHERE principal_id = ? AND command_id = ?
                """,
                (self._principal.principal_id, definition.command_id),
            ).fetchone()
            if row is None:
                raise KeyError(definition.command_id)
            current = _deserialize_row(row)
            if current.revision != expected_revision:
                raise CommandConflictError(
                    f"stale command revision: expected {expected_revision}, current {current.revision}"
                )
            if definition.owner_principal_id not in (None, self._principal.principal_id):
                raise PermissionError("command owner is bound by the store principal")

            saved = replace(
                definition,
                owner_principal_id=self._principal.principal_id,
                revision=current.revision + 1,
                created_at=current.created_at,
                updated_at=_now_iso(),
            )
            conn.execute(
                "DELETE FROM user_command_triggers WHERE principal_id = ? AND command_id = ?",
                (self._principal.principal_id, saved.command_id),
            )
            self._assert_trigger_keys_available(conn, saved)
            cursor = conn.execute(
                """
                UPDATE user_commands
                   SET schema_version = ?, revision = ?, enabled = ?,
                       definition_json = ?, updated_at = ?
                 WHERE principal_id = ? AND command_id = ? AND revision = ?
                """,
                (
                    saved.schema_version,
                    saved.revision,
                    1 if saved.enabled else 0,
                    _serialize_definition(saved),
                    saved.updated_at,
                    self._principal.principal_id,
                    saved.command_id,
                    expected_revision,
                ),
            )
            if cursor.rowcount != 1:
                raise CommandConflictError("command revision changed during update")
            self._insert_trigger_keys(conn, saved)
        return saved

    def delete(self, command_id: str, *, expected_revision: int) -> None:
        self._require_durable_principal()
        normalized_id = _normalize_command_id(command_id)
        expected_revision = _validate_expected_revision(expected_revision)
        with self._db.transaction(immediate=True) as conn:
            row = conn.execute(
                "SELECT revision FROM user_commands WHERE principal_id = ? AND command_id = ?",
                (self._principal.principal_id, normalized_id),
            ).fetchone()
            if row is None:
                raise KeyError(normalized_id)
            if int(row["revision"]) != expected_revision:
                raise CommandConflictError(
                    f"stale command revision: expected {expected_revision}, current {row['revision']}"
                )
            cursor = conn.execute(
                """
                DELETE FROM user_commands
                 WHERE principal_id = ? AND command_id = ? AND revision = ?
                """,
                (self._principal.principal_id, normalized_id, expected_revision),
            )
            if cursor.rowcount != 1:
                raise CommandConflictError("command revision changed during delete")

    def _ensure_schema(self) -> None:
        statements = [
            """
            CREATE TABLE IF NOT EXISTS user_commands (
                principal_id TEXT NOT NULL,
                command_id TEXT NOT NULL,
                schema_version INTEGER NOT NULL,
                revision INTEGER NOT NULL,
                enabled INTEGER NOT NULL,
                definition_json TEXT NOT NULL,
                created_at TEXT NOT NULL,
                updated_at TEXT NOT NULL,
                PRIMARY KEY (principal_id, command_id)
            )
            """,
            """
            CREATE TABLE IF NOT EXISTS user_command_triggers (
                principal_id TEXT NOT NULL,
                trigger_key TEXT NOT NULL,
                command_id TEXT NOT NULL,
                PRIMARY KEY (principal_id, trigger_key),
                FOREIGN KEY (principal_id, command_id)
                    REFERENCES user_commands(principal_id, command_id)
                    ON DELETE CASCADE
            )
            """,
            (
                "CREATE INDEX IF NOT EXISTS idx_user_command_triggers_command "
                "ON user_command_triggers(principal_id, command_id)"
            ),
        ]
        try:
            self._db.register_migration(_MIGRATION_VERSION, statements)
        except ValueError:
            pass
        self._db.connect()
        required = {"user_commands", "user_command_triggers"}
        present = {
            str(row["name"])
            for row in self._db.fetch_all(
                "SELECT name FROM sqlite_master WHERE type = 'table' AND name IN (?, ?)",
                tuple(required),
            )
        }
        if present != required:
            raise RuntimeError("user command persistence schema is unavailable")

    def _require_durable_principal(self) -> None:
        if self._principal.kind.casefold() in _EPHEMERAL_PRINCIPAL_KINDS:
            raise PermissionError("ephemeral principals cannot persist user commands")

    def _assert_trigger_keys_available(self, conn: Any, definition: UserCommandDefinition) -> None:
        for trigger_key in _trigger_keys(definition):
            collision = conn.execute(
                """
                SELECT command_id FROM user_command_triggers
                 WHERE principal_id = ? AND trigger_key = ?
                """,
                (self._principal.principal_id, trigger_key),
            ).fetchone()
            if collision is not None:
                raise CommandConflictError(
                    f"command trigger collides with {collision['command_id']}: {trigger_key}"
                )

    def _insert_trigger_keys(self, conn: Any, definition: UserCommandDefinition) -> None:
        conn.executemany(
            """
            INSERT INTO user_command_triggers (principal_id, trigger_key, command_id)
            VALUES (?, ?, ?)
            """,
            [
                (self._principal.principal_id, trigger_key, definition.command_id)
                for trigger_key in _trigger_keys(definition)
            ],
        )


def _normalize_command_id(value: str) -> str:
    if not isinstance(value, str):
        raise ValueError("command_id must be a string")
    normalized = value.strip().casefold()
    if not _COMMAND_ID_RE.fullmatch(normalized):
        raise ValueError("command_id must contain only lowercase letters, digits, '.', '_' or '-'")
    return normalized


def _normalize_slot_name(value: str) -> str:
    if not isinstance(value, str):
        raise ValueError("slot name must be a string")
    normalized = value.strip().casefold()
    if not _SLOT_NAME_RE.fullmatch(normalized):
        raise ValueError("slot name is invalid")
    return normalized


def _normalize_slot_type(value: str) -> str:
    if not isinstance(value, str):
        raise ValueError("slot value_type must be a string")
    normalized = value.strip().casefold()
    if normalized not in _SLOT_VALUE_TYPES:
        raise ValueError(f"unsupported slot value type: {normalized}")
    return normalized


def _normalize_capability(value: str) -> str:
    if not isinstance(value, str):
        raise ValueError("capability must be a string")
    normalized = value.strip().casefold()
    if not _CAPABILITY_RE.fullmatch(normalized):
        raise ValueError("capability id is invalid")
    if normalized in _FORBIDDEN_CAPABILITIES:
        raise ValueError(f"executable capability is not persistable: {normalized}")
    return normalized


def _normalize_action_location(value: Optional[str]) -> Optional[str]:
    if value is None:
        return None
    if not isinstance(value, str):
        raise ValueError("action location must be a string")
    normalized = value.strip().casefold()
    if normalized not in _ACTION_LOCATIONS:
        raise ValueError(f"unsupported action location: {normalized}")
    return normalized


def _normalize_target_policy(value: Optional[str], location: Optional[str]) -> Optional[str]:
    if value is None:
        return None
    if location != "device":
        raise ValueError("target_policy is only valid for device actions")
    if not isinstance(value, str):
        raise ValueError("target_policy must be a string")
    normalized = value.strip().casefold()
    if normalized not in _DEVICE_TARGET_POLICIES:
        raise ValueError(f"unsupported device target policy: {normalized}")
    return normalized


def _normalize_phrase(value: str, field_name: str) -> str:
    if not isinstance(value, str):
        raise ValueError(f"{field_name} must be a string")
    normalized = " ".join(value.split())
    if not normalized:
        raise ValueError(f"{field_name} must not be empty")
    if len(normalized) > 256:
        raise ValueError(f"{field_name} is too long")
    return normalized


def _normalize_aliases(values: tuple[str, ...], trigger: str) -> tuple[str, ...]:
    if not isinstance(values, (tuple, list)):
        raise ValueError("aliases must be a sequence")
    normalized: list[str] = []
    seen = {_phrase_key(trigger)}
    for value in values:
        alias = _normalize_phrase(value, "alias")
        key = _phrase_key(alias)
        if key in seen:
            raise ValueError("trigger and aliases must be unique")
        seen.add(key)
        normalized.append(alias)
    if len(normalized) > 32:
        raise ValueError("too many aliases")
    return tuple(normalized)


def _validate_slots(values: tuple[CommandSlot, ...]) -> tuple[CommandSlot, ...]:
    if not isinstance(values, (tuple, list)):
        raise ValueError("slots must be a sequence")
    slots = tuple(values)
    if len(slots) > 32:
        raise ValueError("too many command slots")
    if any(not isinstance(slot, CommandSlot) for slot in slots):
        raise ValueError("slots must contain CommandSlot values")
    names = [slot.name for slot in slots]
    if len(set(names)) != len(names):
        raise ValueError("command slot names must be unique")
    return slots


def _validate_actions(values: tuple[SemanticAction, ...]) -> tuple[SemanticAction, ...]:
    if not isinstance(values, (tuple, list)):
        raise ValueError("actions must be a sequence")
    actions = tuple(values)
    if not actions:
        raise ValueError("at least one semantic action is required")
    if len(actions) > 32:
        raise ValueError("too many semantic actions")
    if any(not isinstance(action, SemanticAction) for action in actions):
        raise ValueError("actions must contain SemanticAction values")
    return actions


def _validate_json_object(value: Mapping[str, Any], field_name: str) -> dict[str, Any]:
    if not isinstance(value, Mapping):
        raise ValueError(f"{field_name} must be a JSON object")
    raw = dict(value)
    if any(not isinstance(key, str) for key in raw):
        raise ValueError(f"{field_name} keys must be strings")
    try:
        encoded = json.dumps(raw, allow_nan=False, sort_keys=True, separators=(",", ":"))
        decoded = json.loads(encoded)
    except (TypeError, ValueError) as exc:
        raise ValueError(f"{field_name} must contain JSON-compatible values") from exc
    if not isinstance(decoded, dict):
        raise ValueError(f"{field_name} must be a JSON object")
    return decoded


def _validate_slot_default(
    value_type: str,
    value: Optional[Mapping[str, Any]],
) -> Optional[dict[str, Any]]:
    if value is None:
        return None
    default = _validate_json_object(value, "slot default")
    if set(default) != {value_type}:
        raise ValueError("slot default tag must match slot value_type")
    payload = default[value_type]
    if value_type == "text":
        if not isinstance(payload, str) or not payload.strip():
            raise ValueError("text slot default must be non-empty text")
    elif value_type == "number":
        if isinstance(payload, bool) or not isinstance(payload, (int, float)):
            raise ValueError("number slot default must be numeric")
        if isinstance(payload, float) and not math.isfinite(payload):
            raise ValueError("number slot default must be finite")
    elif value_type == "duration":
        if isinstance(payload, bool) or not isinstance(payload, int) or payload < 0:
            raise ValueError("duration slot default must be non-negative integer seconds")
    elif value_type == "boolean":
        if not isinstance(payload, bool):
            raise ValueError("boolean slot default must be a boolean")
    elif value_type == "ref":
        if not isinstance(payload, dict) or set(payload) != {"kind", "id"}:
            raise ValueError("ref slot default must contain kind and id")
        if not isinstance(payload["kind"], str) or not payload["kind"].strip():
            raise ValueError("ref slot default kind must be non-empty")
        if not isinstance(payload["id"], str) or not payload["id"].strip():
            raise ValueError("ref slot default id must be non-empty")
    elif value_type == "datetime":
        if not isinstance(payload, list) or len(payload) != 6:
            raise ValueError("datetime slot default must be [year, month, day, hour, minute, second]")
        if any(isinstance(item, bool) or not isinstance(item, int) for item in payload):
            raise ValueError("datetime slot default fields must be integers")
        year, month, day, hour, minute, second = payload
        if year < 1 or not 1 <= month <= 12 or not 1 <= day <= 31:
            raise ValueError("datetime slot default date is invalid")
        if not 0 <= hour <= 23 or not 0 <= minute <= 59 or not 0 <= second <= 59:
            raise ValueError("datetime slot default time is invalid")
    return default


def _validate_definition(value: UserCommandDefinition) -> UserCommandDefinition:
    if not isinstance(value, UserCommandDefinition):
        raise TypeError("definition must be a UserCommandDefinition")
    return value


def _validate_expected_revision(value: int) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value < 1:
        raise ValueError("expected_revision must be a positive integer")
    return value


def _normalize_search_query(value: str) -> str:
    if not isinstance(value, str):
        raise ValueError("search query must be a string")
    normalized = " ".join(value.split()).casefold()
    if not normalized:
        raise ValueError("search query must not be empty")
    return normalized


def _phrase_key(value: str) -> str:
    return " ".join(value.split()).casefold()


def _trigger_keys(definition: UserCommandDefinition) -> tuple[str, ...]:
    return tuple(_phrase_key(value) for value in (definition.trigger, *definition.aliases))


def _serialize_definition(definition: UserCommandDefinition) -> str:
    payload = {
        "command_id": definition.command_id,
        "trigger": definition.trigger,
        "aliases": list(definition.aliases),
        "slots": [
            {
                "name": slot.name,
                "value_type": slot.value_type,
                "required": slot.required,
                "default": slot.default,
            }
            for slot in definition.slots
        ],
        "actions": [
            {
                "capability": action.capability,
                "arguments": dict(action.arguments),
                "location": action.location,
                "target_policy": action.target_policy,
            }
            for action in definition.actions
        ],
        "enabled": definition.enabled,
        "migration_metadata": dict(definition.migration_metadata),
    }
    return json.dumps(payload, allow_nan=False, sort_keys=True, separators=(",", ":"))


def _deserialize_row(row: Any) -> UserCommandDefinition:
    schema_version = int(row["schema_version"])
    if schema_version != _SCHEMA_VERSION:
        raise ValueError(f"unsupported user command schema version: {schema_version}")
    try:
        payload = json.loads(str(row["definition_json"]))
    except json.JSONDecodeError as exc:
        raise ValueError("stored user command JSON is malformed") from exc
    if not isinstance(payload, dict):
        raise ValueError("stored user command must be a JSON object")
    expected_keys = {
        "command_id",
        "trigger",
        "aliases",
        "slots",
        "actions",
        "enabled",
        "migration_metadata",
    }
    if set(payload) != expected_keys:
        raise ValueError("stored user command contains unknown or missing fields")

    raw_slots = payload["slots"]
    if not isinstance(raw_slots, list):
        raise ValueError("stored user command slots must be a list")
    slots: list[CommandSlot] = []
    for raw_slot in raw_slots:
        if not isinstance(raw_slot, dict) or set(raw_slot) != {
            "name",
            "value_type",
            "required",
            "default",
        }:
            raise ValueError("stored command slot is malformed")
        slots.append(
            CommandSlot(
                name=raw_slot["name"],
                value_type=raw_slot["value_type"],
                required=raw_slot["required"],
                default=raw_slot["default"],
            )
        )

    raw_actions = payload["actions"]
    if not isinstance(raw_actions, list):
        raise ValueError("stored user command actions must be a list")
    actions: list[SemanticAction] = []
    for raw_action in raw_actions:
        if not isinstance(raw_action, dict) or set(raw_action) != {
            "capability",
            "arguments",
            "location",
            "target_policy",
        }:
            raise ValueError("stored semantic action is malformed")
        actions.append(
            SemanticAction(
                capability=raw_action["capability"],
                arguments=raw_action["arguments"],
                location=raw_action["location"],
                target_policy=raw_action["target_policy"],
            )
        )

    definition = UserCommandDefinition(
        command_id=payload["command_id"],
        trigger=payload["trigger"],
        aliases=tuple(payload["aliases"]),
        slots=tuple(slots),
        actions=tuple(actions),
        enabled=payload["enabled"],
        migration_metadata=payload["migration_metadata"],
        owner_principal_id=str(row["principal_id"]),
        schema_version=schema_version,
        revision=int(row["revision"]),
        created_at=str(row["created_at"]),
        updated_at=str(row["updated_at"]),
    )
    if definition.command_id != str(row["command_id"]):
        raise ValueError("stored command identity does not match row identity")
    if definition.enabled != bool(row["enabled"]):
        raise ValueError("stored command enabled state does not match row state")
    return definition


def _now_iso() -> str:
    return datetime.now(timezone.utc).replace(microsecond=0).isoformat()


__all__ = [
    "CommandConflictError",
    "CommandSlot",
    "SemanticAction",
    "UserCommandDefinition",
    "UserCommandStore",
]
