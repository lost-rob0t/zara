"""Deterministic compiler for persisted user-authored semantic commands.

The compiler is intentionally pure: it validates declarative command definitions
against a caller-supplied semantic capability catalog and produces immutable
IntentFrame/action templates. Execution remains owned by the normal runtime.
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from threading import RLock
from types import MappingProxyType
from typing import Any, Mapping, Optional

from .runtime.frames import (
    BoolValue,
    DateTimeValue,
    DurationValue,
    FilledSlot,
    FrameStatus,
    IntentFrame,
    NumberValue,
    RefValue,
    SlotOrigin,
    SlotValue,
    TextValue,
    validate_value,
)
from .user_commands import CommandSlot, SemanticAction, UserCommandDefinition

_VALUE_TYPES = frozenset({"text", "number", "duration", "datetime", "ref", "boolean"})
_DEVICE_TARGET_POLICIES = frozenset({"initiating_device", "explicit_device"})
_SLOT_PLACEHOLDER_NAME_RE = re.compile(r"^[a-z][a-z0-9._-]{0,63}$")


class CommandCompileError(ValueError):
    """A deterministic validation error tied to one definition field."""

    def __init__(self, command_id: str, field: str, code: str, message: str) -> None:
        self.command_id = command_id
        self.field = field
        self.code = code
        super().__init__(f"{command_id}:{field}:{code}: {message}")


@dataclass(frozen=True)
class CapabilityContract:
    """Closed semantic capability surface available to user commands."""

    action_id: str
    intent_ns: str
    intent_name: str
    arguments: Mapping[str, str]
    locations: frozenset[str]

    def __post_init__(self) -> None:
        if not isinstance(self.action_id, str) or not self.action_id.strip():
            raise ValueError("capability action_id must be non-empty")
        if not isinstance(self.intent_ns, str) or not self.intent_ns.strip():
            raise ValueError("capability intent_ns must be non-empty")
        if not isinstance(self.intent_name, str) or not self.intent_name.strip():
            raise ValueError("capability intent_name must be non-empty")
        arguments = dict(self.arguments)
        if any(not isinstance(name, str) or not name for name in arguments):
            raise ValueError("capability argument names must be non-empty strings")
        if any(value_type not in _VALUE_TYPES for value_type in arguments.values()):
            raise ValueError("capability argument schema contains an unsupported value type")
        locations = frozenset(self.locations)
        if not locations or not locations <= {"server", "device"}:
            raise ValueError("capability locations must contain only server/device")
        object.__setattr__(self, "action_id", self.action_id.strip().casefold())
        object.__setattr__(self, "intent_ns", self.intent_ns.strip())
        object.__setattr__(self, "intent_name", self.intent_name.strip())
        object.__setattr__(self, "arguments", MappingProxyType(arguments))
        object.__setattr__(self, "locations", locations)


@dataclass(frozen=True)
class CompiledAction:
    contract: CapabilityContract
    arguments: Mapping[str, SlotValue]
    slot_bindings: Mapping[str, str]
    location: str
    target_policy: Optional[str]

    def __post_init__(self) -> None:
        object.__setattr__(self, "arguments", MappingProxyType(dict(self.arguments)))
        object.__setattr__(self, "slot_bindings", MappingProxyType(dict(self.slot_bindings)))


@dataclass(frozen=True)
class CompiledCommand:
    command_id: str
    trigger: str
    aliases: tuple[str, ...]
    slots: tuple[CommandSlot, ...]
    actions: tuple[CompiledAction, ...]

    def frame_template(self) -> IntentFrame:
        filled: list[FilledSlot] = []
        missing: list[str] = []
        for slot in self.slots:
            if slot.default is not None:
                filled.append(
                    FilledSlot(
                        name=slot.name,
                        value=_typed_value(slot.default, slot.value_type),
                        origin=SlotOrigin.DEFAULT,
                    )
                )
            elif slot.required:
                missing.append(slot.name)
        return IntentFrame(
            intent_ns="user",
            intent_name=self.command_id,
            slots=tuple(filled),
            status=FrameStatus.MISSING if missing else FrameStatus.COMPLETE,
            missing=tuple(missing),
        )


class UserCommandCompiler:
    """Compile one persisted definition against an immutable capability catalog."""

    def __init__(
        self,
        capabilities: Mapping[str, CapabilityContract],
        *,
        protected_triggers: frozenset[str] = frozenset(),
    ) -> None:
        catalog = dict(capabilities)
        for key, contract in catalog.items():
            if not isinstance(contract, CapabilityContract):
                raise TypeError("capability catalog values must be CapabilityContract")
            if key.casefold() != contract.action_id:
                raise ValueError("capability catalog key must match contract action_id")
        self._capabilities = MappingProxyType({key.casefold(): value for key, value in catalog.items()})
        self._protected_triggers = frozenset(_phrase_key(value) for value in protected_triggers)

    def compile(self, definition: UserCommandDefinition) -> CompiledCommand:
        if not isinstance(definition, UserCommandDefinition):
            raise TypeError("definition must be a UserCommandDefinition")
        self._validate_phrases(definition)
        slots = {slot.name: slot for slot in definition.slots}
        actions = tuple(
            self._compile_action(definition.command_id, index, action, slots)
            for index, action in enumerate(definition.actions)
        )
        return CompiledCommand(
            command_id=definition.command_id,
            trigger=definition.trigger,
            aliases=definition.aliases,
            slots=definition.slots,
            actions=actions,
        )

    def _validate_phrases(self, definition: UserCommandDefinition) -> None:
        declared_slots = {slot.name for slot in definition.slots}
        for field, phrase in (("trigger", definition.trigger), *(
            (f"aliases[{index}]", alias) for index, alias in enumerate(definition.aliases)
        )):
            if _phrase_key(phrase) in self._protected_triggers:
                raise CommandCompileError(
                    definition.command_id,
                    field,
                    "protected_trigger",
                    "phrase collides with a protected built-in trigger",
                )
            placeholders = _phrase_placeholders(phrase)
            if placeholders is None:
                raise CommandCompileError(
                    definition.command_id,
                    field,
                    "invalid_slot_placeholder",
                    "phrase contains malformed slot placeholder syntax",
                )
            seen: set[str] = set()
            for placeholder in placeholders:
                if placeholder not in declared_slots:
                    raise CommandCompileError(
                        definition.command_id,
                        field,
                        "unknown_slot_placeholder",
                        f"phrase placeholder names undeclared slot: {placeholder}",
                    )
                if placeholder in seen:
                    raise CommandCompileError(
                        definition.command_id,
                        field,
                        "duplicate_slot_placeholder",
                        f"phrase repeats slot placeholder: {placeholder}",
                    )
                seen.add(placeholder)

    def _compile_action(
        self,
        command_id: str,
        index: int,
        action: SemanticAction,
        slots: Mapping[str, CommandSlot],
    ) -> CompiledAction:
        prefix = f"actions[{index}]"
        contract = self._capabilities.get(action.capability)
        if contract is None:
            raise CommandCompileError(
                command_id,
                f"{prefix}.capability",
                "unknown_capability",
                f"semantic capability is not registered: {action.capability}",
            )

        location = action.location
        if location is None:
            if len(contract.locations) != 1:
                raise CommandCompileError(
                    command_id,
                    f"{prefix}.location",
                    "location_required",
                    "capability supports multiple locations; action must select one",
                )
            location = next(iter(contract.locations))
        if location not in contract.locations:
            raise CommandCompileError(
                command_id,
                f"{prefix}.location",
                "location_not_allowed",
                f"capability {contract.action_id} cannot run at {location}",
            )

        target_policy = action.target_policy
        if location == "device":
            target_policy = target_policy or "initiating_device"
            if target_policy not in _DEVICE_TARGET_POLICIES:
                raise CommandCompileError(
                    command_id,
                    f"{prefix}.target_policy",
                    "target_policy",
                    "unsupported device target policy",
                )
        elif target_policy is not None:
            raise CommandCompileError(
                command_id,
                f"{prefix}.target_policy",
                "target_policy",
                "server actions cannot carry a device target policy",
            )

        literals: dict[str, SlotValue] = {}
        bindings: dict[str, str] = {}
        for raw_name, raw_value in action.arguments.items():
            is_binding = raw_name.endswith("_slot")
            argument_name = raw_name[:-5] if is_binding else raw_name
            expected_type = contract.arguments.get(argument_name)
            if expected_type is None:
                raise CommandCompileError(
                    command_id,
                    f"{prefix}.arguments.{raw_name}",
                    "unknown_argument",
                    f"capability has no argument named {argument_name}",
                )
            if argument_name in literals or argument_name in bindings:
                raise CommandCompileError(
                    command_id,
                    f"{prefix}.arguments.{raw_name}",
                    "duplicate_argument",
                    f"argument {argument_name} is supplied more than once",
                )
            if is_binding:
                if not isinstance(raw_value, str) or raw_value not in slots:
                    raise CommandCompileError(
                        command_id,
                        f"{prefix}.arguments.{raw_name}",
                        "unknown_slot",
                        "slot binding must name a declared command slot",
                    )
                if slots[raw_value].value_type != expected_type:
                    raise CommandCompileError(
                        command_id,
                        f"{prefix}.arguments.{raw_name}",
                        "slot_type",
                        "bound slot type does not match capability argument schema",
                    )
                bindings[argument_name] = raw_value
            else:
                try:
                    literals[argument_name] = _typed_value(raw_value, expected_type)
                except (TypeError, ValueError) as exc:
                    raise CommandCompileError(
                        command_id,
                        f"{prefix}.arguments.{raw_name}",
                        "argument_type",
                        str(exc),
                    ) from exc

        missing = set(contract.arguments) - set(literals) - set(bindings)
        if missing:
            name = sorted(missing)[0]
            raise CommandCompileError(
                command_id,
                f"{prefix}.arguments.{name}",
                "missing_argument",
                f"required capability argument is missing: {name}",
            )

        return CompiledAction(
            contract=contract,
            arguments=literals,
            slot_bindings=bindings,
            location=location,
            target_policy=target_policy,
        )


class CompiledCommandRegistry:
    """Atomically published immutable snapshots for concurrent runtime readers."""

    def __init__(self, compiler: UserCommandCompiler) -> None:
        if not isinstance(compiler, UserCommandCompiler):
            raise TypeError("compiler must be a UserCommandCompiler")
        self._compiler = compiler
        self._snapshot: Mapping[str, CompiledCommand] = MappingProxyType({})
        self._lock = RLock()

    def set_compiler(self, compiler: UserCommandCompiler) -> None:
        if not isinstance(compiler, UserCommandCompiler):
            raise TypeError("compiler must be a UserCommandCompiler")
        with self._lock:
            self._compiler = compiler

    def snapshot(self) -> Mapping[str, CompiledCommand]:
        return self._snapshot

    def replace_all(self, definitions: tuple[UserCommandDefinition, ...]) -> Mapping[str, CompiledCommand]:
        with self._lock:
            compiler = self._compiler
            compiled: dict[str, CompiledCommand] = {}
            phrases: dict[str, str] = {}
            for definition in definitions:
                if not isinstance(definition, UserCommandDefinition):
                    raise TypeError("definitions must contain UserCommandDefinition values")
                if not definition.enabled:
                    continue
                command = compiler.compile(definition)
                if command.command_id in compiled:
                    raise CommandCompileError(
                        command.command_id,
                        "command_id",
                        "duplicate_command",
                        "command id appears more than once in reload",
                    )
                for field, phrase in (("trigger", command.trigger), *(
                    (f"aliases[{index}]", alias) for index, alias in enumerate(command.aliases)
                )):
                    key = _phrase_key(phrase)
                    owner = phrases.get(key)
                    if owner is not None:
                        raise CommandCompileError(
                            command.command_id,
                            field,
                            "trigger_collision",
                            f"phrase collides with command {owner}",
                        )
                    phrases[key] = command.command_id
                compiled[command.command_id] = command
            published = MappingProxyType(dict(sorted(compiled.items())))
            self._snapshot = published
            return published


def _phrase_key(value: str) -> str:
    return " ".join(value.split()).casefold()


def _phrase_placeholders(value: str) -> Optional[tuple[str, ...]]:
    placeholders: list[str] = []
    cursor = 0
    while cursor < len(value):
        opening = value.find("{", cursor)
        closing = value.find("}", cursor)
        if opening < 0:
            return None if closing >= 0 else tuple(placeholders)
        if closing < opening:
            return None
        closing = value.find("}", opening + 1)
        if closing < 0:
            return None
        body = value[opening + 1 : closing]
        if "{" in body or _SLOT_PLACEHOLDER_NAME_RE.fullmatch(body) is None:
            return None
        placeholders.append(body)
        cursor = closing + 1
    return tuple(placeholders)


def _typed_value(raw: Any, expected_type: str) -> SlotValue:
    payload = raw
    if isinstance(raw, Mapping):
        raw_dict = dict(raw)
        if set(raw_dict) != {expected_type}:
            raise ValueError(f"expected tagged {expected_type} value")
        payload = raw_dict[expected_type]

    if expected_type == "text":
        value: SlotValue = TextValue(payload)
    elif expected_type == "number":
        value = NumberValue(payload)
    elif expected_type == "duration":
        value = DurationValue(payload)
    elif expected_type == "boolean":
        value = BoolValue(payload)
    elif expected_type == "ref":
        if not isinstance(payload, Mapping) or set(payload) != {"kind", "id"}:
            raise ValueError("ref value must contain kind and id")
        value = RefValue(kind=payload["kind"], id=payload["id"])
    elif expected_type == "datetime":
        if not isinstance(payload, (list, tuple)) or len(payload) != 6:
            raise ValueError("datetime value must contain six integer fields")
        value = DateTimeValue(*payload)
    else:
        raise ValueError(f"unsupported value type: {expected_type}")

    reason = validate_value(value)
    if reason is not None:
        raise ValueError(f"invalid {expected_type} value: {reason}")
    return value


__all__ = [
    "CapabilityContract",
    "CommandCompileError",
    "CompiledAction",
    "CompiledCommand",
    "CompiledCommandRegistry",
    "UserCommandCompiler",
]
