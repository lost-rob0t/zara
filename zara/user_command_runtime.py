"""Runtime resolution for compiled user-authored semantic commands.

Resolution is deliberately side-effect free. It reads one immutable compiled
registry snapshot, fills typed slots with the canonical clarification parsers,
and returns semantic command/frame data. Execution and approval stay owned by
the normal Zara runtime.
"""

from __future__ import annotations

import enum
import re
from dataclasses import dataclass
from typing import Callable, Optional

from .runtime.clarification import (
    ClarificationCoordinator,
    DialogueTemplate,
    OpenOutcome,
    SlotSpec,
    SlotType,
    parse_slot_value,
)
from .runtime.frames import FilledSlot, FrameStatus, IntentFrame, SlotOrigin, SlotValue
from .user_command_compiler import CompiledCommand, CompiledCommandRegistry

_PLACEHOLDER = re.compile(r"\{([A-Za-z][A-Za-z0-9_]*)\}")
_MAX_UTTERANCE_CHARS = 512


class UserCommandResolutionKind(str, enum.Enum):
    MATCHED = "matched"
    NO_MATCH = "no_match"
    AMBIGUOUS = "ambiguous"
    DENIED = "denied"
    UNAVAILABLE = "unavailable"


@dataclass(frozen=True)
class UserCommandResolution:
    kind: UserCommandResolutionKind
    command: Optional[CompiledCommand] = None
    frame: Optional[IntentFrame] = None
    reason: Optional[str] = None


CapabilityAvailability = Callable[[str], str]


class UserCommandResolver:
    """Resolve utterances against one atomically published command snapshot."""

    def __init__(
        self,
        registry: CompiledCommandRegistry,
        *,
        capability_availability: Optional[CapabilityAvailability] = None,
    ) -> None:
        if not isinstance(registry, CompiledCommandRegistry):
            raise TypeError("registry must be a CompiledCommandRegistry")
        self._registry = registry
        self._capability_availability = capability_availability

    def resolve(self, utterance: str) -> UserCommandResolution:
        normalized = _normalize_utterance(utterance)
        if normalized is None:
            return UserCommandResolution(UserCommandResolutionKind.NO_MATCH)

        snapshot = self._registry.snapshot()
        matches: dict[str, tuple[CompiledCommand, IntentFrame]] = {}
        for command in snapshot.values():
            matched = _match_command(command, normalized)
            if matched is not None:
                matches[command.command_id] = (command, matched)

        if not matches:
            return UserCommandResolution(UserCommandResolutionKind.NO_MATCH)
        if len(matches) > 1:
            alternatives = tuple(sorted(matches))
            return UserCommandResolution(
                UserCommandResolutionKind.AMBIGUOUS,
                frame=IntentFrame(
                    intent_ns="user",
                    intent_name="command",
                    status=FrameStatus.AMBIGUOUS,
                    alternatives=alternatives,
                ),
            )

        command, frame = next(iter(matches.values()))
        availability = self._capability_availability
        if availability is not None:
            for capability in _command_capabilities(command):
                state = availability(capability)
                if state == "available":
                    continue
                if state == "denied":
                    return UserCommandResolution(
                        UserCommandResolutionKind.DENIED,
                        command=command,
                        frame=frame,
                        reason=capability,
                    )
                return UserCommandResolution(
                    UserCommandResolutionKind.UNAVAILABLE,
                    command=command,
                    frame=frame,
                    reason=capability,
                )

        return UserCommandResolution(
            UserCommandResolutionKind.MATCHED,
            command=command,
            frame=frame,
        )

    def open_clarification(
        self,
        result: UserCommandResolution,
        coordinator: ClarificationCoordinator,
        *,
        principal: str,
        conversation_id: str,
    ) -> OpenOutcome:
        if result.kind is not UserCommandResolutionKind.MATCHED:
            raise ValueError("only matched commands can open clarification")
        if result.command is None or result.frame is None:
            raise ValueError("matched result is missing command/frame")
        if result.frame.status is not FrameStatus.MISSING:
            raise ValueError("clarification requires a missing-slot frame")
        if not isinstance(coordinator, ClarificationCoordinator):
            raise TypeError("coordinator must be a ClarificationCoordinator")

        template = _dialogue_template(result.command)
        prefilled = {slot.name: slot.value for slot in result.frame.slots}
        return coordinator.open(
            template,
            principal=principal,
            conversation_id=conversation_id,
            prefilled=prefilled,
        )


def _normalize_utterance(utterance: str) -> Optional[str]:
    if not isinstance(utterance, str):
        return None
    normalized = " ".join(utterance.split())
    if not normalized or len(normalized) > _MAX_UTTERANCE_CHARS:
        return None
    return normalized


def _match_command(command: CompiledCommand, utterance: str) -> Optional[IntentFrame]:
    exact: Optional[IntentFrame] = None
    parameterized: Optional[IntentFrame] = None
    for phrase in (command.trigger, *command.aliases):
        if not _PLACEHOLDER.search(phrase):
            if phrase.casefold() == utterance.casefold():
                exact = command.frame_template()
                break
            continue
        frame = _match_template(command, phrase, utterance)
        if frame is not None:
            parameterized = frame
    return exact if exact is not None else parameterized


def _match_template(
    command: CompiledCommand, phrase: str, utterance: str
) -> Optional[IntentFrame]:
    pattern = _template_pattern(phrase)
    match = pattern.fullmatch(utterance)
    if match is None:
        return None

    specs = {slot.name: _slot_spec(slot.name, slot.value_type, slot.required) for slot in command.slots}
    captured: dict[str, SlotValue] = {}
    for name, text in match.groupdict().items():
        value = parse_slot_value(specs[name], text, max_chars=_MAX_UTTERANCE_CHARS)
        if value is None:
            return None
        captured[name] = value
    return _fill_frame(command, captured)


def _template_pattern(phrase: str) -> re.Pattern[str]:
    normalized = " ".join(phrase.split())
    chunks: list[str] = []
    offset = 0
    for match in _PLACEHOLDER.finditer(normalized):
        chunks.append(re.escape(normalized[offset : match.start()]))
        chunks.append(f"(?P<{match.group(1)}>.+?)")
        offset = match.end()
    chunks.append(re.escape(normalized[offset:]))
    return re.compile("".join(chunks), re.IGNORECASE)


def _fill_frame(command: CompiledCommand, captured: dict[str, SlotValue]) -> IntentFrame:
    base = command.frame_template()
    current = {slot.name: slot for slot in base.slots}
    for name, value in captured.items():
        current[name] = FilledSlot(name=name, value=value, origin=SlotOrigin.UTTERANCE)

    ordered = tuple(current[slot.name] for slot in command.slots if slot.name in current)
    filled_names = set(current)
    missing = tuple(
        slot.name for slot in command.slots if slot.required and slot.name not in filled_names
    )
    return IntentFrame(
        intent_ns="user",
        intent_name=command.command_id,
        slots=ordered,
        status=FrameStatus.MISSING if missing else FrameStatus.COMPLETE,
        missing=missing,
    )


def _dialogue_template(command: CompiledCommand) -> DialogueTemplate:
    specs = tuple(
        _slot_spec(slot.name, slot.value_type, slot.required) for slot in command.slots
    )
    return DialogueTemplate(
        intent_ns="user",
        intent_name=command.command_id,
        specs=specs,
        arg_order=tuple(slot.name for slot in command.slots),
    )


def _slot_spec(name: str, value_type: str, required: bool) -> SlotSpec:
    slot_type = SlotType(value_type)
    prompts = {
        SlotType.DURATION: "How long?",
        SlotType.NUMBER: "What number?",
        SlotType.BOOLEAN: "Yes or no?",
        SlotType.DATETIME: "When?",
        SlotType.REF: f"Which {name}?",
        SlotType.TEXT: f"What should {name} be?",
    }
    return SlotSpec(
        name=name,
        slot_type=slot_type,
        required=required,
        prompt=prompts[slot_type],
    )


def _command_capabilities(command: CompiledCommand) -> tuple[str, ...]:
    return tuple(sorted({action.contract.action_id for action in command.actions}))


__all__ = [
    "UserCommandResolution",
    "UserCommandResolutionKind",
    "UserCommandResolver",
]
