"""Prolog-driven projection of live Android device capabilities into LLM tools."""

from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Callable, Collection, Dict, Optional, Tuple

from langchain_core.tools import StructuredTool
from pydantic import Field, create_model


_DESCRIPTOR_GOAL = (
    "android_tools:android_tool_descriptor("
    "Name, Wire, Effect, Authority, Permission, Description)"
)
_ARGUMENT_GOAL = (
    "android_tools:android_tool_argument("
    "Name, Arg, Type, Required, MaxBytes)"
)
_ALLOWED_EFFECTS = frozenset({"read", "navigation", "mutating", "destructive"})
_ALLOWED_AUTHORITIES = frozenset({"standard", "accessibility", "elevated"})
_ALLOWED_TYPES = frozenset({"string"})


@dataclass(frozen=True)
class AndroidToolArgument:
    name: str
    value_type: str
    required: bool
    max_bytes: int


@dataclass(frozen=True)
class AndroidToolDescriptor:
    name: str
    wire_id: str
    effect: str
    authority: str
    permission: str
    description: str
    arguments: Tuple[AndroidToolArgument, ...]


def _text(value: Any, label: str) -> str:
    result = str(value)
    if not result or any(ord(ch) < 0x20 for ch in result):
        raise ValueError(f"{label} is invalid")
    return result


def load_android_tool_descriptors(prolog_engine) -> Tuple[AndroidToolDescriptor, ...]:
    """Load the closed Android tool catalog from Prolog.

    No Android tool name/schema is copied into Python. Python validates and
    projects the Prolog metadata into runtime tool objects.
    """

    descriptor_rows = list(
        prolog_engine.query_iter(_DESCRIPTOR_GOAL, max_solutions=128)
    )
    argument_rows = list(
        prolog_engine.query_iter(_ARGUMENT_GOAL, max_solutions=512)
    )

    arguments: Dict[str, list[AndroidToolArgument]] = {}
    for row in argument_rows:
        owner = _text(row["Name"], "argument owner")
        arg_name = _text(row["Arg"], "argument name")
        value_type = _text(row["Type"], "argument type")
        required_atom = _text(row["Required"], "argument requirement")
        if value_type not in _ALLOWED_TYPES:
            raise ValueError(f"unsupported Android argument type: {value_type}")
        if required_atom not in {"required", "optional"}:
            raise ValueError("Android argument requirement must be required or optional")
        max_bytes = row["MaxBytes"]
        if not isinstance(max_bytes, int) or isinstance(max_bytes, bool):
            raise ValueError("Android argument max bytes must be an integer")
        if max_bytes < 1 or max_bytes > 64 * 1024:
            raise ValueError("Android argument max bytes is outside bounds")
        arguments.setdefault(owner, []).append(
            AndroidToolArgument(
                name=arg_name,
                value_type=value_type,
                required=required_atom == "required",
                max_bytes=max_bytes,
            )
        )

    descriptors = []
    names = set()
    wire_ids = set()
    for row in descriptor_rows:
        name = _text(row["Name"], "tool name")
        wire_id = _text(row["Wire"], "wire capability")
        effect = _text(row["Effect"], "effect")
        authority = _text(row["Authority"], "authority")
        permission = _text(row["Permission"], "permission")
        description = _text(row["Description"], "description")
        if name in names:
            raise ValueError(f"duplicate Android tool name: {name}")
        if wire_id in wire_ids:
            raise ValueError(f"duplicate Android wire capability: {wire_id}")
        if effect not in _ALLOWED_EFFECTS:
            raise ValueError(f"unsupported Android tool effect: {effect}")
        if authority not in _ALLOWED_AUTHORITIES:
            raise ValueError(f"unsupported Android authority: {authority}")
        names.add(name)
        wire_ids.add(wire_id)
        descriptors.append(
            AndroidToolDescriptor(
                name=name,
                wire_id=wire_id,
                effect=effect,
                authority=authority,
                permission=permission,
                description=description,
                arguments=tuple(arguments.pop(name, ())),
            )
        )

    if arguments:
        unknown = sorted(arguments)[0]
        raise ValueError(f"Android argument references unknown tool: {unknown}")
    return tuple(descriptors)


def build_android_device_tools(
    prolog_engine,
    dispatch: Callable[[str, Dict[str, Any]], Any],
    available_capabilities: Collection[str],
) -> list[StructuredTool]:
    """Project only capabilities advertised by the initiating Android device."""

    available = frozenset(str(item) for item in available_capabilities)
    tools: list[StructuredTool] = []

    for descriptor in load_android_tool_descriptors(prolog_engine):
        if descriptor.wire_id not in available:
            continue

        fields = {}
        for argument in descriptor.arguments:
            annotation = str if argument.required else Optional[str]
            default = ... if argument.required else None
            fields[argument.name] = (
                annotation,
                Field(
                    default,
                    description=(
                        f"UTF-8 text, maximum {argument.max_bytes} bytes."
                    ),
                ),
            )
        args_schema = create_model(
            "Android"
            + "".join(part.capitalize() for part in descriptor.name.split("_"))
            + "Args",
            **fields,
        )

        def make_invoke(current: AndroidToolDescriptor):
            by_name = {argument.name: argument for argument in current.arguments}

            def invoke(**kwargs):
                payload: Dict[str, Any] = {}
                for name, argument in by_name.items():
                    value = kwargs.get(name)
                    if value is None:
                        if argument.required:
                            raise ValueError(f"{name} is required")
                        continue
                    if not isinstance(value, str) or not value.strip():
                        raise ValueError(f"{name} must be non-empty text")
                    if len(value.encode("utf-8")) > argument.max_bytes:
                        raise ValueError(f"{name} exceeds byte limit")
                    payload[name] = value
                return dispatch(current.wire_id, payload)

            return invoke

        tools.append(
            StructuredTool.from_function(
                func=make_invoke(descriptor),
                name=f"android_{descriptor.name}",
                description=(
                    f"{descriptor.description} "
                    f"Authority: {descriptor.authority}; effect: {descriptor.effect}."
                ),
                args_schema=args_schema,
                metadata={
                    "zara_android_capability": descriptor.wire_id,
                    "zara_android_effect": descriptor.effect,
                    "zara_android_authority": descriptor.authority,
                    "zara_android_permission": descriptor.permission,
                    "zara_requires_approval": descriptor.effect
                    in {"mutating", "destructive"},
                },
            )
        )

    return tools
