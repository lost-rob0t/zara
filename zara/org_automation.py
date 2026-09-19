"""Parser-neutral symbolic contract for Org-backed Zara automation.

Org source remains canonical.  This module deliberately does not parse, write,
schedule, or execute Org files.  The canonical Org parser/storage layer projects
headings into :class:`OrgAutomationHeading`; this module compiles that projection
into inert typed references to the one #986 programmable symbol namespace.

Likewise, symbol lookup is injected by the runtime.  This module owns no command
registry, capability store, approval policy, scheduler, filesystem authority, or
execution path.  A recipe can reference authority, but never grants it.
"""

from __future__ import annotations

import hashlib
import json
import re
from dataclasses import dataclass
from typing import Callable, Mapping, Optional


_MAX_ID_LENGTH = 160
_MAX_TITLE_LENGTH = 512
_MAX_SOURCE_LENGTH = 4096
_MAX_PROPERTY_KEY_LENGTH = 128
_MAX_PROPERTY_VALUE_LENGTH = 4096
_MAX_PROPERTIES = 256
_MAX_ACTIONS = 64
_MAX_SYMBOL_LENGTH = 160
_MAX_CAPABILITIES = 64
_MAX_CAPABILITY_LENGTH = 128
_MAX_PLATFORMS = 16
_MAX_PLATFORM_LENGTH = 48

_SYMBOL_RE = re.compile(r"^[A-Za-z0-9][A-Za-z0-9_.:/-]*$")

_TRUE_VALUES = frozenset({"1", "t", "true", "yes", "on"})
_FALSE_VALUES = frozenset({"0", "nil", "false", "no", "off", ""})

_ROOT_RESERVED = frozenset({"ID", "ZARA_AUTOMATION", "ZARA_ENABLED"})
_STEP_SELECTORS = {
    "WHEN": "EVENT",
    "IF": "PREDICATE",
    "THEN": "COMMAND",
}
_STEP_KINDS = {
    "WHEN": frozenset({"event", "trigger"}),
    "IF": frozenset({"condition", "predicate", "function"}),
    "THEN": frozenset({"action", "command"}),
}

# Synced recipe data is declarative. These keys are not parameter names: they
# are ambient execution mechanisms that must stay behind typed host adapters.
_FORBIDDEN_ARGUMENT_KEYS = frozenset(
    {
        "CALL",
        "COMMAND_LINE",
        "EVAL",
        "EXEC",
        "FILESYSTEM",
        "INTENT",
        "KOTLIN_CLASS",
        "PLUGIN_REGISTRY",
        "PROLOG",
        "PROLOG_GOAL",
        "PYTHON",
        "PYTHON_EVAL",
        "RAW_INTENT",
        "SECRET",
        "SHELL",
    }
)


class AutomationCompileError(ValueError):
    """The projected Org recipe is malformed or tries to embed authority."""


@dataclass(frozen=True)
class OrgAutomationHeading:
    """Minimal projection supplied by Zara's canonical Org parser.

    ``source`` is provenance only. It can name any configured file/workspace;
    this layer has no default Org root and never opens the path.
    """

    title: str
    properties: Mapping[str, str]
    children: tuple["OrgAutomationHeading", ...] = ()
    source: str = ""
    line: Optional[int] = None


@dataclass(frozen=True)
class AutomationStep:
    role: str
    symbol: str
    arguments: tuple[tuple[str, str], ...] = ()


@dataclass(frozen=True)
class AutomationRecipe:
    automation_id: str
    title: str
    enabled: bool
    source: str
    trigger: AutomationStep
    condition: Optional[AutomationStep]
    actions: tuple[AutomationStep, ...]
    metadata: tuple[tuple[str, str], ...]
    definition_hash: str


@dataclass(frozen=True)
class AutomationSymbol:
    """Read-only projection from the canonical programmable registry.

    Required capabilities are descriptive here. They are not acquired or
    granted by inspection; invocation must still pass the runtime's canonical
    principal/capability/permission/approval checks.
    """

    symbol: str
    kind: str
    owner: str = ""
    capabilities: tuple[str, ...] = ()
    platforms: tuple[str, ...] = ()
    available: bool = True


@dataclass(frozen=True)
class AutomationDependency:
    role: str
    symbol: str
    kind: str
    owner: str
    capabilities: tuple[str, ...]


@dataclass(frozen=True)
class AutomationInspection:
    status: str
    dependencies: tuple[AutomationDependency, ...] = ()
    reason: str = ""


def compile_automation(root: OrgAutomationHeading) -> AutomationRecipe:
    """Compile a canonical-parser heading projection into an inert recipe.

    The source text is never rewritten. Unknown root properties remain inert
    metadata, while WHEN/IF/THEN parameters remain inert strings. Only the
    selector fields (EVENT/PREDICATE/COMMAND) become symbol references.
    """

    if not isinstance(root, OrgAutomationHeading):
        raise AutomationCompileError("automation root must be an Org heading projection")

    title = _bounded_text(root.title, "heading title", _MAX_TITLE_LENGTH, allow_empty=False)
    source = _bounded_text(root.source, "source", _MAX_SOURCE_LENGTH, allow_empty=True)
    properties = _normalized_properties(root.properties, context="automation root")

    if not _parse_flag(properties.get("ZARA_AUTOMATION", ""), "ZARA_AUTOMATION"):
        raise AutomationCompileError("heading is not marked ZARA_AUTOMATION")

    automation_id = _bounded_identifier(properties.get("ID", ""), "ID")
    enabled = _parse_flag(properties.get("ZARA_ENABLED", "t"), "ZARA_ENABLED")

    sections: dict[str, list[OrgAutomationHeading]] = {
        "WHEN": [],
        "IF": [],
        "THEN": [],
    }
    for child in root.children:
        if not isinstance(child, OrgAutomationHeading):
            raise AutomationCompileError("automation children must be Org heading projections")
        section = child.title.strip().upper()
        if section in sections:
            sections[section].append(child)

    if len(sections["WHEN"]) != 1:
        raise AutomationCompileError("automation requires exactly one WHEN section")
    if len(sections["IF"]) > 1:
        raise AutomationCompileError("automation permits at most one IF section")
    if not sections["THEN"]:
        raise AutomationCompileError("automation requires at least one THEN section")
    if len(sections["THEN"]) > _MAX_ACTIONS:
        raise AutomationCompileError(f"automation exceeds {_MAX_ACTIONS} actions")

    trigger = _compile_step("WHEN", sections["WHEN"][0])
    condition = (
        _compile_step("IF", sections["IF"][0]) if sections["IF"] else None
    )
    actions = tuple(_compile_step("THEN", child) for child in sections["THEN"])
    metadata = tuple(
        sorted((key, value) for key, value in properties.items() if key not in _ROOT_RESERVED)
    )

    semantic = {
        "id": automation_id,
        "title": title,
        "enabled": enabled,
        "trigger": _step_json(trigger),
        "condition": _step_json(condition) if condition is not None else None,
        "actions": [_step_json(step) for step in actions],
        "metadata": list(metadata),
    }
    encoded = json.dumps(
        semantic,
        ensure_ascii=False,
        sort_keys=True,
        separators=(",", ":"),
    ).encode("utf-8")
    definition_hash = hashlib.sha256(encoded).hexdigest()

    return AutomationRecipe(
        automation_id=automation_id,
        title=title,
        enabled=enabled,
        source=source,
        trigger=trigger,
        condition=condition,
        actions=actions,
        metadata=metadata,
        definition_hash=definition_hash,
    )


def inspect_automation(
    recipe: AutomationRecipe,
    describe_symbol: Callable[[str], Optional[AutomationSymbol]],
    *,
    platform: str,
) -> AutomationInspection:
    """Resolve recipe dependencies without invoking them.

    This is discovery only. Capability requirements are surfaced for callers;
    they are not satisfied or granted here. A registry unload or platform
    mismatch therefore changes inspection state without mutating canonical Org.
    """

    if not isinstance(recipe, AutomationRecipe):
        raise TypeError("recipe must be an AutomationRecipe")
    if not callable(describe_symbol):
        raise TypeError("describe_symbol must be callable")
    platform = _bounded_platform(platform)

    if not recipe.enabled:
        return AutomationInspection(status="disabled")

    dependencies: list[AutomationDependency] = []
    for step in _ordered_steps(recipe):
        try:
            descriptor = describe_symbol(step.symbol)
        except Exception:
            return AutomationInspection(
                status="degraded",
                dependencies=tuple(dependencies),
                reason=f"registry_error:{step.symbol}",
            )
        if descriptor is None:
            return AutomationInspection(
                status="degraded",
                dependencies=tuple(dependencies),
                reason=f"missing_symbol:{step.symbol}",
            )
        try:
            descriptor = _validate_descriptor(descriptor)
        except (AutomationCompileError, TypeError):
            return AutomationInspection(
                status="degraded",
                dependencies=tuple(dependencies),
                reason=f"invalid_symbol_descriptor:{step.symbol}",
            )

        dependency = AutomationDependency(
            role=step.role,
            symbol=descriptor.symbol,
            kind=descriptor.kind,
            owner=descriptor.owner,
            capabilities=descriptor.capabilities,
        )
        dependencies.append(dependency)

        if descriptor.symbol != step.symbol:
            return AutomationInspection(
                status="degraded",
                dependencies=tuple(dependencies),
                reason=f"symbol_identity_mismatch:{step.symbol}",
            )
        if descriptor.kind not in _STEP_KINDS[step.role]:
            return AutomationInspection(
                status="degraded",
                dependencies=tuple(dependencies),
                reason=f"symbol_kind_mismatch:{step.symbol}:{descriptor.kind}",
            )
        if not descriptor.available:
            return AutomationInspection(
                status="degraded",
                dependencies=tuple(dependencies),
                reason=f"unavailable_symbol:{step.symbol}",
            )
        if descriptor.platforms and platform not in descriptor.platforms:
            return AutomationInspection(
                status="unsupported",
                dependencies=tuple(dependencies),
                reason=f"unsupported_platform:{step.symbol}:{platform}",
            )

    return AutomationInspection(status="ready", dependencies=tuple(dependencies))


def _ordered_steps(recipe: AutomationRecipe) -> tuple[AutomationStep, ...]:
    condition = (recipe.condition,) if recipe.condition is not None else ()
    return (recipe.trigger,) + condition + recipe.actions


def _compile_step(role: str, heading: OrgAutomationHeading) -> AutomationStep:
    properties = _normalized_properties(heading.properties, context=role)
    selector = _STEP_SELECTORS[role]
    symbol = _bounded_symbol(properties.get(selector, ""), selector)
    arguments: list[tuple[str, str]] = []
    for key, value in properties.items():
        if key == selector:
            continue
        if key in _FORBIDDEN_ARGUMENT_KEYS:
            raise AutomationCompileError(
                f"{role} argument {key!r} is an ambient authority mechanism"
            )
        arguments.append((key, value))
    return AutomationStep(role=role, symbol=symbol, arguments=tuple(sorted(arguments)))


def _normalized_properties(
    properties: Mapping[str, str],
    *,
    context: str,
) -> dict[str, str]:
    if not isinstance(properties, Mapping):
        raise AutomationCompileError(f"{context} properties must be a mapping")
    if len(properties) > _MAX_PROPERTIES:
        raise AutomationCompileError(f"{context} has too many properties")

    normalized: dict[str, str] = {}
    for raw_key, raw_value in properties.items():
        key = _bounded_text(
            raw_key,
            f"{context} property key",
            _MAX_PROPERTY_KEY_LENGTH,
            allow_empty=False,
        ).upper()
        if any(character.isspace() or ord(character) < 32 for character in key):
            raise AutomationCompileError(f"{context} property key contains whitespace/control")
        if key in normalized:
            raise AutomationCompileError(f"{context} contains duplicate property {key!r}")
        value = _bounded_text(
            raw_value,
            f"{context} property {key}",
            _MAX_PROPERTY_VALUE_LENGTH,
            allow_empty=True,
        )
        normalized[key] = value
    return normalized


def _parse_flag(value: str, name: str) -> bool:
    if not isinstance(value, str):
        raise AutomationCompileError(f"{name} must be a string")
    normalized = value.strip().lower()
    if normalized in _TRUE_VALUES:
        return True
    if normalized in _FALSE_VALUES:
        return False
    raise AutomationCompileError(f"{name} must be a boolean Org property")


def _bounded_identifier(value: str, name: str) -> str:
    value = _bounded_text(value, name, _MAX_ID_LENGTH, allow_empty=False)
    if any(character.isspace() or ord(character) < 32 for character in value):
        raise AutomationCompileError(f"{name} must not contain whitespace/control characters")
    return value


def _bounded_symbol(value: str, name: str) -> str:
    value = _bounded_text(value, name, _MAX_SYMBOL_LENGTH, allow_empty=False)
    if not _SYMBOL_RE.fullmatch(value):
        raise AutomationCompileError(f"{name} must be a bounded symbolic identifier")
    return value


def _bounded_platform(value: str) -> str:
    value = _bounded_text(value, "platform", _MAX_PLATFORM_LENGTH, allow_empty=False)
    if not _SYMBOL_RE.fullmatch(value):
        raise AutomationCompileError("platform must be a bounded identifier")
    return value


def _bounded_text(value: object, name: str, limit: int, *, allow_empty: bool) -> str:
    if not isinstance(value, str):
        raise AutomationCompileError(f"{name} must be a string")
    if len(value) > limit:
        raise AutomationCompileError(f"{name} exceeds the {limit}-character bound")
    if not allow_empty and not value:
        raise AutomationCompileError(f"{name} must not be empty")
    if any(ord(character) == 0 for character in value):
        raise AutomationCompileError(f"{name} contains a NUL character")
    return value


def _validate_descriptor(descriptor: AutomationSymbol) -> AutomationSymbol:
    if not isinstance(descriptor, AutomationSymbol):
        raise TypeError("registry projection must be AutomationSymbol")
    symbol = _bounded_symbol(descriptor.symbol, "symbol")
    kind = _bounded_symbol(descriptor.kind, "kind")
    owner = _bounded_text(descriptor.owner, "owner", _MAX_ID_LENGTH, allow_empty=True)
    if not isinstance(descriptor.available, bool):
        raise AutomationCompileError("symbol availability must be boolean")

    capabilities = _bounded_string_tuple(
        descriptor.capabilities,
        name="capability",
        max_items=_MAX_CAPABILITIES,
        max_length=_MAX_CAPABILITY_LENGTH,
        identifier=True,
    )
    platforms = _bounded_string_tuple(
        descriptor.platforms,
        name="platform",
        max_items=_MAX_PLATFORMS,
        max_length=_MAX_PLATFORM_LENGTH,
        identifier=True,
    )
    return AutomationSymbol(
        symbol=symbol,
        kind=kind,
        owner=owner,
        capabilities=capabilities,
        platforms=platforms,
        available=descriptor.available,
    )


def _bounded_string_tuple(
    values: tuple[str, ...],
    *,
    name: str,
    max_items: int,
    max_length: int,
    identifier: bool,
) -> tuple[str, ...]:
    if not isinstance(values, tuple):
        raise AutomationCompileError(f"{name} list must be a tuple")
    if len(values) > max_items:
        raise AutomationCompileError(f"too many {name} entries")
    result: list[str] = []
    for value in values:
        bounded = _bounded_text(value, name, max_length, allow_empty=False)
        if identifier and not _SYMBOL_RE.fullmatch(bounded):
            raise AutomationCompileError(f"{name} must be a bounded identifier")
        result.append(bounded)
    if len(set(result)) != len(result):
        raise AutomationCompileError(f"duplicate {name} entries")
    return tuple(result)


def _step_json(step: AutomationStep) -> dict[str, object]:
    return {
        "role": step.role,
        "symbol": step.symbol,
        "arguments": list(step.arguments),
    }


__all__ = [
    "AutomationCompileError",
    "AutomationDependency",
    "AutomationInspection",
    "AutomationRecipe",
    "AutomationStep",
    "AutomationSymbol",
    "OrgAutomationHeading",
    "compile_automation",
    "inspect_automation",
]
