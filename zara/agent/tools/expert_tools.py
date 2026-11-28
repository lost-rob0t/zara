"""Conversational ZARA-EXPERT/1 agent tools and expert-pack file access.

This module is the only bridge between the conversational agent and the
canonical expert registry in :mod:`zara.experts`. It never reimplements
contract semantics: catalog, describe, match, activation, invocation,
status, explain and deactivation all run through the one registry and its
consumer port. Expert pack file tools are a narrow FileTooling projection
over the resolved expert roots (project `.zara/experts/`, the user XDG
experts dir, and configured extra canonical roots).
"""

from __future__ import annotations

import csv
import json
import logging
import os
import re
from dataclasses import asdict
from pathlib import Path
from typing import Any, Callable, Dict, List, Optional, Sequence

from langchain_core.tools import StructuredTool
from pydantic import BaseModel, Field

from ...expert_port import CanonicalExpertInvocationPort
from ...experts import (
    ExpertAmbiguityError,
    ExpertContractError,
    ExpertDescriptor,
    ExpertIncompatibleProtocolError,
    ExpertRegistry,
    ExpertRequest,
)
from ...todo_skills import todo_brief
from .file_tools import (
    DEFAULT_MAX_BYTES,
    FileDiffArgs,
    FileListArgs,
    FileReadArgs,
    FileTooling,
    FileToolingConfig,
    FileWriteArgs,
)

logger = logging.getLogger(__name__)

PORTABLE_PATTERN = re.compile(r"^[!-~]{1,512}$")

DESCRIPTOR_FIXTURE = (
    Path(__file__).resolve().parents[3] / "contracts" / "zara-expert-v1" / "descriptors.tsv"
)

PROTOCOL_TOOL_NAMES = frozenset(
    {
        "expert_list",
        "expert_describe",
        "expert_match",
        "expert_activate",
        "expert_status",
        "expert_invoke",
        "expert_explain",
        "expert_deactivate",
    }
)

PACK_TOOL_NAMES = frozenset(
    {
        "expert_read_file",
        "expert_write_file",
        "expert_diff_file",
        "expert_list_dir",
    }
)

EXPERT_TOOL_NAMES = PROTOCOL_TOOL_NAMES | PACK_TOOL_NAMES

_ERROR_CODES = {
    ExpertAmbiguityError: "ambiguity",
}


def _error_code(error: ExpertContractError) -> str:
    for error_type, code in _ERROR_CODES.items():
        if isinstance(error, error_type):
            return code
    name = type(error).__name__.removeprefix("Expert").removesuffix("Error")
    return re.sub(r"(?<!^)(?=[A-Z])", "_", name).lower()


def _expert_error(error: ExpertContractError) -> str:
    return f"Error: {_error_code(error)}: {error}"


def workspace_identity(repo_root: Path) -> str:
    """Bound a repo path into a registry-safe workspace identity string."""

    candidate = str(repo_root)
    return candidate if PORTABLE_PATTERN.match(candidate) else "local"


def _items(value: str) -> tuple[str, ...]:
    if not value or value == "-":
        return ()
    return tuple(part for part in value.split(",") if part)


def _scalar(value: str) -> str:
    return "" if value == "-" else value


def hydrate_descriptor_row(row: Dict[str, str]) -> Dict[str, Any]:
    """Hydrate one shared fixture TSV row into descriptor wire form."""

    wire: Dict[str, Any] = {
        "protocol": row["protocol"],
        "expert_id": row["expert_id"],
        "expert_version": row["expert_version"],
        "package_namespace": row["package_namespace"],
        "manifest_digest": row["manifest_digest"],
        "name": row["name"],
        "description": row["description"],
        "source_reference": row["source_reference"],
        "reasoning_kind": row["reasoning_kind"],
        "operations": [
            {
                "operation_id": operation_id,
                "input_schema": {"fields": []},
                "output_schema": {"fields": []},
            }
            for operation_id in _items(row["operations"])
        ],
        "applicability": {"keywords": list(_items(row["applicability_keywords"]))},
        "required_capabilities": list(_items(row["required_capabilities"])),
        "possible_effects": list(_items(row["possible_effects"])),
        "supported_engines": list(_items(row["supported_engines"])),
        "supported_platforms": list(_items(row["supported_platforms"])),
        "fallback_policy": row["fallback_policy"],
        "delegation_policy": row["delegation_policy"],
        "registry_generation": 1,
        "availability": row["availability"],
    }
    reason = _scalar(row["unavailable_reason"])
    if reason:
        wire["unavailable_reason"] = reason
    return wire


def load_fixture_descriptors(path: Optional[Path] = None) -> list[ExpertDescriptor]:
    fixture = path or DESCRIPTOR_FIXTURE
    with fixture.open(newline="", encoding="utf-8") as handle:
        rows = list(csv.DictReader(handle, delimiter="\t"))
    descriptors: list[ExpertDescriptor] = []
    for row in rows:
        wire = hydrate_descriptor_row(row)
        try:
            descriptors.append(ExpertDescriptor.from_wire(wire))
        except ExpertIncompatibleProtocolError:
            logger.warning(
                "expert %s declares incompatible protocol %s; skipped from registration",
                wire["expert_id"],
                wire["protocol"],
            )
    return descriptors


def _todo_expert_handler(**_inputs: Any) -> Dict[str, Any]:
    return {
        "verdict": "succeeded",
        "data": {"brief": todo_brief(["8"])},
        "evidence_refs": ["zara.todo_skills.todo_brief"],
        "usage": {"model_calls": 0},
        "effect_receipts": [],
    }


DEFAULT_HANDLERS: Dict[str, Callable[..., Any]] = {
    "zara:expert/todo": _todo_expert_handler,
}


def build_expert_registry(
    handlers: Optional[Dict[str, Callable[..., Any]]] = None,
) -> ExpertRegistry:
    """Create the canonical registry seeded from the shared fixture.

    Handlers bind by exact expert id; fixture experts without a handler stay
    visible and diagnosable but fail closed as unsupported on invocation.
    """

    registry = ExpertRegistry()
    descriptors = load_fixture_descriptors()
    registry.reload(
        (descriptor, (handlers or {}).get(descriptor.expert_id))
        for descriptor in descriptors
    )
    return registry


def resolve_expert_roots(
    repo_root: Path,
    extra_roots: Sequence[str] = (),
) -> List[Path]:
    """Resolve existing expert pack roots: project, user XDG, then extras.

    Missing or symlinked candidates are dropped instead of failing startup.
    """

    candidates = [Path(repo_root) / ".zara" / "experts"]
    xdg_config = os.getenv("XDG_CONFIG_HOME")
    config_home = Path(xdg_config).expanduser() if xdg_config else Path.home() / ".config"
    candidates.append(config_home / "zarathushtra" / "experts")
    candidates.extend(Path(os.path.expanduser(str(root))) for root in extra_roots)

    roots: List[Path] = []
    seen = set()
    for candidate in candidates:
        if candidate.is_symlink():
            continue
        try:
            resolved = candidate.resolve(strict=True)
        except OSError:
            continue
        if not resolved.is_dir() or resolved in seen:
            continue
        seen.add(resolved)
        roots.append(resolved)
    return roots


class ExpertListArgs(BaseModel):
    offset: int = Field(0, description="Catalog offset; validated by the expert registry.")
    limit: int = Field(32, description="Catalog page size; validated by the expert registry.")


class ExpertIdArgs(BaseModel):
    expert_id: str = Field(..., description="Canonical expert id, such as zara:expert/todo.")


class ExpertMatchArgs(BaseModel):
    goal_text: str = Field(..., description="Bounded goal text to match against experts.")


class ExpertInvokeArgs(BaseModel):
    expert_id: str = Field(..., description="Canonical expert id to invoke.")
    operation: str = Field(..., description="Declared expert operation, such as route.diagnose.")
    input_json: str = Field("{}", description="JSON object of declared operation inputs.")


class ExpertExplainArgs(BaseModel):
    invocation_id: str = Field(..., description="Invocation id returned by expert_invoke.")


class ExpertWriteArgs(BaseModel):
    path: str = Field(..., description="File within an expert pack root.")
    content: str = Field(..., description="Complete UTF-8 file content.")
    allow_overwrite: bool = Field(
        False,
        description="Replace an existing regular file atomically when true.",
    )


class ExpertDiffArgs(BaseModel):
    path: str = Field(..., description="Existing file within an expert pack root.")
    content: str = Field(..., description="Complete proposed UTF-8 content.")


def build_expert_tools(
    registry: Optional[ExpertRegistry] = None,
    roots: Sequence[Path] = (),
    handlers: Optional[Dict[str, Callable[..., Any]]] = None,
    principal: str = "zara-agent",
    workspace: Optional[str] = None,
) -> list[StructuredTool]:
    """Build the conversational expert toolset over the canonical registry."""

    active_registry = registry if registry is not None else build_expert_registry(handlers)
    port = CanonicalExpertInvocationPort(active_registry)
    workspace_id = workspace if workspace is not None else workspace_identity(Path.cwd())
    handles: Dict[str, Any] = {}

    def _active_handle(expert_id: str):
        return port.active_activation(principal, workspace_id, expert_id)

    def expert_list(offset: int = 0, limit: int = 32) -> str:
        try:
            catalog = active_registry.list_experts(principal, offset=offset, limit=limit)
        except ExpertContractError as error:
            return _expert_error(error)
        return json.dumps(catalog, default=str)

    def expert_describe(expert_id: str) -> str:
        try:
            return json.dumps(active_registry.describe(expert_id), default=str)
        except ExpertContractError as error:
            return _expert_error(error)

    def expert_match(goal_text: str) -> str:
        try:
            matched = active_registry.match(goal_text)
        except ExpertContractError as error:
            return _expert_error(error)
        if matched is None:
            return "no expert matches this goal"
        return json.dumps(matched, default=str)

    def expert_activate(expert_id: str) -> str:
        try:
            existing = _active_handle(expert_id)
            if existing is not None:
                return json.dumps(
                    {
                        "activation_id": existing.activation_id,
                        "expert_id": expert_id,
                        "already_active": True,
                        "state": "active",
                    }
                )
            handle, receipt = active_registry.activate(principal, workspace_id, expert_id)
        except ExpertContractError as error:
            return _expert_error(error)
        handles[expert_id] = handle
        return json.dumps(receipt, default=str)

    def expert_status(expert_id: str) -> str:
        try:
            descriptor = active_registry.describe(expert_id)
        except ExpertContractError as error:
            return _expert_error(error)
        handle = _active_handle(expert_id)
        payload = {
            "expert_id": expert_id,
            "active": handle is not None,
            "availability": descriptor.get("availability"),
            "registry_generation": active_registry.generation,
            "runtime_generation": active_registry.runtime_generation,
        }
        if handle is not None:
            payload["activation_id"] = handle.activation_id
            payload["workspace"] = handle.workspace
        return json.dumps(payload, default=str)

    def expert_invoke(expert_id: str, operation: str, input_json: str = "{}") -> str:
        handle = _active_handle(expert_id)
        if handle is None:
            return (
                "Error: denied: expert "
                f"{expert_id!r} has no active activation in this workspace; "
                "call expert_activate first"
            )
        try:
            expert_input = json.loads(input_json)
        except ValueError as error:
            return f"Error: invalid_input: input_json is not valid JSON: {error}"
        if not isinstance(expert_input, dict):
            return "Error: invalid_input: input_json must encode a JSON object"
        try:
            result = port.invoke(
                ExpertRequest(
                    request_id=None,
                    operation="expert.invoke",
                    activation_id=handle.activation_id,
                    expert_id=expert_id,
                    expert_operation=operation,
                    input=expert_input,
                )
            )
        except ExpertContractError as error:
            return _expert_error(error)
        return json.dumps(asdict(result), default=str)

    def expert_explain(invocation_id: str) -> str:
        try:
            return json.dumps(active_registry.explain(invocation_id), default=str)
        except ExpertContractError as error:
            return _expert_error(error)

    def expert_deactivate(expert_id: str) -> str:
        handle = _active_handle(expert_id)
        if handle is None:
            return f"Error: denied: expert {expert_id!r} has no active activation"
        try:
            receipt = active_registry.deactivate(handle)
        except ExpertContractError as error:
            return _expert_error(error)
        handles.pop(expert_id, None)
        return json.dumps(receipt, default=str)

    protocol_tools = [
        StructuredTool.from_function(
            func=expert_list,
            name="expert_list",
            description=(
                "List registered ZARA-EXPERT/1 experts with availability and "
                "applicability keywords."
            ),
            args_schema=ExpertListArgs,
        ),
        StructuredTool.from_function(
            func=expert_describe,
            name="expert_describe",
            description="Return the full ZARA-EXPERT/1 descriptor for one expert.",
            args_schema=ExpertIdArgs,
        ),
        StructuredTool.from_function(
            func=expert_match,
            name="expert_match",
            description=(
                "Match a goal against expert applicability keywords; deterministic, "
                "no model involved."
            ),
            args_schema=ExpertMatchArgs,
        ),
        StructuredTool.from_function(
            func=expert_activate,
            name="expert_activate",
            description="Activate one expert for this principal and workspace.",
            args_schema=ExpertIdArgs,
        ),
        StructuredTool.from_function(
            func=expert_status,
            name="expert_status",
            description="Report one expert's availability and activation state.",
            args_schema=ExpertIdArgs,
        ),
        StructuredTool.from_function(
            func=expert_invoke,
            name="expert_invoke",
            description=(
                "Invoke a declared operation on an active expert; requires "
                "expert_activate first."
            ),
            args_schema=ExpertInvokeArgs,
        ),
        StructuredTool.from_function(
            func=expert_explain,
            name="expert_explain",
            description="Explain one expert invocation: verdict, decisions, evidence, usage.",
            args_schema=ExpertExplainArgs,
        ),
        StructuredTool.from_function(
            func=expert_deactivate,
            name="expert_deactivate",
            description="Deactivate one expert and release its activation handle.",
            args_schema=ExpertIdArgs,
        ),
    ]

    pack_tools = _build_pack_tools(roots)
    return protocol_tools + pack_tools


def _build_pack_tools(roots: Sequence[Path]) -> list[StructuredTool]:
    if not roots:
        return []
    base = Path(roots[0])
    try:
        tooling = FileTooling(
            FileToolingConfig(
                base_dir=base,
                readable_roots=tuple(Path(root) for root in roots),
                writable_roots=tuple(Path(root) for root in roots),
                max_bytes=DEFAULT_MAX_BYTES,
            )
        )
    except (ValueError, OSError) as error:
        logger.warning("Expert pack file tools disabled: %r", error)
        return []

    return [
        StructuredTool.from_function(
            func=tooling.read_file,
            name="expert_read_file",
            description="Read expert pack text files within the expert roots.",
            args_schema=FileReadArgs,
        ),
        StructuredTool.from_function(
            func=tooling.write_file,
            name="expert_write_file",
            description="Atomically write expert pack files within the expert roots.",
            args_schema=ExpertWriteArgs,
        ),
        StructuredTool.from_function(
            func=tooling.diff_file,
            name="expert_diff_file",
            description="Diff proposed expert pack content against the file on disk.",
            args_schema=ExpertDiffArgs,
        ),
        StructuredTool.from_function(
            func=tooling.list_dir,
            name="expert_list_dir",
            description="List expert pack directories within the expert roots.",
            args_schema=FileListArgs,
        ),
    ]
