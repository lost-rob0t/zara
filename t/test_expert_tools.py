"""Conversational ZARA-EXPERT/1 tools and out-of-the-box expert-pack access.

Zara's agent must speak the canonical expert protocol through the one
ExpertRegistry (list/describe/match/activate/invoke/status/deactivate/explain)
and must be able to read and write expert pack directories out of the box:
the project `.zara/experts/`, the user XDG experts dir, and configured extra
canonical roots such as the dotfiles expert library.
"""

from __future__ import annotations

import json
from pathlib import Path
from typing import Any
import csv

import pytest

from zara.agent.tools import expert_tools
from zara.agent.tools.builtin_tools import get_builtin_tools
from zara.agent.tools.expert_tools import (
    DEFAULT_HANDLERS,
    PACK_TOOL_NAMES,
    PROTOCOL_TOOL_NAMES,
    build_expert_registry,
    build_expert_tools,
    hydrate_descriptor_row,
    load_fixture_descriptors,
    resolve_expert_roots,
    workspace_identity,
)
from zara.agent.tools.registry import ToolRegistry
from zara.config import DEFAULT_CONFIG_TOML, ZaraConfig
from zara.experts import (
    ExpertDescriptor,
    ExpertIncompatibleProtocolError,
    ExpertRegistry,
)

ALL_EXPERT_TOOL_NAMES = PROTOCOL_TOOL_NAMES | PACK_TOOL_NAMES

DESCRIPTOR_FIXTURE = (
    Path(__file__).resolve().parents[1] / "contracts" / "zara-expert-v1" / "descriptors.tsv"
)


def _fixture_rows() -> list[dict[str, str]]:
    with DESCRIPTOR_FIXTURE.open(newline="", encoding="utf-8") as handle:
        return list(csv.DictReader(handle, delimiter="\t"))


def _todo_handler(**_inputs: Any):
    return {
        "verdict": "succeeded",
        "data": {"brief": "1. (TODO) write the red test"},
        "evidence_refs": ["ev:test-todo-brief"],
        "usage": {"model_calls": 0},
        "effect_receipts": [],
    }


def _registry_with_todo() -> ExpertRegistry:
    return build_expert_registry(handlers={"zara:expert/todo": _todo_handler})


def _tools(registry: ExpertRegistry, roots: list[Path] | None = None):
    return {
        tool.name: tool
        for tool in build_expert_tools(
            registry=registry,
            roots=roots if roots is not None else [],
            workspace="test-workspace",
        )
    }


def _invoke(tool, **kwargs: Any) -> str:
    return str(tool.invoke(kwargs))


def test_shared_fixture_seeds_the_canonical_registry() -> None:
    registry = _registry_with_todo()

    todo = registry.describe("zara:expert/todo")
    assert todo["protocol"] == "ZARA-EXPERT/1"
    assert todo["reasoning_kind"] == "symbolic"

    catalog = registry.list_experts("zara-agent")
    listed = {item["expert_id"] for item in catalog["experts"]}
    assert "zara:expert/todo" in listed
    assert "zara:expert/android-troubleshooting" in listed
    # Incompatible protocol majors fail closed and are never registered.
    assert "zara:expert/future" not in listed
    # Absent experts stay out of the catalog.
    assert "zara:expert/prolog-rlm" not in listed
    assert registry.snapshot().generation >= 1


def test_incompatible_protocol_major_fails_closed_at_hydration() -> None:
    descriptors = load_fixture_descriptors()
    assert all(descriptor.protocol == "ZARA-EXPERT/1" for descriptor in descriptors)

    with pytest.raises(ExpertIncompatibleProtocolError):
        ExpertDescriptor.from_wire(
            hydrate_descriptor_row(
                next(
                    row
                    for row in _fixture_rows()
                    if row["expert_id"] == "zara:expert/future"
                )
            )
        )


def test_expert_tool_surface_matches_the_closed_vocabulary() -> None:
    registry = _registry_with_todo()
    tools = _tools(registry)

    assert PROTOCOL_TOOL_NAMES.issubset(tools)
    assert PACK_TOOL_NAMES.isdisjoint(tools)


def test_pack_file_tools_load_when_roots_exist(tmp_path: Path) -> None:
    registry = _registry_with_todo()
    pack_root = tmp_path / "packs"
    pack_root.mkdir()
    tools = _tools(registry, roots=[pack_root])

    assert PACK_TOOL_NAMES.issubset(tools)
    assert (
        _invoke(tools["expert_write_file"], path="kb/note.pl", content="fact.")
        == "Wrote kb/note.pl"
    )
    assert _invoke(tools["expert_read_file"], path="kb/note.pl") == "fact."
    assert _invoke(tools["expert_list_dir"], path=".") == "kb/"
    assert "+facts." in _invoke(tools["expert_diff_file"], path="kb/note.pl", content="facts.")


def test_pack_file_tools_reject_paths_outside_expert_roots(tmp_path: Path) -> None:
    registry = _registry_with_todo()
    pack_root = tmp_path / "packs"
    pack_root.mkdir()
    outside = tmp_path / "outside"
    outside.mkdir()
    (outside / "secret.txt").write_text("untouched")
    tools = _tools(registry, roots=[pack_root])

    assert _invoke(tools["expert_read_file"], path=str(outside / "secret.txt")).startswith(
        "Error:"
    )
    assert _invoke(
        tools["expert_write_file"],
        path=str(outside / "escape.txt"),
        content="x",
        allow_overwrite=True,
    ).startswith("Error:")
    assert (outside / "secret.txt").read_text() == "untouched"
    assert not (outside / "escape.txt").exists()


def test_pack_file_tools_reject_symlink_escape(tmp_path: Path) -> None:
    registry = _registry_with_todo()
    pack_root = tmp_path / "packs"
    pack_root.mkdir()
    outside = tmp_path / "outside"
    outside.mkdir()
    link = pack_root / "link"
    link.symlink_to(outside, target_is_directory=True)
    tools = _tools(registry, roots=[pack_root])

    result = _invoke(tools["expert_write_file"], path="link/escaped.txt", content="x")
    assert result.startswith("Error:")
    assert not (outside / "escaped.txt").exists()


def test_default_todo_handler_is_zero_model(monkeypatch, tmp_path) -> None:
    monkeypatch.setattr(expert_tools, "todo_brief", lambda limit: "1. (STRT) shipped")
    registry = build_expert_registry(handlers=DEFAULT_HANDLERS)
    tools = _tools(registry, roots=[tmp_path])

    json.loads(_invoke(tools["expert_activate"], expert_id="zara:expert/todo"))
    result = json.loads(
        _invoke(tools["expert_invoke"], expert_id="zara:expert/todo", operation="route.diagnose")
    )
    assert result["verdict"] == "succeeded"
    assert result["data"]["brief"] == "1. (STRT) shipped"
    assert result["usage"] == {"model_calls": 0}


def test_workspace_identity_bounds_repo_paths(tmp_path: Path) -> None:
    assert workspace_identity(tmp_path) == str(tmp_path)
    assert workspace_identity(Path("/tmp/has space")) == "local"


def test_protocol_tools_report_typed_errors_for_bad_input() -> None:
    registry = _registry_with_todo()
    tools = _tools(registry)

    assert _invoke(tools["expert_list"], offset=-1).startswith("Error: invalid_input")
    assert _invoke(tools["expert_match"], goal_text="bad\x07goal").startswith(
        "Error: invalid_input"
    )
    assert _invoke(tools["expert_status"], expert_id="zara:expert/missing").startswith(
        "Error:"
    )
    assert _invoke(tools["expert_explain"], invocation_id="inv:nope").startswith("Error:")

    denied = _invoke(tools["expert_deactivate"], expert_id="zara:expert/todo")
    assert denied.startswith("Error: denied")
    assert "no active activation" in denied


def test_expert_invoke_rejects_malformed_input_json() -> None:
    registry = _registry_with_todo()
    tools = _tools(registry)
    json.loads(_invoke(tools["expert_activate"], expert_id="zara:expert/todo"))

    assert _invoke(
        tools["expert_invoke"],
        expert_id="zara:expert/todo",
        operation="route.diagnose",
        input_json="{not json",
    ).startswith("Error: invalid_input")
    assert _invoke(
        tools["expert_invoke"],
        expert_id="zara:expert/todo",
        operation="route.diagnose",
        input_json="[1]",
    ).startswith("Error: invalid_input")


def test_pack_tools_are_dropped_when_roots_are_invalid(tmp_path: Path) -> None:
    real = tmp_path / "real"
    real.mkdir()
    alias = tmp_path / "alias"
    alias.symlink_to(real, target_is_directory=True)

    tools = _tools(_registry_with_todo(), roots=[alias])

    assert PACK_TOOL_NAMES.isdisjoint(tools)
    assert PROTOCOL_TOOL_NAMES.issubset(tools)


def test_resolve_expert_roots_deduplicates_and_skips_missing(
    tmp_path: Path, monkeypatch
) -> None:
    monkeypatch.delenv("XDG_CONFIG_HOME", raising=False)
    monkeypatch.setenv("HOME", str(tmp_path / "home"))
    extra = tmp_path / "library"
    extra.mkdir()

    roots = resolve_expert_roots(tmp_path / "repo", [str(extra), str(extra)])

    assert roots == [extra]


def test_activate_invoke_status_deactivate_flow() -> None:
    registry = _registry_with_todo()
    tools = _tools(registry)

    receipt = json.loads(
        _invoke(tools["expert_activate"], expert_id="zara:expert/todo")
    )
    assert receipt["expert_id"] == "zara:expert/todo"
    assert receipt["state"] == "active"

    result = json.loads(
        _invoke(tools["expert_invoke"], expert_id="zara:expert/todo", operation="route.diagnose")
    )
    assert result["verdict"] == "succeeded"
    assert result["usage"] == {"model_calls": 0}
    assert result["data"]["brief"] == "1. (TODO) write the red test"

    status = json.loads(_invoke(tools["expert_status"], expert_id="zara:expert/todo"))
    assert status["active"] is True
    assert status["activation_id"] == receipt["activation_id"]

    explain = json.loads(
        _invoke(tools["expert_explain"], invocation_id=result["invocation_id"])
    )
    assert explain["expert_id"] == "zara:expert/todo"
    assert explain["verdict"] == "succeeded"

    deactivated = json.loads(
        _invoke(tools["expert_deactivate"], expert_id="zara:expert/todo")
    )
    assert deactivated["deactivated"] is True
    status = json.loads(_invoke(tools["expert_status"], expert_id="zara:expert/todo"))
    assert status["active"] is False

    late = _invoke(tools["expert_invoke"], expert_id="zara:expert/todo", operation="route.diagnose")
    assert late.startswith("Error:")


def test_invoke_without_activation_fails_closed() -> None:
    registry = _registry_with_todo()
    tools = _tools(registry)

    result = _invoke(tools["expert_invoke"], expert_id="zara:expert/todo", operation="route.diagnose")
    assert result.startswith("Error:")
    assert "activate" in result


def test_activate_is_idempotent_for_one_identity() -> None:
    registry = _registry_with_todo()
    tools = _tools(registry)

    first = json.loads(_invoke(tools["expert_activate"], expert_id="zara:expert/todo"))
    second = json.loads(_invoke(tools["expert_activate"], expert_id="zara:expert/todo"))
    assert second["activation_id"] == first["activation_id"]
    assert second["already_active"] is True


def test_invoking_an_expert_without_handler_is_typed_unsupported() -> None:
    registry = _registry_with_todo()
    tools = _tools(registry)

    json.loads(_invoke(tools["expert_activate"], expert_id="zara:expert/android-troubleshooting"))
    result = _invoke(
        tools["expert_invoke"],
        expert_id="zara:expert/android-troubleshooting",
        operation="adb.diagnose",
    )
    assert result.startswith("Error:")
    assert "unsupported_operation" in result
    assert "no registered handler" in result


def test_future_protocol_major_is_not_activatable() -> None:
    registry = _registry_with_todo()
    tools = _tools(registry)

    result = _invoke(tools["expert_activate"], expert_id="zara:expert/future")
    assert result.startswith("Error:")
    assert "not registered" in result


def test_match_routes_symbolic_goals_without_a_model() -> None:
    registry = _registry_with_todo()
    tools = _tools(registry)

    matched = json.loads(_invoke(tools["expert_match"], goal_text="rank my todos"))
    assert matched["expert_id"] == "zara:expert/todo"

    assert "no expert matches" in _invoke(tools["expert_match"], goal_text="zzz qqq")


def test_describe_unknown_expert_is_typed_unavailable() -> None:
    registry = _registry_with_todo()
    tools = _tools(registry)

    result = _invoke(tools["expert_describe"], expert_id="zara:expert/missing")
    assert result.startswith("Error:")
    assert "not registered" in result


def test_resolve_expert_roots_covers_project_xdg_and_extras(
    tmp_path: Path, monkeypatch
) -> None:
    xdg = tmp_path / "xdg"
    (xdg / "zarathushtra" / "experts").mkdir(parents=True)
    monkeypatch.setenv("XDG_CONFIG_HOME", str(xdg))
    project = tmp_path / "repo"
    (project / ".zara" / "experts").mkdir(parents=True)
    extra = tmp_path / "dotfiles" / ".zara" / "experts"
    extra.mkdir(parents=True)
    missing = tmp_path / "missing" / "experts"
    symlinked = tmp_path / "symlink-experts"
    symlinked.symlink_to(extra, target_is_directory=True)

    roots = resolve_expert_roots(project, [str(extra), str(missing), str(symlinked)])

    assert roots == [project / ".zara" / "experts", xdg / "zarathushtra" / "experts", extra]


def test_resolve_expert_roots_defaults_to_xdg_fallback(
    tmp_path: Path, monkeypatch
) -> None:
    monkeypatch.delenv("XDG_CONFIG_HOME", raising=False)
    monkeypatch.setenv("HOME", str(tmp_path))
    user_experts = tmp_path / ".config" / "zarathushtra" / "experts"
    user_experts.mkdir(parents=True)

    roots = resolve_expert_roots(tmp_path / "repo", [])

    assert roots == [user_experts]


def test_default_config_enables_experts_out_of_the_box(tmp_path: Path) -> None:
    config_path = tmp_path / "config.toml"
    config_path.write_text(DEFAULT_CONFIG_TOML)
    config = ZaraConfig(str(config_path))

    experts_config = config.get_experts_config()
    assert experts_config["enabled"] is True
    assert experts_config["extra_roots"] == ["~/.dotfiles/.zara/experts"]


def test_expert_tools_load_out_of_the_box(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / "config.toml"
    config_path.write_text(DEFAULT_CONFIG_TOML)
    xdg = tmp_path / "xdg"
    (xdg / "zarathushtra" / "experts").mkdir(parents=True)
    monkeypatch.setenv("XDG_CONFIG_HOME", str(xdg))

    config = ZaraConfig(str(config_path))
    registry = ToolRegistry(config=config)
    registry.load_builtin_tools()

    names = set(registry.list_tools())
    assert PROTOCOL_TOOL_NAMES.issubset(names)
    assert PACK_TOOL_NAMES.issubset(names)


def test_disabling_experts_removes_every_expert_tool(tmp_path: Path, monkeypatch) -> None:
    config_path = tmp_path / "config.toml"
    config_path.write_text(DEFAULT_CONFIG_TOML)
    xdg = tmp_path / "xdg"
    (xdg / "zarathushtra" / "experts").mkdir(parents=True)
    monkeypatch.setenv("XDG_CONFIG_HOME", str(xdg))

    config = ZaraConfig(str(config_path))
    config._config["experts"]["enabled"] = False
    registry = ToolRegistry(config=config)
    registry.load_builtin_tools()

    assert ALL_EXPERT_TOOL_NAMES.isdisjoint(registry.list_tools())


def test_get_builtin_tools_wires_experts_only_through_expert_config(
    tmp_path: Path,
) -> None:
    pack_root = tmp_path / "packs"
    pack_root.mkdir()

    none_tools = {
        tool.name for tool in get_builtin_tools()
    }
    assert ALL_EXPERT_TOOL_NAMES.isdisjoint(none_tools)

    registry = _registry_with_todo()
    wired = {
        tool.name
        for tool in get_builtin_tools(
            expert_config={"registry": registry, "roots": [pack_root], "workspace": "ws"}
        )
    }
    assert ALL_EXPERT_TOOL_NAMES.issubset(wired)
