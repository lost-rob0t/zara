from __future__ import annotations

import pytest

from zara.runtime.symbols import (
    ProgrammableSymbolRegistry,
    SymbolLookupError,
    SymbolRegistrationError,
    SymbolSpec,
)


def test_package_override_is_introspectable_and_unload_restores_previous_definition():
    registry = ProgrammableSymbolRegistry()

    registry.register(
        symbol="zara:home/open",
        kind="command",
        owner="core:home",
        layer="core",
        value="core-home",
        docs="Open the shipped home surface.",
    )
    package_id = registry.register(
        symbol="zara:home/open",
        kind="command",
        owner="plugin:org-home",
        layer="package",
        value="org-home",
        docs="Open the Org-first home surface.",
    )

    assert registry.get("zara:home/open") == "org-home"

    chain = registry.describe("zara:home/open")
    assert [(item.owner, item.active) for item in chain] == [
        ("core:home", False),
        ("plugin:org-home", True),
    ]

    assert registry.unregister(package_id, owner="plugin:org-home") is True
    assert registry.get("zara:home/open") == "core-home"
    assert registry.describe("zara:home/open")[0].active is True


def test_precedence_layers_make_user_configuration_win_over_packages():
    registry = ProgrammableSymbolRegistry()

    registry.register(
        symbol="org:daily/open",
        kind="command",
        owner="user:init",
        layer="user",
        value="user-daily",
    )
    registry.register(
        symbol="org:daily/open",
        kind="command",
        owner="plugin:logseq-daily",
        layer="package",
        priority=100_000,
        value="package-daily",
    )

    assert registry.get("org:daily/open") == "user-daily"


def test_same_layer_priority_then_later_registration_is_deterministic():
    registry = ProgrammableSymbolRegistry()

    registry.register(
        symbol="zara:startup",
        kind="command",
        owner="plugin:first",
        value="first",
        priority=5,
    )
    registry.register(
        symbol="zara:startup",
        kind="command",
        owner="plugin:second",
        value="second",
        priority=5,
    )
    registry.register(
        symbol="zara:startup",
        kind="command",
        owner="plugin:lower",
        value="lower",
        priority=4,
    )

    assert registry.get("zara:startup") == "second"


def test_replace_owner_is_failure_atomic_and_keeps_last_good_generation():
    registry = ProgrammableSymbolRegistry()
    registry.replace_owner(
        "plugin:daily",
        (
            SymbolSpec("org:daily/open", "command", "v1-open"),
            SymbolSpec("org:daily/capture", "command", "v1-capture"),
        ),
    )

    with pytest.raises(SymbolRegistrationError, match="duplicate symbol"):
        registry.replace_owner(
            "plugin:daily",
            (
                SymbolSpec("org:daily/open", "command", "bad-1"),
                SymbolSpec("org:daily/open", "command", "bad-2"),
            ),
        )

    assert registry.get("org:daily/open") == "v1-open"
    assert registry.get("org:daily/capture") == "v1-capture"


def test_replace_owner_swaps_generation_and_removes_stale_exports():
    registry = ProgrammableSymbolRegistry()
    registry.replace_owner(
        "plugin:daily",
        (
            SymbolSpec("org:daily/open", "command", "v1-open"),
            SymbolSpec("org:daily/old-command", "command", "v1-old"),
        ),
    )

    ids = registry.replace_owner(
        "plugin:daily",
        (
            SymbolSpec(
                "org:daily/open",
                "command",
                "v2-open",
                docs="Open the continuous Daily stream.",
                capabilities=("org.read",),
                source="daily/plugin.pl",
            ),
        ),
    )

    assert len(ids) == 1
    assert registry.get("org:daily/open") == "v2-open"
    with pytest.raises(SymbolLookupError):
        registry.resolve("org:daily/old-command")

    diagnostic = registry.describe("org:daily/open")[0]
    assert diagnostic.owner == "plugin:daily"
    assert diagnostic.active is True
    assert diagnostic.docs == "Open the continuous Daily stream."
    assert diagnostic.capabilities == ("org.read",)
    assert diagnostic.source == "daily/plugin.pl"


def test_symbol_kind_cannot_silently_change_across_packages():
    registry = ProgrammableSymbolRegistry()
    registry.register(
        symbol="zara:model/select",
        kind="command",
        owner="core:model",
        layer="core",
        value=lambda: None,
    )

    with pytest.raises(SymbolRegistrationError, match="already registered as"):
        registry.register(
            symbol="zara:model/select",
            kind="variable",
            owner="plugin:bad",
            value="oops",
        )

    assert registry.resolve("zara:model/select").kind == "command"


def test_owner_guard_prevents_one_package_from_unregistering_another():
    registry = ProgrammableSymbolRegistry()
    registration_id = registry.register(
        symbol="org:roam/open",
        kind="command",
        owner="plugin:roam",
        value="roam",
    )

    assert registry.unregister(registration_id, owner="plugin:daily") is False
    assert registry.get("org:roam/open") == "roam"
    assert registry.unregister(registration_id, owner="plugin:roam") is True


def test_clear_owner_restores_other_package_and_core_definitions():
    registry = ProgrammableSymbolRegistry()
    registry.register(
        symbol="zara:home/open",
        kind="command",
        owner="core:home",
        layer="core",
        value="core",
    )
    registry.register(
        symbol="zara:home/open",
        kind="command",
        owner="plugin:a",
        value="a",
    )
    registry.register(
        symbol="zara:home/open",
        kind="command",
        owner="plugin:b",
        value="b",
    )
    registry.register(
        symbol="zara:other",
        kind="command",
        owner="plugin:b",
        value="other",
    )

    assert registry.get("zara:home/open") == "b"
    assert registry.clear_owner("plugin:b") == 2
    assert registry.get("zara:home/open") == "a"
    assert registry.clear_owner("plugin:a") == 1
    assert registry.get("zara:home/open") == "core"


def test_symbols_can_be_filtered_by_kind_without_exposing_values():
    registry = ProgrammableSymbolRegistry()
    registry.register(
        symbol="zara:home/open",
        kind="command",
        owner="core:home",
        layer="core",
        value=object(),
    )
    registry.register(
        symbol="zara:theme",
        kind="variable",
        owner="core:theme",
        layer="core",
        value=object(),
    )

    assert registry.symbols() == ("zara:home/open", "zara:theme")
    assert registry.symbols(kind="command") == ("zara:home/open",)


def test_separate_application_registries_do_not_share_package_state():
    editor = ProgrammableSymbolRegistry()
    todo = ProgrammableSymbolRegistry()

    editor.register(
        symbol="org:daily/open",
        kind="command",
        owner="plugin:logseq-daily",
        layer="package",
        value="editor-daily",
    )
    todo.register(
        symbol="org:daily/open",
        kind="command",
        owner="core:todo",
        layer="core",
        value="todo-daily",
    )

    assert editor.get("org:daily/open") == "editor-daily"
    assert todo.get("org:daily/open") == "todo-daily"

    editor.clear_owner("plugin:logseq-daily")

    with pytest.raises(SymbolLookupError):
        editor.resolve("org:daily/open")
    assert todo.get("org:daily/open") == "todo-daily"


@pytest.mark.parametrize(
    "kwargs",
    (
        {"symbol": "org:\x00daily/open", "kind": "command", "owner": "plugin:daily"},
        {"symbol": "org:dáilies/open", "kind": "command", "owner": "plugin:daily"},
        {"symbol": "org:daily/open", "kind": "com\x00mand", "owner": "plugin:daily"},
        {"symbol": "org:daily/open", "kind": "cømmand", "owner": "plugin:daily"},
        {"symbol": "org:daily/open", "kind": "command", "owner": "plugin:bad owner"},
        {"symbol": "org:daily/open", "kind": "command", "owner": "plugin:bäd"},
    ),
)
def test_registry_rejects_nonportable_namespace_identifiers_before_mutation(kwargs):
    registry = ProgrammableSymbolRegistry()

    with pytest.raises(SymbolRegistrationError, match="portable"):
        registry.register(**kwargs, value="bad")

    assert registry.symbols() == ()
