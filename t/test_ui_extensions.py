from __future__ import annotations

import textwrap

import pytest

from zara.ui.extensions import (
    UiContribution,
    UiContributionKind,
    UiExtensionRegistry,
    UiInitLoader,
    UiPlatform,
    UiSlot,
    UserUiInitLoadError,
)


def test_registry_replaces_owner_atomically_and_orders_deterministically():
    registry = UiExtensionRegistry()
    registry.replace_owner(
        "plugin:zeta",
        [
            UiContribution(
                id="late",
                slot=UiSlot.DRAWER,
                kind=UiContributionKind.BUTTON,
                label="Late",
                action="submit:late",
                priority=90,
            ),
            UiContribution(
                id="early",
                slot=UiSlot.DRAWER,
                kind=UiContributionKind.BUTTON,
                label="Early",
                action="submit:early",
                priority=10,
            ),
        ],
    )

    assert [item.id for item in registry.for_platform(UiPlatform.DESKTOP)] == [
        "early",
        "late",
    ]
    assert {item.owner for item in registry.snapshot()} == {"plugin:zeta"}

    with pytest.raises(ValueError, match="duplicate UI contribution"):
        registry.replace_owner(
            "plugin:zeta",
            [
                UiContribution(
                    id="same",
                    slot=UiSlot.DRAWER,
                    kind=UiContributionKind.TEXT,
                    label="one",
                ),
                UiContribution(
                    id="same",
                    slot=UiSlot.DRAWER,
                    kind=UiContributionKind.TEXT,
                    label="two",
                ),
            ],
        )

    assert [item.id for item in registry.snapshot()] == ["early", "late"]


def test_registry_filters_platform_and_slot():
    registry = UiExtensionRegistry()
    registry.replace_owner(
        "user:init",
        [
            UiContribution(
                id="both",
                slot=UiSlot.CHAT_TOP,
                kind=UiContributionKind.TEXT,
                label="both",
                platforms=(UiPlatform.DESKTOP, UiPlatform.ANDROID),
            ),
            UiContribution(
                id="android-only",
                slot=UiSlot.CHAT_TOP,
                kind=UiContributionKind.TEXT,
                label="android",
                platforms=(UiPlatform.ANDROID,),
            ),
        ],
    )

    assert [item.id for item in registry.for_platform(UiPlatform.DESKTOP, UiSlot.CHAT_TOP)] == [
        "both"
    ]
    assert [item.id for item in registry.for_platform(UiPlatform.ANDROID, UiSlot.CHAT_TOP)] == [
        "android-only",
        "both",
    ]


def test_python_init_loader_uses_same_portable_ui_add_call(tmp_path):
    init_file = tmp_path / "init.py"
    init_file.write_text(
        textwrap.dedent(
            '''
            def register(ui):
                ui.add("logic-shortcut", "drawer", "surface", "My Logic", "route:logic", 20, ["desktop", "android"])
                ui.add("query", "chat.top", "button", "Run query", "submit:? - demo(Result)", 5, ["desktop"])
            '''
        ),
        encoding="utf-8",
    )
    registry = UiExtensionRegistry()
    loader = UiInitLoader(config_dir=tmp_path, registry=registry)

    assert loader.load() == 2
    desktop = registry.for_platform(UiPlatform.DESKTOP)
    android = registry.for_platform(UiPlatform.ANDROID)
    assert [(item.id, item.slot.value, item.kind.value) for item in desktop] == [
        ("query", "chat.top", "button"),
        ("logic-shortcut", "drawer", "surface"),
    ]
    assert [item.id for item in android] == ["logic-shortcut"]


def test_python_init_reload_is_failure_atomic(tmp_path):
    init_file = tmp_path / "init.py"
    init_file.write_text(
        'def register(ui):\n    ui.add("stable", "drawer", "text", "Stable", "", 1, ["desktop"])\n',
        encoding="utf-8",
    )
    registry = UiExtensionRegistry()
    loader = UiInitLoader(config_dir=tmp_path, registry=registry)
    assert loader.load() == 1

    init_file.write_text('def register(ui):\n    raise RuntimeError("boom")\n', encoding="utf-8")
    with pytest.raises(UserUiInitLoadError, match="failed to load init.py"):
        loader.reload()

    assert [item.id for item in registry.snapshot()] == ["stable"]


def test_invalid_ui_schema_fails_closed():
    with pytest.raises(ValueError, match="UI action"):
        UiContribution(
            id="bad",
            slot=UiSlot.DRAWER,
            kind=UiContributionKind.BUTTON,
            label="Bad",
            action="shell:rm -rf /",
        )

    with pytest.raises(ValueError, match="requires an action"):
        UiContribution(
            id="missing-action",
            slot=UiSlot.DRAWER,
            kind=UiContributionKind.BUTTON,
            label="Missing",
        )
