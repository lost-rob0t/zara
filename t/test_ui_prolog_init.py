from __future__ import annotations

import pytest

from zara.ui.extensions import UiExtensionRegistry, UiPlatform, UiSlot
from zara.ui.prolog_init import PrologUiInitLoader, UserPrologUiInitLoadError


def test_prolog_init_projects_ui_seven_terms(tmp_path):
    (tmp_path / "init.pl").write_text(
        '''
        zara_ui(ui(logic, drawer, surface, "Logic +", "route:logic", 20, [desktop, android])).
        zara_ui(ui(ready, 'chat.top', status, "Symbolic ready", "", 5, [desktop])).
        ''',
        encoding="utf-8",
    )
    registry = UiExtensionRegistry()
    loader = PrologUiInitLoader(config_dir=tmp_path, registry=registry)

    assert loader.load() == 2
    assert [item.id for item in registry.for_platform(UiPlatform.DESKTOP)] == [
        "ready",
        "logic",
    ]
    assert [item.id for item in registry.for_platform(UiPlatform.ANDROID)] == ["logic"]
    assert registry.for_platform(UiPlatform.DESKTOP, UiSlot.DRAWER)[0].owner == "user:init.pl"


def test_prolog_init_reload_is_failure_atomic(tmp_path):
    path = tmp_path / "init.pl"
    path.write_text(
        'zara_ui(ui(stable, drawer, text, "Stable", "", 1, [desktop])).\n',
        encoding="utf-8",
    )
    registry = UiExtensionRegistry()
    loader = PrologUiInitLoader(config_dir=tmp_path, registry=registry)
    assert loader.load() == 1

    path.write_text(
        'zara_ui(ui(bad, drawer, button, "Bad", "shell:nope", 1, [desktop])).\n',
        encoding="utf-8",
    )
    with pytest.raises(UserPrologUiInitLoadError, match="failed to load init.pl"):
        loader.reload()

    assert [item.id for item in registry.snapshot()] == ["stable"]
