from __future__ import annotations

import json

import pytest

from zara.ui.extensions import UiExtensionRegistry, UiPlatform, UiSlot
from zara.ui.plugin_manifests import UiManifestLoadError, UiManifestLoader


def write_manifest(path, *, plugin="notes", action="route:plugins"):
    path.write_text(
        json.dumps(
            {
                "api_version": "1",
                "plugin": plugin,
                "contributions": [
                    {
                        "id": "notes",
                        "slot": "drawer",
                        "kind": "surface",
                        "label": "Notes",
                        "action": action,
                        "priority": 20,
                        "platforms": ["desktop", "android"],
                    },
                    {
                        "id": "status",
                        "slot": "chat.top",
                        "kind": "status",
                        "label": "Notes ready",
                        "priority": 5,
                        "platforms": ["desktop"],
                    },
                ],
            }
        ),
        encoding="utf-8",
    )


def test_plugin_manifest_projects_without_importing_plugin_code(tmp_path):
    plugin_dir = tmp_path / "notes"
    plugin_dir.mkdir()
    write_manifest(plugin_dir / "ui.json")
    (plugin_dir / "plugin.py").write_text(
        'raise RuntimeError("UI discovery must not import plugin code")\n',
        encoding="utf-8",
    )
    registry = UiExtensionRegistry()

    assert UiManifestLoader((tmp_path,), registry=registry).load() == 2
    assert [item.id for item in registry.for_platform(UiPlatform.DESKTOP)] == [
        "status",
        "notes",
    ]
    assert [item.id for item in registry.for_platform(UiPlatform.ANDROID)] == ["notes"]
    assert registry.snapshot()[0].owner == "plugin:notes"


def test_manifest_enablement_comes_from_canonical_provider(tmp_path):
    write_manifest(tmp_path / "notes.ui.json")
    registry = UiExtensionRegistry()

    loader = UiManifestLoader(
        (tmp_path,),
        registry=registry,
        enabled_provider=lambda name: name != "notes",
    )
    assert loader.load() == 0
    assert registry.snapshot() == ()


@pytest.mark.parametrize("layout", ["flat", "directory"])
def test_manifest_cannot_claim_another_plugin_identity(tmp_path, layout):
    if layout == "flat":
        manifest = tmp_path / "notes.ui.json"
    else:
        plugin_dir = tmp_path / "notes"
        plugin_dir.mkdir()
        manifest = plugin_dir / "ui.json"
    write_manifest(manifest, plugin="calendar")
    registry = UiExtensionRegistry()
    loader = UiManifestLoader(
        (tmp_path,),
        registry=registry,
        enabled_provider=lambda _name: True,
    )

    with pytest.raises(UiManifestLoadError, match="failed to load"):
        loader.load()

    assert registry.snapshot() == ()


def test_manifest_reload_is_failure_atomic(tmp_path):
    manifest = tmp_path / "notes.ui.json"
    write_manifest(manifest)
    registry = UiExtensionRegistry()
    loader = UiManifestLoader((tmp_path,), registry=registry)
    assert loader.load() == 2

    write_manifest(manifest, action="shell:unsafe")
    with pytest.raises(UiManifestLoadError, match="failed to load"):
        loader.reload()

    assert [item.id for item in registry.snapshot()] == ["status", "notes"]
    assert [item.id for item in registry.for_platform(UiPlatform.DESKTOP, UiSlot.DRAWER)] == [
        "notes"
    ]
