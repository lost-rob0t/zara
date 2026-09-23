from __future__ import annotations

import hashlib
import json

import pytest

from android.integration.device_acceptance import Device


PNG = b"\x89PNG\r\n\x1a\nrendered-plugin-state"
UI_XML = """<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>
<hierarchy rotation=\"0\">
  <node text=\"Plugins\" content-desc=\"\" class=\"android.widget.TextView\" clickable=\"false\" enabled=\"true\" bounds=\"[0,0][120,40]\" />
  <node text=\"Choose APK\" content-desc=\"\" class=\"android.widget.Button\" clickable=\"true\" enabled=\"true\" bounds=\"[0,40][180,100]\" />
</hierarchy>
"""


def _fake_adb(*arguments: str, binary: bool = False):
    if arguments == ("exec-out", "screencap", "-p"):
        assert binary is True
        return PNG
    if arguments[:3] == ("shell", "rm", "-f"):
        return ""
    if arguments[:3] == ("shell", "uiautomator", "dump"):
        return "UI hierchary dumped"
    if arguments[:2] == ("shell", "cat"):
        return UI_XML
    raise AssertionError(f"unexpected adb call: {arguments!r}")


def test_capture_retains_same_state_text_twin_and_asserted_action_hash(tmp_path):
    device = Device("emulator-test", tmp_path)
    device.adb = _fake_adb  # type: ignore[method-assign]

    device.capture("settings-plugins", required_actions=("Choose APK",))

    assert len(device.screenshots) == 1
    evidence = device.screenshots[0]
    assert evidence["state"] == "settings-plugins"
    assert evidence["file"] == "settings-plugins.png"
    assert evidence["sha256"] == hashlib.sha256(PNG).hexdigest()
    assert evidence["text_twin_file"] == "settings-plugins.ui.json"
    assert evidence["asserted_actions"] == ["Choose APK"]

    twin_bytes = (tmp_path / evidence["text_twin_file"]).read_bytes()
    assert evidence["text_twin_sha256"] == hashlib.sha256(twin_bytes).hexdigest()
    twin = json.loads(twin_bytes)
    assert twin["state"] == "settings-plugins"
    assert twin["asserted_actions"] == ["Choose APK"]
    assert any(node["text"] == "Plugins" for node in twin["nodes"])
    action = next(node for node in twin["nodes"] if node["text"] == "Choose APK")
    assert action["clickable"] is True
    assert action["enabled"] is True


@pytest.mark.parametrize(
    ("label", "xml"),
    [
        (
            "Missing action",
            UI_XML.replace(
                '<node text="Choose APK" content-desc="" class="android.widget.Button" clickable="true" enabled="true" bounds="[0,40][180,100]" />',
                "",
            ),
        ),
        (
            "Choose APK",
            UI_XML.replace('clickable="true"', 'clickable="false"'),
        ),
        (
            "Choose APK",
            UI_XML.replace('enabled="true" bounds="[0,40][180,100]"', 'enabled="false" bounds="[0,40][180,100]"'),
        ),
    ],
)
def test_capture_fails_closed_when_required_action_is_not_usable(tmp_path, label, xml):
    device = Device("emulator-test", tmp_path)

    def fake_adb(*arguments: str, binary: bool = False):
        if arguments == ("exec-out", "screencap", "-p"):
            return PNG
        if arguments[:3] == ("shell", "rm", "-f"):
            return ""
        if arguments[:3] == ("shell", "uiautomator", "dump"):
            return "UI hierarchy dumped"
        if arguments[:2] == ("shell", "cat"):
            return xml
        raise AssertionError(f"unexpected adb call: {arguments!r}")

    device.adb = fake_adb  # type: ignore[method-assign]

    with pytest.raises(AssertionError, match="rendered action"):
        device.capture("settings-plugins", required_actions=(label,))

    assert device.screenshots == []
