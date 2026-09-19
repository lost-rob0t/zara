from __future__ import annotations

import importlib.util
from pathlib import Path
import sys
import xml.etree.ElementTree as ET


ROOT = Path(__file__).resolve().parents[1]
INTEGRATION = ROOT / "android" / "integration"
ORG_ACCEPTANCE = INTEGRATION / "org_device_acceptance.py"


def _load_org_acceptance_module():
    spec = importlib.util.spec_from_file_location(
        "zara_org_device_acceptance_picker_test",
        ORG_ACCEPTANCE,
    )
    assert spec is not None
    assert spec.loader is not None
    sys.path.insert(0, str(INTEGRATION))
    try:
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module
    finally:
        sys.path.pop(0)


def test_picker_exact_action_ignores_permission_prompt_body(monkeypatch, tmp_path) -> None:
    module = _load_org_acceptance_module()
    device = module.Device("emulator-5554", tmp_path)
    hierarchy = ET.fromstring(
        "<hierarchy>"
        '<node text="Allow Org to access files in ZaraOrgAcceptance?" bounds="[0,0][100,20]" />'
        '<node text="ALLOW" bounds="[20,40][90,80]" />'
        "</hierarchy>"
    )
    monkeypatch.setattr(module, "_picker_nodes", lambda _device: tuple(hierarchy.iter("node")))

    node = module._find_picker_action(device, "Allow")

    assert node is not None
    assert node.get("text") == "ALLOW"
