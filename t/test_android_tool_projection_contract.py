from __future__ import annotations

import json
from pathlib import Path

from zara.agent.approval import valid_tool_name


CATALOG_PATH = Path(__file__).resolve().parents[1] / "protocol" / "android-tools.v1.json"
REQUESTED = {
    "sms_compose",
    "sms_send",
    "sms_read",
    "contacts_list",
    "flashlight_set",
    "wifi_status",
    "wifi_panel",
    "wifi_set_enabled",
    "location_current",
    "maps_search",
    "maps_directions",
    "maps_show",
}


def _catalog() -> dict:
    return json.loads(CATALOG_PATH.read_text(encoding="utf-8"))


def test_android_tool_catalog_is_one_versioned_language_neutral_source():
    payload = _catalog()

    assert payload["schema"] == "ZARA-ANDROID-TOOLS/1"
    tools = payload["tools"]
    assert isinstance(tools, list)
    assert tools

    capabilities = [item["capability"] for item in tools]
    assert len(capabilities) == len(set(capabilities))
    assert REQUESTED <= set(capabilities)


def test_each_android_descriptor_projects_coherent_prolog_and_llm_names():
    tools = _catalog()["tools"]
    llm_names = []
    prolog_actions = []

    for descriptor in tools:
        capability = descriptor["capability"]
        prolog_action = descriptor["prolog_action"]
        llm_tool = descriptor["llm_tool"]

        assert prolog_action == capability
        assert llm_tool == f"android_{capability}"
        assert valid_tool_name(llm_tool)
        assert descriptor["side_effect"] in {"read_only", "local", "external"}
        assert descriptor["authority"] in {"standard", "restricted", "elevated"}
        assert isinstance(descriptor["description"], str) and descriptor["description"].strip()
        assert isinstance(descriptor["arguments"], dict)
        assert isinstance(descriptor["result"], dict)

        llm_names.append(llm_tool)
        prolog_actions.append(prolog_action)

    assert len(llm_names) == len(set(llm_names))
    assert len(prolog_actions) == len(set(prolog_actions))


def test_standard_catalog_does_not_smuggle_generic_shell_eval_or_raw_intent_authority():
    rendered = json.dumps(_catalog(), sort_keys=True).lower()

    forbidden = (
        "raw_shell",
        "shell_command",
        "runtime.exec",
        "java.lang.runtime",
        "eval(",
        "raw_intent",
        "arbitrary_intent",
    )
    assert all(marker not in rendered for marker in forbidden)
