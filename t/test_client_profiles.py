from __future__ import annotations

import pytest

from zara.client_profiles import ClientProfileError, section_for_client, validate_client_profiles
from zara.config import DEFAULT_CONFIG_TOML, ConfigError, ZaraConfig


def _write_config(tmp_path, extra: str) -> ZaraConfig:
    path = tmp_path / "config.toml"
    path.write_text(DEFAULT_CONFIG_TOML + "\n" + extra, encoding="utf-8")
    return ZaraConfig(str(path))


def _clear_llm_env(monkeypatch) -> None:
    for name in (
        "ZARA_CLIENT_ID",
        "ZARA_CLIENT_PROFILE",
        "ZARA_LLM_PROVIDER",
        "ZARA_LLM_MODEL",
        "ZARA_LLM_ENDPOINT",
        "ANTHROPIC_API_KEY",
        "OPENAI_API_KEY",
        "OPENROUTER_API_KEY",
    ):
        monkeypatch.delenv(name, raising=False)


def test_exact_mobile_pin_can_select_openrouter_without_storing_secret(monkeypatch, tmp_path):
    _clear_llm_env(monkeypatch)
    monkeypatch.setenv("OPENROUTER_API_KEY", "from-environment")
    config = _write_config(
        tmp_path,
        """
[client_pins]
"android:phone" = "mobile"

[client_profiles.mobile.llm]
provider = "openrouter"
model = "openai/gpt-5-mini"
""",
    )

    llm = config.get_llm_config(client_id="android:phone")

    assert llm["provider"] == "openrouter"
    assert llm["model"] == "openai/gpt-5-mini"
    assert llm["endpoint"] is None
    assert llm["openrouter_api_key"] == "from-environment"


def test_unknown_client_keeps_base_local_provider(monkeypatch, tmp_path):
    _clear_llm_env(monkeypatch)
    config = _write_config(
        tmp_path,
        """
[client_pins]
"android:phone" = "mobile"

[client_profiles.mobile.llm]
provider = "openrouter"
""",
    )

    assert config.get_llm_config(client_id="desktop:local")["provider"] == "ollama"


def test_explicit_local_profile_can_pin_loopback_llm_serve(monkeypatch, tmp_path):
    _clear_llm_env(monkeypatch)
    config = _write_config(
        tmp_path,
        """
[client_profiles.local-phone.llm]
provider = "ollama"
endpoint = "http://127.0.0.1:11434/api/chat"
model = "zara-local"
""",
    )

    llm = config.get_llm_config(profile_name="local-phone")

    assert llm["provider"] == "ollama"
    assert llm["endpoint"] == "http://127.0.0.1:11434/api/chat"
    assert llm["model"] == "zara-local"


def test_profile_secret_fields_fail_closed(tmp_path):
    with pytest.raises(ConfigError, match="unsupported/secret"):
        _write_config(
            tmp_path,
            """
[client_profiles.mobile.llm]
provider = "openrouter"
openrouter_api_key = "must-not-live-here"
""",
        )


def test_pin_must_reference_existing_profile():
    config = {"client_pins": {"android:phone": "missing"}, "client_profiles": {}}
    with pytest.raises(ClientProfileError, match="unknown profile"):
        validate_client_profiles(config)


def test_overlay_does_not_mutate_base_mapping():
    config = {
        "llm": {"provider": "ollama", "model": "base"},
        "client_pins": {"android:phone": "mobile"},
        "client_profiles": {"mobile": {"llm": {"provider": "openrouter"}}},
    }
    validate_client_profiles(config)

    resolved = section_for_client(config, "llm", client_id="android:phone")

    assert resolved == {"provider": "openrouter", "model": "base"}
    assert config["llm"]["provider"] == "ollama"
