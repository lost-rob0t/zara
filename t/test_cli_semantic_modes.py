"""CLI mode isolation regressions for issue #160."""

from __future__ import annotations

from types import SimpleNamespace

import pytest

import zara.__main__ as cli


class FakeConfig:
    def get_section(self, name):
        if name == "stt":
            return {
                "provider": "faster-whisper",
                "model": "small",
                "device": "amd",
            }
        return {}


def test_typed_command_does_not_initialize_stt(monkeypatch, capsys):
    monkeypatch.setattr(cli, "init_config", lambda: FakeConfig())
    monkeypatch.setattr(cli, "_default_daemon_endpoint", lambda: "ipc:///tmp/zara-test")
    seen = []

    def fake_connected(endpoint, command):
        seen.append((endpoint, command))
        return 0

    monkeypatch.setattr(cli, "_run_connected_text", fake_connected)
    monkeypatch.setattr(
        cli,
        "_resolve_stt_runtime",
        lambda args: (_ for _ in ()).throw(AssertionError("STT initialized")),
    )
    monkeypatch.setattr(
        cli.sys,
        "argv",
        ["zara", "set", "a", "timer", "for", "2", "hours"],
    )

    with pytest.raises(SystemExit) as stopped:
        cli.main()

    assert stopped.value.code == 0
    assert seen == [("ipc:///tmp/zara-test", "set a timer for 2 hours")]
    assert "AMD GPU STT requested" not in capsys.readouterr().err


def test_stt_runtime_still_routes_amd_for_voice_modes(capsys):
    args = SimpleNamespace(
        stt_provider="faster-whisper",
        device="amd",
        model="small",
    )

    provider, device, model = cli._resolve_stt_runtime(args)

    assert provider == "whisper-cpp"
    assert device == "vulkan"
    assert model
    assert "AMD GPU STT requested" in capsys.readouterr().err
