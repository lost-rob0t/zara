from __future__ import annotations

import asyncio
import sys
from types import SimpleNamespace

import numpy as np
import pytest
import requests

import zara.console as console
import zara.noaa as noaa
import zara.notifications as notifications
import zara.transcription as transcription
import zara.tts.elevenlabs as eleven


def test_noaa_helpers_and_forecast_paths(monkeypatch):
    assert noaa._normalize_coordinate(None) is None
    assert noaa._normalize_coordinate("39.9") == 39.9
    assert noaa._normalize_coordinate("bad") is None
    assert noaa._build_headers(None)["User-Agent"] == "ZarathushtraWeather/1.0"
    assert noaa._build_headers("agent")["User-Agent"] == "agent"

    assert noaa._format_period({}, "auto") == "No forecast data available."
    line = noaa._format_period({
        "name": "Tonight", "temperature": 70, "temperatureUnit": "F",
        "shortForecast": "Clear", "windSpeed": "5 mph", "windDirection": "W",
    }, "si")
    assert "Tonight:" in line and "70F" in line and "Wind 5 mph W" in line
    assert "NOAA reports in US units" in line

    responses = {
        "https://api.weather.gov/points/1.0,2.0": {"properties": {"forecast": "forecast-url"}},
        "forecast-url": {"properties": {"periods": [
            {"name": "One", "temperature": 1, "temperatureUnit": "F"},
            {"name": "Two", "temperature": 2, "temperatureUnit": "F"},
        ]}},
    }
    monkeypatch.setattr(noaa, "_fetch_json", lambda url, _headers: responses[url])
    assert "One:" in noaa._get_forecast(1.0, 2.0, "auto", None, 1)

    monkeypatch.setattr(noaa, "_fetch_json", lambda _url, _headers: {"properties": {}})
    assert "No forecast endpoint" in noaa._get_forecast(1.0, 2.0, "auto", None, 1)

    calls = iter([{"properties": {"forecast": "x"}}, {"properties": {"periods": []}}])
    monkeypatch.setattr(noaa, "_fetch_json", lambda _url, _headers: next(calls))
    assert "No forecast periods" in noaa._get_forecast(1.0, 2.0, "auto", None, 1)


def test_noaa_public_config_and_errors(monkeypatch):
    class Cfg:
        def get_section(self, _name):
            return {"default_latitude": "1", "default_longitude": "2", "user_agent": "ua"}

    import zara.config as config_module
    monkeypatch.setattr(config_module, "get_config", lambda: Cfg())
    assert noaa._get_noaa_config()["user_agent"] == "ua"

    monkeypatch.setattr(noaa, "_get_noaa_config", lambda: {})
    assert "Missing latitude/longitude" in noaa.get_noaa_weather()

    monkeypatch.setattr(noaa, "_get_noaa_config", lambda: {"default_latitude": 1, "default_longitude": 2})
    monkeypatch.setattr(noaa, "_get_forecast", lambda *_args: "ok")
    assert noaa.get_noaa_weather(2, "si") == "ok"

    def request_error(*_args):
        raise requests.RequestException("offline")

    monkeypatch.setattr(noaa, "_get_forecast", request_error)
    assert "NOAA request failed" in noaa.get_noaa_weather()

    monkeypatch.setattr(noaa, "_get_forecast", lambda *_args: (_ for _ in ()).throw(RuntimeError("boom")))
    assert "Failed to fetch NOAA forecast" in noaa.get_noaa_weather()
    assert noaa.build_noaa_weather_tool().name == "get_noaa_weather"


def test_console_init_config_execute_and_repl(monkeypatch, tmp_path, capsys):
    main_pl = tmp_path / "main.pl"
    main_pl.write_text("% test")

    class Engine:
        def __init__(self, path):
            self.path = path
            self.goals = []
            self.commands = []

        def query_once(self, goal):
            self.goals.append(goal)
            return {}

        def execute_command(self, text):
            self.commands.append(text)
            return text != "fail"

    class Cfg:
        def __init__(self, enabled=True):
            self.enabled = enabled

        def get_section(self, _name):
            return {"enabled": self.enabled}

    monkeypatch.setattr(console, "PrologEngine", Engine)
    monkeypatch.setattr(console, "get_config", lambda: Cfg(False))
    instance = console.ZaraConsole(main_pl)
    assert instance.engine.goals == ["kb_intents:set_todo_intents_enabled(false)"]
    assert instance.execute_command("hello") is True

    inputs = iter(["", "hello", "quit"])
    monkeypatch.setattr("builtins.input", lambda _prompt: next(inputs))
    instance.repl()
    out = capsys.readouterr().out
    assert "Thus spoke" in out and "prophet has spoken" in out
    assert instance.engine.commands[-1] == "hello"

    monkeypatch.setattr(console, "get_config", lambda: Cfg("false"))
    with pytest.raises(ValueError, match="todo.enabled"):
        console.ZaraConsole(main_pl)
    with pytest.raises(FileNotFoundError):
        console.ZaraConsole(tmp_path / "missing.pl")


def test_console_find_and_main_exit_paths(monkeypatch, tmp_path, capsys):
    prefix_main = tmp_path / "share" / "zarathushtra" / "main.pl"
    prefix_main.parent.mkdir(parents=True)
    prefix_main.write_text("% main")
    monkeypatch.setattr(sys, "prefix", str(tmp_path))
    assert console.find_main_pl() == prefix_main

    class FakeConsole:
        def __init__(self, main_file=None):
            self.main_file = main_file

        def execute_command(self, text):
            return text != "bad"

        def repl(self):
            return None

    monkeypatch.setattr(console, "ZaraConsole", FakeConsole)
    monkeypatch.setattr(sys, "argv", ["zara-console", "hello"])
    with pytest.raises(SystemExit) as exc:
        console.main()
    assert exc.value.code == 0

    monkeypatch.setattr(sys, "argv", ["zara-console", "bad"])
    with pytest.raises(SystemExit) as exc:
        console.main()
    assert exc.value.code == 1

    class Missing:
        def __init__(self, **_kwargs):
            raise FileNotFoundError("gone")

    monkeypatch.setattr(console, "ZaraConsole", Missing)
    monkeypatch.setattr(sys, "argv", ["zara-console"])
    with pytest.raises(SystemExit) as exc:
        console.main()
    assert exc.value.code == 1
    assert "Error: gone" in capsys.readouterr().err


def test_transcription_device_init_and_sync_paths(monkeypatch):
    assert transcription.normalize_device(" CPU ") == "cpu"
    assert transcription.normalize_device("cuda") == "cuda"
    assert transcription.normalize_device("rocm") == "cuda"
    with pytest.raises(ValueError, match="Unsupported"):
        transcription.normalize_device("tpu")

    created = []

    class Model:
        def __init__(self, *args, **kwargs):
            created.append((args, kwargs))
            self.mode = "normal"

        def transcribe(self, *_args, **_kwargs):
            if self.mode == "error":
                raise RuntimeError("boom")
            text = "thank you" if self.mode == "thanks" else " hello "
            return [SimpleNamespace(text=text), SimpleNamespace(text="")], None

    monkeypatch.setattr(transcription, "WhisperModel", Model)
    monkeypatch.setattr(transcription.os, "cpu_count", lambda: 4)
    t = transcription.Transcriber("tiny", "cpu", None)
    assert created[-1][1]["compute_type"] == "int8"
    assert created[-1][1]["cpu_threads"] == 4

    silence = np.zeros(32, dtype=np.int16).tobytes()
    assert t.transcribe(silence) == ""
    loud = np.full(32, 2000, dtype=np.int16).tobytes()
    assert t.transcribe(loud) == "hello"
    t.model.mode = "thanks"
    assert t.transcribe(loud) == ""
    t.model.mode = "error"
    assert t.transcribe(loud) == ""

    t.model.mode = "normal"
    assert asyncio.run(t.transcribe_async(loud)) == "hello"

    transcription.Transcriber("tiny", "amd", 0)
    assert created[-1][1]["compute_type"] == "float16"
    assert created[-1][1]["cpu_threads"] == 1


def test_notifications_async_and_sync(monkeypatch):
    class Proc:
        def __init__(self, code, stdout=b"", stderr=b""):
            self.returncode = code
            self.stdout = stdout
            self.stderr = stderr

        async def communicate(self):
            return self.stdout, self.stderr

    async def good(*_args, **_kwargs):
        return Proc(0)

    monkeypatch.setattr(notifications.asyncio, "create_subprocess_exec", good)
    assert asyncio.run(notifications.send_notification_async("t", "m")) is True

    async def bad(*_args, **_kwargs):
        return Proc(1, stdout=b"oops")

    monkeypatch.setattr(notifications.asyncio, "create_subprocess_exec", bad)
    assert asyncio.run(notifications.send_notification_async("t", "m")) is False

    async def explode(*_args, **_kwargs):
        raise RuntimeError("no notify")

    monkeypatch.setattr(notifications.asyncio, "create_subprocess_exec", explode)
    assert asyncio.run(notifications.send_notification_async("t", "m")) is False

    monkeypatch.setattr(notifications.asyncio, "get_event_loop", lambda: SimpleNamespace(is_running=lambda: False))
    monkeypatch.setattr(notifications.asyncio, "run", lambda coro: (coro.close(), True)[1])
    assert notifications.send_notification("t", "m") is True

    scheduled = []
    monkeypatch.setattr(notifications.asyncio, "get_event_loop", lambda: SimpleNamespace(is_running=lambda: True))
    monkeypatch.setattr(notifications.asyncio, "create_task", lambda coro: (scheduled.append(coro), coro.close()))
    assert notifications.send_notification("t", "m") is True
    assert len(scheduled) == 1

    monkeypatch.setattr(notifications.asyncio, "get_event_loop", lambda: (_ for _ in ()).throw(RuntimeError("loop")))
    assert notifications.send_notification("t", "m") is False


def test_elevenlabs_client_settings_synthesis_and_close(monkeypatch):
    eleven._client = None

    class TextToSpeech:
        def convert(self, **kwargs):
            assert kwargs["voice_id"] == "voice"
            return [b"a", b"b"]

        def stream(self, **_kwargs):
            return iter([b"a", b"", b"b"])

    class Client:
        def __init__(self, api_key=None):
            self.api_key = api_key
            self.text_to_speech = TextToSpeech()
            self.closed = False

        def close(self):
            self.closed = True

    monkeypatch.setattr(eleven, "ElevenLabs", Client)
    one = eleven._ensure_client("key")
    assert one is eleven._ensure_client("other")
    assert one.api_key == "key"
    assert eleven._get_voice_settings({}) is None
    assert eleven._get_voice_settings({"elevenlabs_stability": 0.1}) is not None

    cfg = {"elevenlabs_api_key": "key", "elevenlabs_voice_id": "voice"}
    assert asyncio.run(eleven.synthesize("hello", cfg)) == b"ab"

    async def collect():
        return [chunk async for chunk in eleven.synthesize_stream("hello", cfg)]

    assert asyncio.run(collect()) == [b"a", b"b"]
    asyncio.run(eleven.close())
    assert eleven._client is None and one.closed is True

    with pytest.raises(AssertionError, match="non-empty"):
        asyncio.run(eleven.synthesize("", cfg))
    with pytest.raises(AssertionError, match="api_key"):
        asyncio.run(eleven.synthesize("hello", {"elevenlabs_voice_id": "voice"}))
    with pytest.raises(AssertionError, match="voice_id"):
        asyncio.run(eleven.synthesize("hello", {"elevenlabs_api_key": "key"}))
