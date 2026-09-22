import json
import subprocess
from pathlib import Path
from types import SimpleNamespace

import pytest

from zara.voice_expert import VoiceExpert, VoiceSegment, build_voice_tools


class FakeConfig:
    def get_section(self, name):
        if name == "tts":
            return {
                "provider": "qwen3",
                "endpoint": "http://qwen.test",
                "voice": "zara",
            }
        return {}


class FakeProlog:
    def __init__(self, voices=None):
        self.goals = []
        self.voices = list(["zara"] if voices is None else voices)

    def query_once(self, goal):
        self.goals.append(goal)
        if not self.voices:
            return {}
        return {"Voice": self.voices.pop(0)}


def expert(prolog=None):
    value = VoiceExpert(prolog or FakeProlog(), FakeConfig())
    value._available_voices = lambda: ["zara", "alice", "bob"]
    return value


def test_voice_plan_is_prolog_authoritative_per_new_speaker():
    prolog = FakeProlog(["zara", "alice"])
    value = expert(prolog)

    plan = value.plan(
        [
            VoiceSegment(text="Opening.", speaker="narrator", role="narrator"),
            VoiceSegment(text="Hello.", speaker="a", role="dialogue"),
            VoiceSegment(text="Again.", speaker="a", role="dialogue"),
        ]
    )

    assert [row["voice"] for row in plan] == ["zara", "alice", "alice"]
    assert len(prolog.goals) == 2
    assert all("kb_voice_expert:resolve_voice(" in goal for goal in prolog.goals)
    assert '"a","dialogue"' in prolog.goals[1]


def test_voice_plan_never_invents_python_fallback():
    value = expert(FakeProlog([]))

    with pytest.raises(RuntimeError, match="Prolog policy returned no voice"):
        value.plan([VoiceSegment(text="Hello", speaker="narrator", role="narrator")])


def test_single_mode_collapses_to_one_prolog_selected_voice():
    prolog = FakeProlog(["zara"])
    value = expert(prolog)

    plan = value.plan(
        [
            VoiceSegment(text="Narration", speaker="narrator", role="narrator"),
            VoiceSegment(text="Quoted line", speaker="character", role="dialogue"),
        ],
        mode="single",
    )

    assert [row["voice"] for row in plan] == ["zara", "zara"]
    assert len(prolog.goals) == 1


def test_youtube_search_routes_through_delayed_wrapper(monkeypatch):
    value = expert()
    monkeypatch.setattr(value, "_ytdlp_command", lambda: ["/bin/zara-ytdlp"])
    captured = {}

    def fake_run(command, *, timeout):
        captured["command"] = command
        captured["timeout"] = timeout
        return subprocess.CompletedProcess(
            command,
            0,
            stdout=json.dumps(
                {
                    "entries": [
                        {
                            "id": "abc123",
                            "title": "Voice sample",
                            "channel": "Example",
                            "duration": 12,
                            "url": "abc123",
                        }
                    ]
                }
            ),
            stderr="",
        )

    monkeypatch.setattr(value, "_run", fake_run)

    payload = json.loads(value.youtube_search("authorized sample", limit=1))

    assert payload["results"][0]["url"] == "https://www.youtube.com/watch?v=abc123"
    command = captured["command"]
    assert command[0] == "/bin/zara-ytdlp"
    assert command[-1] == "ytsearch1:authorized sample"


def test_packaged_ytdlp_wrapper_enforces_delays():
    text = (Path(__file__).resolve().parents[1] / "scripts" / "zara-ytdlp").read_text()
    assert "--sleep-requests 1" in text
    assert "--sleep-interval 1" in text
    assert "--max-sleep-interval 3" in text


@pytest.mark.parametrize(
    "url",
    [
        "https://example.com/watch?v=abc",
        "file:///tmp/reference.wav",
        "javascript:alert(1)",
    ],
)
def test_clone_rejects_non_youtube_sources_before_download(url):
    value = expert()

    with pytest.raises(ValueError, match="YouTube"):
        value.clone_from_youtube(url, "narrator_voice")


def test_tool_surface_marks_voice_mutations_for_approval(monkeypatch):
    tools = build_voice_tools(FakeProlog(), FakeConfig())
    by_name = {tool.name: tool for tool in tools}

    assert {
        "youtube_search",
        "voice_list",
        "voice_plan",
        "voice_speak",
        "voice_narrate",
        "voice_analyze_youtube",
        "voice_clone_from_youtube",
        "voice_delete",
    }.issubset(by_name)
    assert by_name["voice_clone_from_youtube"].metadata["zara_requires_approval"] is True
    assert by_name["voice_delete"].metadata["zara_requires_approval"] is True


def test_packaged_prolog_voice_policy_defaults_and_distinct_dialogue():
    goal = (
        'kb_voice_expert:resolve_voice("narrator","",["zara","alice"],[],V1),'
        'V1="zara",'
        'kb_voice_expert:resolve_voice("dialogue","",["zara","alice"],["zara"],V2),'
        'V2="alice",'
        'asserta(kb_voice_expert:voice_speaker("speaker_00","alice"),Ref),'
        'kb_voice_expert:resolve_voice("speaker_00","narrator","",["zara","alice"],[],V3),'
        'V3="alice",erase(Ref),'
        'kb_voice_expert:replace_speaker_segments("source",[segment(0,"speaker_00",100,900)]),'
        'kb_voice_expert:speaker_segments("source",Segments),'
        'Segments=[segment(0,"speaker_00",100,900)]'
    )
    result = subprocess.run(
        ["swipl", "-q", "-s", "main.pl", "-g", goal, "-t", "halt"],
        capture_output=True,
        text=True,
        timeout=20,
        check=False,
    )
    assert result.returncode == 0, result.stderr
