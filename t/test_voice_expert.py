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


def test_qwen_voice_inventory_never_fabricates_configured_voice(monkeypatch):
    value = VoiceExpert(FakeProlog(), FakeConfig())

    async def provider_inventory():
        return ["alice", "", "bob"]

    monkeypatch.setattr(value, "_qwen_list_voices", provider_inventory)

    listed = json.loads(value.list_voices())
    assert listed == {"provider": "qwen3", "voices": ["alice", "bob"]}
    assert "zara" not in listed["voices"]


def test_qwen_empty_provider_inventory_blocks_voice_plan(monkeypatch):
    value = VoiceExpert(FakeProlog(), FakeConfig())

    async def provider_inventory():
        return []

    monkeypatch.setattr(value, "_qwen_list_voices", provider_inventory)

    assert json.loads(value.list_voices()) == {"provider": "qwen3", "voices": []}
    with pytest.raises(RuntimeError, match="no available voices"):
        value.plan([VoiceSegment(text="hello")])


def prepare_clone_fixture(
    monkeypatch,
    value,
    *,
    inventory,
    register_updates_inventory=False,
):
    monkeypatch.setattr(value, "_require_binary", lambda name: "/bin/ffmpeg")
    provider_inventory = list(inventory)

    def fake_download(url, root):
        path = root / "source.webm"
        path.write_bytes(b"source" * 400)
        return path

    def fake_run(command, *, timeout):
        output = Path(command[-1])
        if output.name == "reference.wav":
            output.write_bytes(b"wav" * 700)
        return subprocess.CompletedProcess(command, 0, stdout="", stderr="")

    async def fake_register(voice_name, wav_path, *, reference_text):
        if register_updates_inventory and voice_name not in provider_inventory:
            provider_inventory.append(voice_name)
        return {"ok": True, "voice": voice_name}

    async def fake_list():
        return list(provider_inventory)

    monkeypatch.setattr(value, "_download_youtube_audio", fake_download)
    monkeypatch.setattr(value, "_run", fake_run)
    monkeypatch.setattr(value, "_qwen_register_voice", fake_register)
    monkeypatch.setattr(value, "_qwen_list_voices", fake_list)


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
        value.clone_from_youtube(
            url,
            "narrator_voice",
            rights_basis="consent",
            attest_not_public_figure=True,
        )


def test_clone_requires_typed_rights_and_public_figure_attestation(monkeypatch):
    value = expert()
    attempted_downloads = []
    monkeypatch.setattr(
        value,
        "_download_youtube_audio",
        lambda url, root: attempted_downloads.append(url),
    )

    with pytest.raises(ValueError, match="rights_basis"):
        value.clone_from_youtube(
            "https://youtu.be/abc123",
            "authorized_voice",
            rights_basis="",
            attest_not_public_figure=True,
        )
    with pytest.raises(ValueError, match="public figure"):
        value.clone_from_youtube(
            "https://youtu.be/abc123",
            "authorized_voice",
            rights_basis="consent",
            attest_not_public_figure=False,
        )

    assert attempted_downloads == []


def test_clone_tool_schema_requires_authority_fields():
    tools = build_voice_tools(FakeProlog(), FakeConfig())
    schema = {
        tool.name: tool for tool in tools
    }["voice_clone_from_youtube"].args_schema.model_json_schema()

    assert {"rights_basis", "attest_not_public_figure"}.issubset(schema["required"])


def test_clone_fails_when_fresh_inventory_does_not_confirm_registration(monkeypatch):
    value = expert()
    prepare_clone_fixture(monkeypatch, value, inventory=["zara"])

    with pytest.raises(RuntimeError, match="fresh provider inventory"):
        value.clone_from_youtube(
            "https://youtu.be/abc123",
            "authorized_voice",
            rights_basis="consent",
            attest_not_public_figure=True,
        )


def test_clone_rejects_preexisting_target_before_registration(monkeypatch):
    value = expert()
    attempted_downloads = []
    attempted_registrations = []

    monkeypatch.setattr(value, "_require_binary", lambda name: "/bin/ffmpeg")
    monkeypatch.setattr(
        value,
        "_download_youtube_audio",
        lambda url, root: attempted_downloads.append(url),
    )

    async def fake_list():
        return ["zara", "authorized_voice"]

    async def fake_register(voice_name, wav_path, *, reference_text):
        attempted_registrations.append(voice_name)
        return {"ok": True, "voice": voice_name}

    monkeypatch.setattr(value, "_qwen_list_voices", fake_list)
    monkeypatch.setattr(value, "_qwen_register_voice", fake_register)

    with pytest.raises(RuntimeError, match="already exists"):
        value.clone_from_youtube(
            "https://youtu.be/abc123",
            "authorized_voice",
            rights_basis="consent",
            attest_not_public_figure=True,
        )

    assert attempted_downloads == []
    assert attempted_registrations == []


def test_delete_rejects_absent_target_before_mutation(monkeypatch):
    value = expert()
    attempted_deletions = []

    async def fake_list():
        return ["zara"]

    async def fake_delete(voice_name):
        attempted_deletions.append(voice_name)
        return {"ok": True, "voice": voice_name}

    monkeypatch.setattr(value, "_qwen_list_voices", fake_list)
    monkeypatch.setattr(value, "_qwen_delete_voice", fake_delete)

    with pytest.raises(RuntimeError, match="does not exist"):
        value.delete_voice("authorized_voice")

    assert attempted_deletions == []


def test_delete_fails_when_fresh_inventory_still_contains_voice(monkeypatch):
    value = expert()

    async def fake_delete(voice_name):
        return {"ok": True, "voice": voice_name}

    async def fake_list():
        return ["zara", "authorized_voice"]

    monkeypatch.setattr(value, "_qwen_delete_voice", fake_delete)
    monkeypatch.setattr(value, "_qwen_list_voices", fake_list)

    with pytest.raises(RuntimeError, match="fresh provider inventory"):
        value.delete_voice("authorized_voice")


def test_verified_voice_mutations_report_postcondition_evidence(monkeypatch):
    value = expert()
    prepare_clone_fixture(
        monkeypatch,
        value,
        inventory=["zara"],
        register_updates_inventory=True,
    )

    registered = json.loads(
        value.clone_from_youtube(
            "https://youtu.be/abc123",
            "authorized_voice",
            rights_basis="licensed",
            attest_not_public_figure=True,
        )
    )
    assert registered["registered"] is True
    assert registered["postcondition"] == {
        "source": "fresh_provider_inventory",
        "voice_present": True,
    }

    delete_inventory = ["zara", "authorized_voice"]

    async def fake_delete(voice_name):
        delete_inventory.remove(voice_name)
        return {"ok": True, "voice": voice_name}

    async def fake_list_after_delete():
        return list(delete_inventory)

    monkeypatch.setattr(value, "_qwen_delete_voice", fake_delete)
    monkeypatch.setattr(value, "_qwen_list_voices", fake_list_after_delete)

    deleted = json.loads(value.delete_voice("authorized_voice"))
    assert deleted["deleted"] is True
    assert deleted["postcondition"] == {
        "source": "fresh_provider_inventory",
        "voice_present": False,
    }


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
