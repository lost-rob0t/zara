from pathlib import Path

import pytest

from zara.voice_analysis import SpeakerSegment, VoiceAnalysisError, VoiceAnalyzer


class FakeConfig:
    def __init__(self, sections=None):
        self.sections = sections or {}

    def get_section(self, name):
        return dict(self.sections.get(name, {}))


class FakeTerm:
    def __init__(self, name, *args):
        self.name = name
        self.args = list(args)


class FakeProlog:
    def __init__(self, *, mutation_result=None, readback_segments=None):
        self.goals = []
        self.mutation_result = {} if mutation_result is None else mutation_result
        self.readback_segments = [] if readback_segments is None else readback_segments

    def query_once(self, goal):
        self.goals.append(goal)
        if "replace_speaker_segments" in goal:
            return self.mutation_result
        if "speaker_segments" in goal:
            return {"Segments": self.readback_segments}
        return {}


def analyzer(config=None, prolog=None):
    return VoiceAnalyzer(
        config=config or FakeConfig(),
        prolog_engine=prolog or FakeProlog(),
    )


def test_vad_and_diarization_intersection_keeps_only_speech():
    value = analyzer()

    segments = value._intersect(
        speech_intervals=[(0.0, 5.0), (7.0, 12.0)],
        diarized=[
            ("speaker_00", 0.2, 2.0),
            ("speaker_01", 2.0, 8.0),
            ("speaker_01", 8.0, 10.0),
            ("speaker_02", 5.2, 6.8),
        ],
    )

    assert [(row.speaker, row.start, row.end) for row in segments] == [
        ("speaker_00", 0.2, 2.0),
        ("speaker_01", 2.0, 5.0),
        ("speaker_01", 7.0, 10.0),
    ]


def test_speaker_fact_projection_is_bounded_typed_prolog():
    prolog = FakeProlog(
        readback_segments=[
            FakeTerm("segment", 0, "speaker_00", 125, 1500),
            FakeTerm("segment", 1, "speaker_01", 2000, 4250),
        ]
    )
    value = analyzer(prolog=prolog)

    value._store_segments(
        "https://www.youtube.com/watch?v=abc123",
        [
            SpeakerSegment(0, "speaker_00", 0.125, 1.5),
            SpeakerSegment(1, "speaker_01", 2.0, 4.25),
        ],
    )

    assert len(prolog.goals) == 2
    mutation_goal, readback_goal = prolog.goals
    assert "kb_voice_expert:replace_speaker_segments(" in mutation_goal
    assert 'segment(0,"speaker_00",125,1500)' in mutation_goal
    assert 'segment(1,"speaker_01",2000,4250)' in mutation_goal
    assert readback_goal == (
        'kb_voice_expert:speaker_segments('
        '"https://www.youtube.com/watch?v=abc123",Segments)'
    )


def test_speaker_fact_projection_rejects_failed_mutation():
    prolog = FakeProlog(mutation_result=False)
    value = analyzer(prolog=prolog)

    with pytest.raises(VoiceAnalysisError, match="failed to persist speaker segments"):
        value._store_segments(
            "source-1",
            [SpeakerSegment(0, "speaker_00", 0.0, 1.0)],
        )

    assert len(prolog.goals) == 1


def test_speaker_fact_projection_rejects_readback_mismatch():
    prolog = FakeProlog(
        readback_segments=[FakeTerm("segment", 0, "speaker_99", 0, 1000)]
    )
    value = analyzer(prolog=prolog)

    with pytest.raises(VoiceAnalysisError, match="postcondition mismatch"):
        value._store_segments(
            "source-1",
            [SpeakerSegment(0, "speaker_00", 0.0, 1.0)],
        )

    assert len(prolog.goals) == 2


def test_missing_diarization_models_fail_actionably(tmp_path):
    value = analyzer(
        config=FakeConfig(
            {
                "voice_expert": {
                    "diarization_segmentation_model": str(tmp_path / "missing.onnx"),
                    "diarization_embedding_model": str(tmp_path / "missing-embed.onnx"),
                }
            }
        )
    )

    with pytest.raises(VoiceAnalysisError, match="does not exist"):
        value._diarize(
            __import__("numpy").zeros(16_000, dtype="float32"),
            num_speakers=None,
        )


def test_empty_diarization_model_path_fails_before_import():
    value = analyzer()

    with pytest.raises(VoiceAnalysisError, match="is not configured"):
        value._diarize(
            __import__("numpy").zeros(16_000, dtype="float32"),
            num_speakers=None,
        )


def test_speaker_segment_serializes_duration():
    segment = SpeakerSegment(3, "speaker_02", 1.25, 4.0)

    assert segment.to_dict() == {
        "index": 3,
        "speaker": "speaker_02",
        "start": 1.25,
        "end": 4.0,
        "duration": 2.75,
    }


@pytest.mark.parametrize("seconds", [5, 1800, 14400])
def test_analysis_source_bound_accepts_valid_values(seconds):
    value = analyzer(
        config=FakeConfig({"voice_expert": {"max_source_seconds": seconds}})
    )

    assert value._max_source_seconds() == float(seconds)
