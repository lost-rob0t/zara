import json
import re
from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
SCHEMA_PATH = ROOT / "contracts" / "symbolic-dialogue-v1" / "response-act.schema.json"
ANDROID_PATH = (
    ROOT
    / "android"
    / "app"
    / "src"
    / "main"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "prolog"
    / "SymbolicResponseAct.kt"
)


def _kotlin_enum_wire_names(source: str, enum_name: str) -> set[str]:
    match = re.search(
        rf"enum class {re.escape(enum_name)}\(val wireName: String\) \{{(?P<body>.*?)\n\}}",
        source,
        flags=re.DOTALL,
    )
    assert match is not None, f"missing Kotlin wire enum: {enum_name}"
    return set(
        re.findall(
            r'^\s+[A-Z_]+\("([a-z_]+)"\)[,;]?$',
            match.group("body"),
            flags=re.MULTILINE,
        )
    )


def _schema_payload_enum(schema: dict, act: str, field: str) -> set[str]:
    branch = next(
        candidate
        for candidate in schema["oneOf"]
        if candidate["properties"]["act"].get("const") == act
    )
    return set(branch["properties"]["payload"]["properties"][field]["enum"])


def test_android_response_act_vocabulary_matches_canonical_schema():
    schema = json.loads(SCHEMA_PATH.read_text())
    source = ANDROID_PATH.read_text()
    android_acts = _kotlin_enum_wire_names(source, "SymbolicResponseActKind")

    assert android_acts == set(schema["properties"]["act"]["enum"])


def test_android_acknowledgement_vocabulary_matches_canonical_schema():
    schema = json.loads(SCHEMA_PATH.read_text())
    source = ANDROID_PATH.read_text()
    android_kinds = _kotlin_enum_wire_names(source, "SymbolicAcknowledgementKind")

    assert android_kinds == _schema_payload_enum(schema, "acknowledgement", "kind")


def test_android_projection_pins_canonical_zero_model_wire_identity():
    schema = json.loads(SCHEMA_PATH.read_text())
    source = ANDROID_PATH.read_text()

    assert f'"{schema["properties"]["protocol"]["const"]}"' in source
    assert f'"{schema["properties"]["renderer"]["const"]}"' in source
    assert "providersEnabled: Boolean = false" in source
    assert "maxModelCalls: Int = 0" in source
    assert "providerCalls == 0" in source
    assert "modelCalls == 0" in source
