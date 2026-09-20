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


def test_android_response_act_vocabulary_matches_canonical_schema():
    schema = json.loads(SCHEMA_PATH.read_text())
    source = ANDROID_PATH.read_text()
    android_acts = set(
        re.findall(r'^\s+[A-Z_]+\("([a-z_]+)"\)[,;]?$', source, flags=re.MULTILINE)
    )

    assert android_acts == set(schema["properties"]["act"]["enum"])


def test_android_projection_pins_canonical_zero_model_wire_identity():
    schema = json.loads(SCHEMA_PATH.read_text())
    source = ANDROID_PATH.read_text()

    assert f'"{schema["properties"]["protocol"]["const"]}"' in source
    assert f'"{schema["properties"]["renderer"]["const"]}"' in source
    assert "providersEnabled: Boolean = false" in source
    assert "maxModelCalls: Int = 0" in source
    assert "providerCalls == 0" in source
    assert "modelCalls == 0" in source
