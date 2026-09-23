import hashlib
import json
from pathlib import Path
import subprocess
import sys


EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")
SOURCE_SHA = "a" * 40


def visual_manifest_verifier() -> str:
    source = EMULATOR_GATE.read_text(encoding="utf-8")
    marker = (
        'python3 - "$visual_manifest" '
        '"$repo_root/android/app/build/reports/device" "$source_sha" <<\'PY\'\n'
    )
    start = source.index(marker) + len(marker)
    end = source.index("\nPY\n", start)
    return source[start:end]


def write_visual_bundle(tmp_path: Path, *, twin_xml: str) -> Path:
    screenshot = tmp_path / "drawer-conversation-overflow.png"
    screenshot.write_bytes(b"synthetic screenshot bytes")
    text_twin = tmp_path / "drawer-conversation-overflow.xml"
    text_twin.write_text(twin_xml, encoding="utf-8")

    actions = [
        {"label": "Pin", "bounds": "[10,10][50,30]"},
        {"label": "Rename", "bounds": "[10,30][70,50]"},
        {"label": "Move to project", "bounds": "[10,50][110,70]"},
    ]
    manifest = {
        "passed": True,
        "source_sha": SOURCE_SHA,
        "device": {"api": "35"},
        "screenshots": [
            {
                "state": "drawer-conversation-overflow",
                "file": screenshot.name,
                "sha256": hashlib.sha256(screenshot.read_bytes()).hexdigest(),
            }
        ],
        "visual_checks": [
            {
                "state": "drawer-conversation-overflow",
                "source_sha": SOURCE_SHA,
                "device_api": "35",
                "profile": "default",
                "screenshot_file": screenshot.name,
                "screenshot_sha256": hashlib.sha256(screenshot.read_bytes()).hexdigest(),
                "text_twin_file": text_twin.name,
                "text_twin_sha256": hashlib.sha256(text_twin.read_bytes()).hexdigest(),
                "actions": actions,
            }
        ],
    }
    manifest_path = tmp_path / "manifest.json"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    return manifest_path


def run_visual_verifier(tmp_path: Path, *, twin_xml: str) -> subprocess.CompletedProcess[str]:
    manifest_path = write_visual_bundle(tmp_path, twin_xml=twin_xml)
    return subprocess.run(
        [sys.executable, "-", str(manifest_path), str(tmp_path), SOURCE_SHA],
        input=visual_manifest_verifier(),
        text=True,
        capture_output=True,
        check=False,
    )


def matching_twin() -> str:
    return """<hierarchy>
<node text="Pin" bounds="[10,10][50,30]" />
<node text="Rename" bounds="[10,30][70,50]" />
<node text="Move to project" bounds="[10,50][110,70]" />
</hierarchy>"""


def test_visual_manifest_verifier_accepts_matching_text_twin(tmp_path: Path) -> None:
    result = run_visual_verifier(tmp_path, twin_xml=matching_twin())
    assert result.returncode == 0, result.stderr


def test_visual_manifest_verifier_rejects_text_twin_missing_action(tmp_path: Path) -> None:
    stale_twin = """<hierarchy>
<node text="Pin" bounds="[10,10][50,30]" />
<node text="Rename" bounds="[10,30][70,50]" />
</hierarchy>"""

    result = run_visual_verifier(tmp_path, twin_xml=stale_twin)

    assert result.returncode != 0
    assert "text twin is missing action" in result.stderr


def test_visual_manifest_verifier_rejects_text_twin_bounds_drift(tmp_path: Path) -> None:
    stale_twin = """<hierarchy>
<node text="Pin" bounds="[10,10][50,30]" />
<node text="Rename" bounds="[80,80][140,100]" />
<node text="Move to project" bounds="[10,50][110,70]" />
</hierarchy>"""

    result = run_visual_verifier(tmp_path, twin_xml=stale_twin)

    assert result.returncode != 0
    assert "text twin action bounds differ" in result.stderr
