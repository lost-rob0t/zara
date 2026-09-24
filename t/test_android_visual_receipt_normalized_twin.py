import hashlib
import json
from pathlib import Path
import subprocess
import sys


EMULATOR_GATE = Path("scripts/test-android-emulator-install.sh")
SOURCE_SHA = "a" * 40
APK_SHA256 = "b" * 64


def visual_manifest_verifier() -> str:
    source = EMULATOR_GATE.read_text(encoding="utf-8")
    marker = (
        'python3 - "$visual_manifest" '
        '"$repo_root/android/app/build/reports/device" "$source_sha" <<\'PY\'\n'
    )
    start = source.index(marker) + len(marker)
    end = source.index("\nPY\n", start)
    return source[start:end]


def write_bundle(tmp_path: Path) -> Path:
    screenshot = tmp_path / "drawer-conversation-overflow.png"
    screenshot.write_bytes(b"\x89PNG\r\n\x1a\nnormalized-twin-regression")
    screenshot_sha = hashlib.sha256(screenshot.read_bytes()).hexdigest()

    text_twin = tmp_path / "drawer-conversation-overflow.ui.txt"
    text_twin.write_text(
        'route="chat"\n'
        'runtime={"mode":null,"model":null,"phase":null,"quantization":null,"runtime_id":null}\n'
        'ACTION 1 tap_contains:Actions for\n'
        'ACTION 2 capture:drawer-conversation-overflow\n'
        'class="android.widget.TextView" text="Pin" content_desc="" enabled=true clickable=false selected=false focused=false bounds=[10,10][50,30]\n'
        'class="android.widget.TextView" text="Rename" content_desc="" enabled=true clickable=false selected=false focused=false bounds=[10,30][70,50]\n'
        'class="android.widget.TextView" text="Move to project" content_desc="" enabled=true clickable=false selected=false focused=false bounds=[10,50][110,70]\n'
        'ASSERT PASS transient-surface-visible actions=Pin,Rename,Move to project viewport=120x100\n',
        encoding="utf-8",
    )
    text_sha = hashlib.sha256(text_twin.read_bytes()).hexdigest()

    actions = [
        {
            "label": "Pin",
            "bounds": "[10,10][50,30]",
            "content_inset_px": 2,
            "luma_span": 64,
            "occupied_luma_bins": 12,
        },
        {
            "label": "Rename",
            "bounds": "[10,30][70,50]",
            "content_inset_px": 2,
            "luma_span": 72,
            "occupied_luma_bins": 16,
        },
        {
            "label": "Move to project",
            "bounds": "[10,50][110,70]",
            "content_inset_px": 2,
            "luma_span": 80,
            "occupied_luma_bins": 20,
        },
    ]
    manifest = {
        "passed": True,
        "source_sha": SOURCE_SHA,
        "device": {"api": "35"},
        "screenshots": [
            {
                "state": "drawer-conversation-overflow",
                "file": screenshot.name,
                "sha256": screenshot_sha,
            }
        ],
        "visual_checks": [
            {
                "state": "drawer-conversation-overflow",
                "source_sha": SOURCE_SHA,
                "device_api": "35",
                "profile": "default",
                "trigger_bounds": [90, 10, 110, 30],
                "action_union": [10, 10, 110, 70],
                "viewport": [120, 100],
                "screenshot_file": screenshot.name,
                "screenshot_sha256": screenshot_sha,
                "text_twin_file": text_twin.name,
                "text_twin_sha256": text_sha,
                "actions": actions,
            }
        ],
    }
    manifest_path = tmp_path / "manifest.json"
    manifest_path.write_text(json.dumps(manifest), encoding="utf-8")
    return manifest_path


def test_visual_manifest_verifier_accepts_canonical_normalized_text_twin(tmp_path: Path) -> None:
    manifest_path = write_bundle(tmp_path)
    result = subprocess.run(
        [sys.executable, "-", str(manifest_path), str(tmp_path), SOURCE_SHA],
        input=visual_manifest_verifier(),
        text=True,
        capture_output=True,
        check=False,
    )

    assert result.returncode == 0, result.stderr
