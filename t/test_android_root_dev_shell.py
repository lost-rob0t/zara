from __future__ import annotations

import json
import os
import pathlib
import shutil
import subprocess


ROOT = pathlib.Path(__file__).resolve().parents[1]


def _assert_projection_structure() -> None:
    root_flake = (ROOT / "flake.nix").read_text()
    lock = json.loads((ROOT / "flake.lock").read_text())
    android_flake = (ROOT / "android" / "flake.nix").read_text()

    assert 'android.url = "path:./android";' in root_flake
    assert "outputs = { self, nixpkgs, android, ... }:" in root_flake
    assert "devShells.android = android.devShells.${system}.default;" in root_flake
    assert "androidPkgs = import nixpkgs" not in root_flake
    assert "androidEnv = androidPkgs.androidenv.composeAndroidPackages" not in root_flake

    android_lock = lock["nodes"]["android"]
    assert android_lock["locked"] == {"path": "./android", "type": "path"}
    assert android_lock["original"] == {"path": "./android", "type": "path"}

    for export in (
        "ANDROID_NDK_ROOT",
        "ZARA_ANDROID_NDK_VERSION",
        "ZARA_TREALLA_SOURCE_DIR",
        "ZARA_TREALLA_LIBRARY_ROOT",
    ):
        assert f"export {export}=" in android_flake


def test_root_android_dev_shell_is_the_pinned_android_toolchain(tmp_path: pathlib.Path) -> None:
    _assert_projection_structure()

    # `nix flake check` also runs pytest inside a pure derivation where the Nix
    # client is intentionally absent. The repository's primary test-all and both
    # adversarial CI jobs run outside that derivation with Nix installed, so they
    # execute the real root shell below. Keep the pure derivation deterministic
    # without skipping the test or trying to nest Nix inside a Nix sandbox.
    if shutil.which("nix") is None:
        return

    env = os.environ.copy()
    env["GRADLE_USER_HOME"] = str(tmp_path / "gradle-home")
    probe = r'''
set -euo pipefail
: "${ZARA_ANDROID_NDK_VERSION:?root Android shell must export ZARA_ANDROID_NDK_VERSION}"
: "${ANDROID_HOME:?root Android shell must export ANDROID_HOME}"
: "${ANDROID_NDK_ROOT:?root Android shell must export ANDROID_NDK_ROOT}"
: "${ZARA_TREALLA_SOURCE_DIR:?root Android shell must export ZARA_TREALLA_SOURCE_DIR}"
: "${ZARA_TREALLA_LIBRARY_ROOT:?root Android shell must export ZARA_TREALLA_LIBRARY_ROOT}"
test -d "$ANDROID_NDK_ROOT"
test -d "$ZARA_TREALLA_SOURCE_DIR"
test -f "$ZARA_TREALLA_LIBRARY_ROOT/arm64-v8a/libtrealla.a"
test -f "$ZARA_TREALLA_LIBRARY_ROOT/x86_64/libtrealla.a"
test "$ANDROID_NDK_ROOT" = "$ANDROID_HOME/ndk/$ZARA_ANDROID_NDK_VERSION"
cd android
gradle --no-daemon :app:assembleDebug
'''
    result = subprocess.run(
        ["nix", "develop", ".#android", "--command", "bash", "-lc", probe],
        cwd=ROOT,
        env=env,
        capture_output=True,
        text=True,
        timeout=900,
    )

    assert result.returncode == 0, (
        "root .#android shell drifted from the canonical pinned Android toolchain\n"
        f"stdout:\n{result.stdout}\n"
        f"stderr:\n{result.stderr}"
    )
