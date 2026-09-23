from __future__ import annotations

import os
import pathlib
import shutil
import subprocess


ROOT = pathlib.Path(__file__).resolve().parents[1]


def test_root_android_dev_shell_is_the_pinned_android_toolchain(tmp_path: pathlib.Path) -> None:
    assert shutil.which("nix") is not None, "release toolchain regression requires nix"

    env = os.environ.copy()
    env["GRADLE_USER_HOME"] = str(tmp_path / "gradle-home")
    probe = r'''
set -euo pipefail
: "${ZARA_ANDROID_NDK_VERSION:?root Android shell must export ZARA_ANDROID_NDK_VERSION}"
: "${ANDROID_HOME:?root Android shell must export ANDROID_HOME}"
: "${ANDROID_NDK_ROOT:?root Android shell must export ANDROID_NDK_ROOT}"
: "${ZARA_TREALLA_SOURCE_DIR:?root Android shell must export ZARA_TREALLA_SOURCE_DIR}"
test -d "$ANDROID_NDK_ROOT"
test -d "$ZARA_TREALLA_SOURCE_DIR"
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
