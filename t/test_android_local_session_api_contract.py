"""Catch local-model UI/session integration breaks before the APK compilation gate."""
from __future__ import annotations

import hashlib
from pathlib import Path
import re
import unittest

ROOT = Path(__file__).resolve().parents[1]
ANDROID = ROOT / "android/app/src/main/java/ai/zara/app"
LOCAL_API = frozenset(
    {
        "localChatPathEnabled",
        "setLocalChatPathEnabled",
        "localAiState",
        "localAiModels",
        "installLocalModel",
        "selectLocalModel",
        "unloadLocalModel",
    }
)
DECLARATION = re.compile(r"\bfun\s+(\w+)\s*\(")
CALL = re.compile(r"\b(?:appSession|session)\s*(?:\.|::)\s*(\w+)\b")


def missing_local_members(session: str, callers: list[str]) -> set[str]:
    declared = set(DECLARATION.findall(session))
    referenced = {name for source in callers for name in CALL.findall(source)}
    return (referenced & LOCAL_API) - declared


def git_blob_sha(source: bytes) -> str:
    header = f"blob {len(source)}\0".encode("ascii")
    return hashlib.sha1(header + source).hexdigest()


class LocalSessionApiContractTest(unittest.TestCase):
    def test_missing_getter_and_setter_are_detected(self) -> None:
        self.assertEqual(
            {"localChatPathEnabled", "setLocalChatPathEnabled"},
            missing_local_members(
                "class AndroidAppSession { fun localAiState() = Unit }",
                [
                    "session.localChatPathEnabled()",
                    "onChanged = appSession::setLocalChatPathEnabled",
                ],
            ),
        )

    def test_existing_session_api_is_accepted(self) -> None:
        self.assertEqual(
            set(),
            missing_local_members(
                "fun localAiState() = Unit\nfun installLocalModel(source: InputStream) {}",
                ["session.localAiState(); appSession.installLocalModel(source)"],
            ),
        )

    def test_checker_does_not_invent_a_second_settings_api(self) -> None:
        self.assertEqual(
            set(),
            missing_local_members("fun setRuntimeMode(mode: RuntimeMode) {}", ["onSelect(mode)"]),
        )

    def test_shipping_local_ui_calls_have_session_members(self) -> None:
        paths = (
            ANDROID / "AndroidAppSession.kt",
            ANDROID / "MainActivity.kt",
            ANDROID / "ui/ZaraApp.kt",
            ANDROID / "ui/PairingZaraApp.kt",
        )
        contents = [path.read_bytes() for path in paths]
        sources = [content.decode("utf-8") for content in contents]
        missing = missing_local_members(sources[0], sources[1:])
        fingerprints = {
            str(path.relative_to(ROOT)): git_blob_sha(content)
            for path, content in zip(paths, contents)
        }
        self.assertFalse(
            missing,
            f"Unresolved local UI/session APIs: {sorted(missing)}; source_blobs={fingerprints}",
        )


if __name__ == "__main__":
    unittest.main()
