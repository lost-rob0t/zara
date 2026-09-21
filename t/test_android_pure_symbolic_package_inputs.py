from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
ANDROID_APP = ROOT / "android" / "app"
BUILD = ANDROID_APP / "build.gradle.kts"
PORTABLE_CORE = (
    ANDROID_APP
    / "src"
    / "main"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "prolog"
    / "PortableSemanticCore.kt"
)
PORTABLE_STORE = (
    ANDROID_APP
    / "src"
    / "main"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "history"
    / "PortableConversationStore.kt"
)


CANONICAL_SEMANTIC_SOURCES = {
    "modules/intent_frames.pl": "prolog/shared/modules/intent_frames.pl",
    "modules/normalizer.pl": "prolog/shared/modules/normalizer.pl",
    "modules/symbolic_dialogue.pl": "prolog/shared/modules/symbolic_dialogue.pl",
    "modules/symbolic_dialogue_turn.pl": "prolog/shared/modules/symbolic_dialogue_turn.pl",
    "kb/intents.pl": "prolog/shared/kb/intents.pl",
}


def test_android_package_uses_only_canonical_symbolic_sources() -> None:
    build = BUILD.read_text(encoding="utf-8")
    core = PORTABLE_CORE.read_text(encoding="utf-8")

    assert "GeneratePortableSemanticAssets" in build
    assert "addGeneratedSourceDirectory" in build

    for source, asset in CANONICAL_SEMANTIC_SOURCES.items():
        declaration = f'layout.projectDirectory.file("../../{source}")'
        assert build.count(declaration) == 1, source
        assert f'"{asset}"' in core, asset

        committed_copy = ANDROID_APP / "src" / "main" / "assets" / asset
        assert not committed_copy.exists(), (
            f"{asset} must be generated from {source}; committed copies create "
            "a second symbolic runtime resource authority"
        )


def test_android_package_and_store_share_canonical_conversation_schema() -> None:
    build = BUILD.read_text(encoding="utf-8")
    store = PORTABLE_STORE.read_text(encoding="utf-8")

    schema_source = 'layout.projectDirectory.file("../../zara/conversation_schema.sql")'
    assert build.count(schema_source) == 1
    assert "GeneratePortableConversationSchema" in build
    assert 'into(output.resolve("database"))' in build
    assert 'rename { "conversation_schema.sql" }' in build
    assert 'const val schemaAsset = "database/conversation_schema.sql"' in store

    committed_schema = ANDROID_APP / "src" / "main" / "assets" / "database" / "conversation_schema.sql"
    assert not committed_schema.exists(), (
        "Android must package the canonical zara/conversation_schema.sql via the generated "
        "asset task instead of carrying a second writable schema copy"
    )


def test_pure_symbolic_runtime_resources_are_mandatory_build_inputs() -> None:
    build = BUILD.read_text(encoding="utf-8")

    required = (
        'checkNotNull(sources["symbolic_dialogue.pl"])',
        'checkNotNull(sources["symbolic_dialogue_turn.pl"])',
        '"symbolic_dialogue.pl input is required"',
        '"symbolic_dialogue_turn.pl input is required"',
    )
    for marker in required:
        assert marker in build

    assert "output.deleteRecursively()" in build
    assert "@get:InputFiles" in build
    assert "PathSensitivity.RELATIVE" in build
