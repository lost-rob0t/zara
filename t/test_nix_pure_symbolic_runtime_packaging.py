from __future__ import annotations

import pathlib
import sqlite3
import subprocess
import sys
import tarfile

import pytest


ROOT = pathlib.Path(__file__).resolve().parents[1]

CANONICAL_SYMBOLIC_RESOURCES = (
    "modules/intent_frames.pl",
    "modules/normalizer.pl",
    "modules/symbolic_dialogue.pl",
    "modules/symbolic_dialogue_turn.pl",
    "kb/intents.pl",
    "zara/conversation_schema.sql",
)

CANONICAL_CONVERSATION_INDEXES = {
    "idx_desktop_conversations_updated",
    "idx_desktop_messages_conversation",
    "idx_desktop_messages_turn",
    "idx_desktop_messages_tool_run",
    "idx_desktop_conversations_principal_updated",
    "idx_desktop_messages_principal_conversation",
    "idx_desktop_symbolic_project",
    "idx_desktop_symbolic_turn",
}


def test_canonical_symbolic_resources_exist() -> None:
    missing = [rel for rel in CANONICAL_SYMBOLIC_RESOURCES if not (ROOT / rel).is_file()]
    assert not missing, f"missing canonical symbolic runtime resources: {missing}"


def test_nix_install_uses_source_owned_symbolic_trees() -> None:
    flake = (ROOT / "flake.nix").read_text(encoding="utf-8")

    required = (
        "cp -r $src/zara $out/lib/python/",
        "cp -r $src/kb $out/share/zarathushtra/",
        "cp -r $src/modules $out/share/zarathushtra/",
        "cp -r $src/zara $out/share/zarathushtra/",
        'zara-cli = mkZaraPackage {',
        'zara-server = mkZaraPackage {',
        'zara-desktop = mkZaraPackage {',
        'zara-wake = mkZaraPackage {',
    )
    missing = [fragment for fragment in required if fragment not in flake]
    assert not missing, f"flake lost canonical symbolic install paths: {missing}"

    assert "android/app/src/main/assets/prolog/shared" not in flake
    assert "android/app/build/generated" not in flake
    assert "conversation_schema.sql" not in flake


def _build_sdist(tmp_path: pathlib.Path) -> pathlib.Path:
    dist_dir = tmp_path / "dist"
    dist_dir.mkdir()
    result = subprocess.run(
        [sys.executable, "setup.py", "sdist", "--dist-dir", str(dist_dir)],
        cwd=ROOT,
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, (
        "sdist build failed\nstdout:\n{}\nstderr:\n{}".format(
            result.stdout,
            result.stderr,
        )
    )
    archives = list(dist_dir.glob("*.tar.gz"))
    assert len(archives) == 1, f"expected one sdist archive, got {archives}"
    return archives[0]


@pytest.fixture(scope="module")
def sdist_files(tmp_path_factory: pytest.TempPathFactory) -> dict[str, bytes]:
    archive = _build_sdist(tmp_path_factory.mktemp("symbolic-sdist"))
    with tarfile.open(archive, "r:gz") as tf:
        files: dict[str, bytes] = {}
        for member in tf.getmembers():
            if not member.isfile():
                continue
            extracted = tf.extractfile(member)
            assert extracted is not None, f"failed to read sdist member: {member.name}"
            files[member.name] = extracted.read()
        return files


@pytest.fixture(scope="module")
def sdist_names(sdist_files: dict[str, bytes]) -> set[str]:
    return set(sdist_files)


def _sdist_contains(names: set[str], rel: str) -> bool:
    suffix = f"/{rel}"
    return any(name.endswith(suffix) for name in names)


def _packaged_schema_payload(sdist_files: dict[str, bytes]) -> bytes:
    matches = [
        payload
        for name, payload in sdist_files.items()
        if name.endswith("/zara/conversation_schema.sql")
    ]
    assert len(matches) == 1, "expected exactly one canonical packaged conversation schema"
    return matches[0]


def _packaged_schema(sdist_files: dict[str, bytes]) -> str:
    return _packaged_schema_payload(sdist_files).decode("utf-8")


def test_sdist_ships_canonical_symbolic_runtime(sdist_names: set[str]) -> None:
    missing = [rel for rel in CANONICAL_SYMBOLIC_RESOURCES if not _sdist_contains(sdist_names, rel)]
    assert not missing, f"sdist dropped canonical symbolic runtime resources: {missing}"


def test_sdist_has_one_conversation_schema_authority(sdist_names: set[str]) -> None:
    schemas = sorted(name for name in sdist_names if name.endswith("/conversation_schema.sql"))
    assert len(schemas) == 1, f"sdist has competing conversation schemas: {schemas}"
    assert schemas[0].endswith("/zara/conversation_schema.sql")


def test_packaged_schema_is_byte_identical_to_source_authority(
    sdist_files: dict[str, bytes],
) -> None:
    source = (ROOT / "zara" / "conversation_schema.sql").read_bytes()
    assert _packaged_schema_payload(sdist_files) == source


def test_packaged_schema_bootstraps_symbolic_projection_indexes(
    sdist_files: dict[str, bytes],
) -> None:
    connection = sqlite3.connect(":memory:")
    try:
        connection.executescript(_packaged_schema(sdist_files))
        tables = {
            row[0]
            for row in connection.execute(
                "SELECT name FROM sqlite_master WHERE type = 'table'"
            )
        }
        indexes = {
            row[0]
            for row in connection.execute(
                "SELECT name FROM sqlite_master WHERE type = 'index'"
            )
        }
    finally:
        connection.close()

    assert "desktop_conversations" in tables
    assert "desktop_messages" in tables
    assert "desktop_symbolic_projections" in tables
    assert CANONICAL_CONVERSATION_INDEXES <= indexes


def test_packaged_schema_keeps_legacy_owner_migration(
    sdist_files: dict[str, bytes],
) -> None:
    schema = _packaged_schema(sdist_files)
    connection = sqlite3.connect(":memory:")
    try:
        connection.executescript(schema)
        connection.execute(
            """
            INSERT INTO desktop_conversations (id, title, created_at, updated_at)
            VALUES ('legacy-turn', 'Legacy', '2026-09-22T00:00:00Z', '2026-09-22T00:00:00Z')
            """
        )
        connection.execute(
            """
            INSERT INTO desktop_symbolic_projections (
                conversation_id, principal_id, dialogue_state_json, updated_at
            ) VALUES (
                'legacy-turn', 'uid:123', '{"state":"kept"}', '2026-09-22T00:00:00Z'
            )
            """
        )
        connection.executescript(schema)
        row = connection.execute(
            """
            SELECT principal_id, dialogue_state_json
            FROM desktop_symbolic_projections
            WHERE conversation_id = 'legacy-turn'
            """
        ).fetchone()
    finally:
        connection.close()

    assert row == ("local:owner", '{"state":"kept"}')


def test_packaged_schema_survives_file_backed_restart_with_zero_model_usage(
    sdist_files: dict[str, bytes],
    tmp_path: pathlib.Path,
) -> None:
    schema = _packaged_schema(sdist_files)
    db_path = tmp_path / "portable-conversation.db"

    with sqlite3.connect(db_path) as connection:
        connection.execute("PRAGMA foreign_keys = ON")
        connection.executescript(schema)
        connection.execute(
            """
            INSERT INTO desktop_conversations (
                id, title, created_at, updated_at, provider, model, principal_id
            ) VALUES (
                'restart-conversation', 'Restart',
                '2026-09-22T00:00:00Z', '2026-09-22T00:00:02Z',
                '', '', 'local:owner'
            )
            """
        )
        connection.executemany(
            """
            INSERT INTO desktop_messages (
                id, conversation_id, sequence, turn_id, role, content, status,
                error, tool_run_id, created_at, updated_at, principal_id
            ) VALUES (?, 'restart-conversation', ?, ?, ?, ?, 'complete', '', NULL, ?, ?, 'local:owner')
            """,
            (
                (
                    "message-1",
                    1,
                    "turn-1",
                    "user",
                    "remember the project",
                    "2026-09-22T00:00:01Z",
                    "2026-09-22T00:00:01Z",
                ),
                (
                    "message-2",
                    2,
                    "turn-2",
                    "assistant",
                    "I remember the project.",
                    "2026-09-22T00:00:02Z",
                    "2026-09-22T00:00:02Z",
                ),
            ),
        )
        connection.execute(
            """
            INSERT INTO desktop_symbolic_projections (
                conversation_id, principal_id, turn_id, outcome,
                projection_generation, runtime_generation, project_id,
                project_generation, dialogue_act, dialogue_state_json,
                discourse_entities_json, unresolved_questions_json,
                expert_evidence_json, verified_facts_json, verified_outcome_refs,
                renderer_provenance, providers_enabled, max_model_calls,
                provider_calls, model_calls, updated_at
            ) VALUES (
                'restart-conversation', 'local:owner', 'turn-2', 'success',
                9, 4, 'project-alpha', 3, 'answer',
                '{"topic":"project-alpha","follow_up":"why"}',
                '["project-alpha"]', '[]', '[{"expert":"DotfilesExpert"}]',
                '[{"fact":"project-alpha"}]', 'turn-2:evidence-1',
                'symbolic-renderer/v1', 0, 0, 0, 0,
                '2026-09-22T00:00:02Z'
            )
            """
        )

    with sqlite3.connect(db_path) as connection:
        connection.execute("PRAGMA foreign_keys = ON")
        connection.executescript(schema)
        projection = connection.execute(
            """
            SELECT turn_id, outcome, projection_generation, runtime_generation,
                   project_id, project_generation, dialogue_act,
                   dialogue_state_json, expert_evidence_json,
                   providers_enabled, max_model_calls, provider_calls, model_calls
            FROM desktop_symbolic_projections
            WHERE conversation_id = 'restart-conversation'
              AND principal_id = 'local:owner'
            """
        ).fetchone()
        messages = connection.execute(
            """
            SELECT sequence, turn_id, role, content
            FROM desktop_messages
            WHERE conversation_id = 'restart-conversation'
            ORDER BY sequence
            """
        ).fetchall()

    assert projection == (
        "turn-2",
        "success",
        9,
        4,
        "project-alpha",
        3,
        "answer",
        '{"topic":"project-alpha","follow_up":"why"}',
        '[{"expert":"DotfilesExpert"}]',
        0,
        0,
        0,
        0,
    )
    assert messages == [
        (1, "turn-1", "user", "remember the project"),
        (2, "turn-2", "assistant", "I remember the project."),
    ]


def test_python_package_metadata_keeps_schema_source_owned() -> None:
    manifest = (ROOT / "MANIFEST.in").read_text(encoding="utf-8")
    setup = (ROOT / "setup.py").read_text(encoding="utf-8")

    assert "recursive-include zara *.py *.pl *.toml *.sql" in manifest
    assert '"zara": ["py.typed", "conversation_schema.sql"]' in setup
    assert not (
        ROOT
        / "android"
        / "app"
        / "src"
        / "main"
        / "assets"
        / "database"
        / "conversation_schema.sql"
    ).exists()
