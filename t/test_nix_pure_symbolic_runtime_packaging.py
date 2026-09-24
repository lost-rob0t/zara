from __future__ import annotations

import hashlib
import os
import pathlib
import sqlite3
import subprocess
import sys
import tarfile
import zipfile

import pytest


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCHEMA_PATH = ROOT / "zara" / "conversation_schema.sql"

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
        'zara-dictate = mkZaraPackage {',
        'zara-prolog = pkgs.stdenv.mkDerivation {',
    )
    missing = [fragment for fragment in required if fragment not in flake]
    assert not missing, f"flake lost canonical symbolic install paths: {missing}"

    # Nix must copy the source-owned zara tree wholesale. A schema-specific copy
    # or generated Android asset here would create a second packaging authority.
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
        timeout=120,
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


def _build_wheel_from_sdist(
    sdist_archive: pathlib.Path,
    tmp_path: pathlib.Path,
) -> pathlib.Path:
    source_dir = tmp_path / "source"
    source_dir.mkdir()
    with tarfile.open(sdist_archive, "r:gz") as archive:
        archive.extractall(source_dir)

    roots = [entry for entry in source_dir.iterdir() if entry.is_dir()]
    assert len(roots) == 1, f"expected one sdist source root, got {roots}"

    dist_dir = tmp_path / "dist"
    dist_dir.mkdir()
    result = subprocess.run(
        [sys.executable, "setup.py", "bdist_wheel", "--dist-dir", str(dist_dir)],
        cwd=roots[0],
        capture_output=True,
        text=True,
        timeout=120,
    )
    assert result.returncode == 0, (
        "wheel-from-sdist build failed\nstdout:\n{}\nstderr:\n{}".format(
            result.stdout,
            result.stderr,
        )
    )
    wheels = list(dist_dir.glob("*.whl"))
    assert len(wheels) == 1, f"expected one wheel archive, got {wheels}"
    return wheels[0]


@pytest.fixture(scope="module")
def sdist_archive(tmp_path_factory: pytest.TempPathFactory) -> pathlib.Path:
    return _build_sdist(tmp_path_factory.mktemp("symbolic-sdist"))


@pytest.fixture(scope="module")
def sdist_files(sdist_archive: pathlib.Path) -> dict[str, bytes]:
    with tarfile.open(sdist_archive, "r:gz") as archive:
        files: dict[str, bytes] = {}
        for member in archive.getmembers():
            if not member.isfile():
                continue
            extracted = archive.extractfile(member)
            assert extracted is not None, f"failed to read sdist member: {member.name}"
            files[member.name] = extracted.read()
        return files


@pytest.fixture(scope="module")
def wheel_archive(
    sdist_archive: pathlib.Path,
    tmp_path_factory: pytest.TempPathFactory,
) -> pathlib.Path:
    return _build_wheel_from_sdist(
        sdist_archive,
        tmp_path_factory.mktemp("symbolic-wheel-from-sdist"),
    )


@pytest.fixture(scope="module")
def wheel_files(wheel_archive: pathlib.Path) -> dict[str, bytes]:
    with zipfile.ZipFile(wheel_archive) as archive:
        return {name: archive.read(name) for name in archive.namelist() if not name.endswith("/")}


def _contains(names: set[str], rel: str) -> bool:
    return rel in names or any(name.endswith(f"/{rel}") for name in names)


def _schema_payload(files: dict[str, bytes]) -> bytes:
    matches = [
        payload
        for name, payload in files.items()
        if name == "zara/conversation_schema.sql"
        or name.endswith("/zara/conversation_schema.sql")
    ]
    assert len(matches) == 1, "expected exactly one canonical packaged conversation schema"
    return matches[0]


def _schema_text(files: dict[str, bytes]) -> str:
    return _schema_payload(files).decode("utf-8")


def _assert_schema_bootstraps(schema: str) -> None:
    connection = sqlite3.connect(":memory:")
    try:
        connection.executescript(schema)
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


def _assert_legacy_owner_replay(schema: str) -> None:
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


def _assert_file_backed_restart(schema: str, db_path: pathlib.Path) -> None:
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
        conversation = connection.execute(
            """
            SELECT provider, model
            FROM desktop_conversations
            WHERE id = 'restart-conversation'
            """
        ).fetchone()

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
    assert conversation == ("", "")


def test_sdist_ships_canonical_symbolic_runtime(sdist_files: dict[str, bytes]) -> None:
    names = set(sdist_files)
    missing = [rel for rel in CANONICAL_SYMBOLIC_RESOURCES if not _contains(names, rel)]
    assert not missing, f"sdist dropped canonical symbolic runtime resources: {missing}"


def test_sdist_has_one_conversation_schema_authority(sdist_files: dict[str, bytes]) -> None:
    schemas = sorted(name for name in sdist_files if name.endswith("/conversation_schema.sql"))
    assert len(schemas) == 1, f"sdist has competing conversation schemas: {schemas}"
    assert schemas[0].endswith("/zara/conversation_schema.sql")


def test_sdist_schema_is_byte_identical_to_source_authority(
    sdist_files: dict[str, bytes],
) -> None:
    assert _schema_payload(sdist_files) == SCHEMA_PATH.read_bytes()


def test_wheel_built_from_sdist_keeps_one_byte_identical_schema(
    wheel_files: dict[str, bytes],
) -> None:
    schemas = sorted(name for name in wheel_files if name.endswith("conversation_schema.sql"))
    assert schemas == ["zara/conversation_schema.sql"]
    assert _schema_payload(wheel_files) == SCHEMA_PATH.read_bytes()


def test_sdist_and_wheel_share_exact_schema_authority(
    sdist_files: dict[str, bytes],
    wheel_files: dict[str, bytes],
) -> None:
    source = SCHEMA_PATH.read_bytes()
    assert _schema_payload(sdist_files) == _schema_payload(wheel_files) == source


def test_packaged_schema_bootstraps_symbolic_projection_indexes(
    sdist_files: dict[str, bytes],
    wheel_files: dict[str, bytes],
) -> None:
    _assert_schema_bootstraps(_schema_text(sdist_files))
    _assert_schema_bootstraps(_schema_text(wheel_files))


def test_packaged_schema_keeps_legacy_owner_migration(
    sdist_files: dict[str, bytes],
    wheel_files: dict[str, bytes],
) -> None:
    _assert_legacy_owner_replay(_schema_text(sdist_files))
    _assert_legacy_owner_replay(_schema_text(wheel_files))


def test_packaged_schema_survives_file_backed_restart_with_zero_model_usage(
    sdist_files: dict[str, bytes],
    wheel_files: dict[str, bytes],
    tmp_path: pathlib.Path,
) -> None:
    _assert_file_backed_restart(
        _schema_text(sdist_files),
        tmp_path / "sdist-conversation.db",
    )
    _assert_file_backed_restart(
        _schema_text(wheel_files),
        tmp_path / "wheel-conversation.db",
    )


def test_wheel_runtime_loader_executes_packaged_schema_with_hard_zero_restart(
    wheel_archive: pathlib.Path,
    tmp_path: pathlib.Path,
) -> None:
    wheel_root = tmp_path / "wheel"
    wheel_root.mkdir()
    with zipfile.ZipFile(wheel_archive) as archive:
        archive.extractall(wheel_root)

    expected_sha256 = hashlib.sha256(SCHEMA_PATH.read_bytes()).hexdigest()
    env = os.environ.copy()
    env.update(
        {
            "PYTHONNOUSERSITE": "1",
            "PYTHONPATH": str(wheel_root),
            "ZARA_SCHEMA_SHA256": expected_sha256,
            "ZARA_WHEEL_ROOT": str(wheel_root),
            "ZARA_SCHEMA_DB": str(tmp_path / "runtime-loader.db"),
        }
    )

    script = r'''
import hashlib
import os
import pathlib
import sqlite3

import zara
from zara.conversation_schema import conversation_schema_sql

wheel_root = pathlib.Path(os.environ["ZARA_WHEEL_ROOT"]).resolve()
package_path = pathlib.Path(zara.__file__).resolve()
assert package_path.is_relative_to(wheel_root), (
    f"loaded Zara outside candidate wheel: {package_path}"
)

schema = conversation_schema_sql()
assert hashlib.sha256(schema.encode("utf-8")).hexdigest() == os.environ["ZARA_SCHEMA_SHA256"]
db_path = pathlib.Path(os.environ["ZARA_SCHEMA_DB"])

with sqlite3.connect(db_path) as connection:
    connection.executescript(schema)
    connection.execute(
        """
        INSERT INTO desktop_conversations (id, title, created_at, updated_at)
        VALUES ('legacy-runtime', 'Legacy', '2026-09-22T00:00:00Z', '2026-09-22T00:00:00Z')
        """
    )
    connection.execute(
        """
        INSERT INTO desktop_symbolic_projections (
            conversation_id, principal_id, dialogue_state_json, updated_at
        ) VALUES (
            'legacy-runtime', 'uid:321', '{"state":"runtime-kept"}', '2026-09-22T00:00:00Z'
        )
        """
    )
    connection.executescript(schema)
    assert connection.execute(
        """
        SELECT principal_id, dialogue_state_json
        FROM desktop_symbolic_projections
        WHERE conversation_id = 'legacy-runtime'
        """
    ).fetchone() == ('local:owner', '{"state":"runtime-kept"}')

    connection.execute(
        """
        INSERT INTO desktop_conversations (
            id, title, created_at, updated_at, provider, model, principal_id
        ) VALUES (
            'runtime-restart', 'Runtime',
            '2026-09-22T00:00:01Z', '2026-09-22T00:00:02Z',
            '', '', 'local:owner'
        )
        """
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
            'runtime-restart', 'local:owner', 'turn-runtime', 'success',
            4, 8, 'runtime-project', 2, 'answer',
            '{"follow_up":"why"}', '[]', '[]',
            '[{"expert":"DotfilesExpert"}]', '[{"fact":"runtime-project"}]',
            'turn-runtime:evidence-1', 'symbolic-renderer/v1',
            0, 0, 0, 0, '2026-09-22T00:00:02Z'
        )
        """
    )

with sqlite3.connect(db_path) as connection:
    connection.executescript(conversation_schema_sql())
    projection = connection.execute(
        """
        SELECT project_id, dialogue_state_json, expert_evidence_json,
               providers_enabled, max_model_calls, provider_calls, model_calls
        FROM desktop_symbolic_projections
        WHERE conversation_id = 'runtime-restart'
          AND principal_id = 'local:owner'
        """
    ).fetchone()
    conversation = connection.execute(
        "SELECT provider, model FROM desktop_conversations WHERE id = 'runtime-restart'"
    ).fetchone()

assert projection == (
    'runtime-project',
    '{"follow_up":"why"}',
    '[{"expert":"DotfilesExpert"}]',
    0,
    0,
    0,
    0,
)
assert conversation == ('', '')
'''

    result = subprocess.run(
        [sys.executable, "-P", "-c", script],
        cwd=tmp_path,
        env=env,
        capture_output=True,
        text=True,
        timeout=30,
    )
    assert result.returncode == 0, (
        "candidate wheel schema runtime failed\nstdout:\n{}\nstderr:\n{}".format(
            result.stdout,
            result.stderr,
        )
    )


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
