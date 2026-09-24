from __future__ import annotations

import hashlib
import os
import pathlib
import shutil
import subprocess
import sys

import pytest


ROOT = pathlib.Path(__file__).resolve().parents[1]
SCHEMA_PATH = ROOT / "zara" / "conversation_schema.sql"

RUNTIME_IMPORT_ROOTS = {
    "zara-cli": pathlib.Path("lib/python"),
    "zara-server": pathlib.Path("lib/python"),
    "zara-desktop": pathlib.Path("lib/python"),
    "zara-wake": pathlib.Path("lib/python"),
    "zara-dictate": pathlib.Path("lib/python"),
    "zara-prolog": pathlib.Path("share/zarathushtra"),
}

RUNTIME_PROBE = r'''
import hashlib
import os
import pathlib
import sqlite3

import zara
from zara.conversation_schema import conversation_schema_sql

import_root = pathlib.Path(os.environ["ZARA_NIX_IMPORT_ROOT"]).resolve()
package_path = pathlib.Path(zara.__file__).resolve()
assert package_path.is_relative_to(import_root), (
    f"loaded Zara outside candidate Nix runtime: {package_path}"
)

schema = conversation_schema_sql()
assert hashlib.sha256(schema.encode("utf-8")).hexdigest() == os.environ["ZARA_SCHEMA_SHA256"]
db_path = pathlib.Path(os.environ["ZARA_SCHEMA_DB"])

with sqlite3.connect(db_path) as connection:
    connection.executescript(schema)
    connection.execute(
        """
        INSERT INTO desktop_conversations (id, title, created_at, updated_at)
        VALUES ('legacy-runtime', 'Legacy', '2026-09-23T00:00:00Z', '2026-09-23T00:00:00Z')
        """
    )
    connection.execute(
        """
        INSERT INTO desktop_symbolic_projections (
            conversation_id, principal_id, dialogue_state_json, updated_at
        ) VALUES (
            'legacy-runtime', 'uid:321', '{"state":"runtime-kept"}', '2026-09-23T00:00:00Z'
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
            '2026-09-23T00:00:01Z', '2026-09-23T00:00:02Z',
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
            0, 0, 0, 0, '2026-09-23T00:00:02Z'
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


def _build_nix_runtime(attribute: str) -> pathlib.Path:
    result = subprocess.run(
        [
            "nix",
            "build",
            "--no-link",
            "--print-out-paths",
            f".#${attribute}".replace("$", ""),
        ],
        cwd=ROOT,
        capture_output=True,
        text=True,
        timeout=600,
    )
    assert result.returncode == 0, (
        f"Nix runtime build failed for {attribute}\n"
        f"stdout:\n{result.stdout}\n"
        f"stderr:\n{result.stderr}"
    )
    outputs = [pathlib.Path(line.strip()) for line in result.stdout.splitlines() if line.strip()]
    assert len(outputs) == 1, f"expected one Nix output for {attribute}, got {outputs}"
    return outputs[0]


@pytest.mark.skipif(
    bool(os.environ.get("PYTHONHASHSEED")),
    reason="Nix runtime packaging executes once in the primary suite, not adversarial replays",
)
def test_every_nix_runtime_executes_the_source_owned_conversation_schema(tmp_path: pathlib.Path) -> None:
    if shutil.which("nix") is None:
        pytest.skip("nix CLI is unavailable in this test environment")

    source_schema = SCHEMA_PATH.read_bytes()
    source_sha256 = hashlib.sha256(source_schema).hexdigest()

    for attribute, relative_import_root in RUNTIME_IMPORT_ROOTS.items():
        output = _build_nix_runtime(attribute)
        packaged_schemas = list(output.rglob("conversation_schema.sql"))
        assert packaged_schemas == [
            output / relative_import_root / "zara" / "conversation_schema.sql"
        ], f"{attribute} must ship exactly one canonical conversation schema"
        assert packaged_schemas[0].read_bytes() == source_schema
        assert hashlib.sha256(packaged_schemas[0].read_bytes()).hexdigest() == source_sha256

        import_root = output / relative_import_root
        env = os.environ.copy()
        env.update(
            {
                "PYTHONPATH": str(import_root),
                "PYTHONNOUSERSITE": "1",
                "ZARA_NIX_IMPORT_ROOT": str(import_root),
                "ZARA_SCHEMA_SHA256": source_sha256,
                "ZARA_SCHEMA_DB": str(tmp_path / f"{attribute}.db"),
            }
        )
        result = subprocess.run(
            [sys.executable, "-P", "-c", RUNTIME_PROBE],
            cwd=tmp_path,
            env=env,
            capture_output=True,
            text=True,
            timeout=60,
        )
        assert result.returncode == 0, (
            f"{attribute} failed canonical schema runtime probe\n"
            f"stdout:\n{result.stdout}\n"
            f"stderr:\n{result.stderr}"
        )
