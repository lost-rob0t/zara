#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
fixtures="$repo_root/android/app/src/main/assets/prolog/portable/semantic_fixtures.json"
semantic_core="$repo_root/android/app/src/main/assets/prolog/portable/semantic_core.pl"

: "${ZARA_TREALLA_SOURCE_DIR:?ZARA_TREALLA_SOURCE_DIR must point to the pinned Trealla source}"
command -v swipl >/dev/null
command -v python3 >/dev/null
command -v make >/dev/null
command -v gcc >/dev/null

test -f "$fixtures"
test -f "$semantic_core"
test -f "$repo_root/modules/intent_frames.pl"
test -f "$repo_root/modules/normalizer.pl"
test -f "$repo_root/kb/intents.pl"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

trealla="$tmp/trealla"
cp -R "$ZARA_TREALLA_SOURCE_DIR" "$trealla"
chmod -R u+w "$trealla"
make -C "$trealla" -f GNUmakefile -j2 NOSSL=1 NOFFI=1 NOTHREADS=1 tpl >/dev/null

stage="$tmp/prolog"
mkdir -p "$stage/portable" "$stage/shared/modules" "$stage/shared/kb"
cp "$semantic_core" "$stage/portable/semantic_core.pl"
cp "$repo_root/modules/intent_frames.pl" "$stage/shared/modules/intent_frames.pl"
cp "$repo_root/modules/normalizer.pl" "$stage/shared/modules/normalizer.pl"
cp "$repo_root/kb/intents.pl" "$stage/shared/kb/intents.pl"

SWIPL="$(command -v swipl)" \
TPL="$trealla/tpl" \
SEMANTIC_CORE="$stage/portable/semantic_core.pl" \
FIXTURES="$fixtures" \
python3 <<'PY'
import json
import os
import subprocess
import sys
from pathlib import Path

swipl = os.environ["SWIPL"]
tpl = os.environ["TPL"]
semantic_core = os.environ["SEMANTIC_CORE"]
fixture_path = Path(os.environ["FIXTURES"])

with fixture_path.open("r", encoding="utf-8") as handle:
    document = json.load(handle)

if document.get("contract") != "ZARA-SEMANTIC/1":
    raise SystemExit("semantic parity: unsupported fixture contract")

cases = document.get("cases")
if not isinstance(cases, list) or not cases or len(cases) > 256:
    raise SystemExit("semantic parity: invalid fixture case list")

seen = set()
allowed_states = {"passive", "conversation", "dictation"}


def run(runtime, command, fixture_id):
    completed = subprocess.run(
        command,
        text=True,
        capture_output=True,
        timeout=20,
        check=False,
    )
    if completed.returncode != 0:
        stderr = completed.stderr.strip().splitlines()
        detail = stderr[-1] if stderr else f"exit {completed.returncode}"
        raise RuntimeError(f"{fixture_id}: {runtime} failed: {detail}")
    lines = [line.strip() for line in completed.stdout.splitlines() if line.strip()]
    if len(lines) != 1:
        raise RuntimeError(
            f"{fixture_id}: {runtime} produced {len(lines)} result lines"
        )
    return lines[0]


for case in cases:
    fixture_id = case.get("id")
    utterance = case.get("utterance")
    state = case.get("state")
    expected_terms = case.get("expected_terms")

    if not isinstance(fixture_id, str) or not fixture_id or len(fixture_id) > 96:
        raise SystemExit("semantic parity: invalid fixture id")
    if fixture_id in seen:
        raise SystemExit(f"semantic parity: duplicate fixture id: {fixture_id}")
    seen.add(fixture_id)

    if not isinstance(utterance, str) or len(utterance) > 4096:
        raise SystemExit(f"semantic parity: invalid utterance: {fixture_id}")
    if state not in allowed_states:
        raise SystemExit(f"semantic parity: invalid state: {fixture_id}")
    if not isinstance(expected_terms, list) or not all(
        isinstance(term, str) and term for term in expected_terms
    ):
        raise SystemExit(f"semantic parity: invalid expected terms: {fixture_id}")

    text_term = json.dumps(utterance, ensure_ascii=False)
    goal = (
        "zara_portable_semantic_core:resolve_frames("
        f"{text_term},{state},[],Frames),"
        "zara_portable_semantic_core:normalize_frames(Frames,Normalized),"
        "write_canonical(Normalized),nl,halt"
    )

    swi = run(
        "SWI-Prolog",
        [swipl, "-q", "-f", "none", "-s", semantic_core, "-g", goal],
        fixture_id,
    )
    trealla = run(
        "Trealla",
        [tpl, "-q", "-f", semantic_core, "-g", goal],
        fixture_id,
    )
    expected = "[" + ",".join(expected_terms) + "]"

    if swi != trealla:
        raise SystemExit(
            f"semantic parity mismatch [{fixture_id}]\nSWI:     {swi}\nTrealla: {trealla}"
        )
    if swi != expected:
        raise SystemExit(
            f"semantic corpus mismatch [{fixture_id}]\nExpected: {expected}\nActual:   {swi}"
        )

    print(f"semantic parity ok: {fixture_id}")

print(f"semantic parity gate ok: {len(cases)} fixtures")
PY
