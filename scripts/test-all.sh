#!/usr/bin/env bash
# ZARA-021: Full deterministic regression gate.
#
# Runs every test phase in fail-fast order with isolated HOME/XDG.
# One command validates the entire repository:
#
#   nix develop -c bash scripts/test-all.sh
#
# Phases (in order):
#   1. Python compile/import checks
#   2. Prolog module load and resolver corpus
#   3. Focused pytest suite (with JUnit XML output)
#   4. Config/process/file-tool security scripts
#   5. Packaging/Nix checks
#
# Output:
#   - JUnit XML at $ARTIFACT_DIR/junit.xml (pytest)
#   - Per-phase pass/fail summary on stdout
#   - Artifacts under $ARTIFACT_DIR
#
# No live microphone, desktop, provider credential, model download, or
# external network is required.
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

# --- Isolated environment -------------------------------------------------
# Every sub-test gets a clean HOME, XDG_CONFIG_HOME, XDG_RUNTIME_DIR, and
# ZARA_DICTATION_* paths so no user data leaks in and no test pollutes the
# user's real config.
TEST_ROOT="$(mktemp -d)"
trap 'rm -rf "$TEST_ROOT"; rm -rf "$repo_root/t/fixtures/audio"' EXIT

export HOME="$TEST_ROOT/home"
export XDG_CONFIG_HOME="$TEST_ROOT/config"
export XDG_RUNTIME_DIR="$TEST_ROOT/run"
export XDG_DATA_HOME="$TEST_ROOT/share"
export XARA_DICTATION_PIDFILE="$TEST_ROOT/run/zara_dictation.pid"
export ZARA_DICTATION_PIDFILE="$TEST_ROOT/run/zara_dictation.pid"
export ZARA_DICTATION_LOGFILE="$TEST_ROOT/run/zara_dictation.log"
export LANG=C.UTF-8
export LC_ALL=C.UTF-8
export PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"

mkdir -p "$HOME" "$XDG_CONFIG_HOME" "$XDG_RUNTIME_DIR" "$XDG_DATA_HOME"

# --- Artifact directory --------------------------------------------------
ARTIFACT_DIR="${ARTIFACT_DIR:-$TEST_ROOT/artifacts}"
mkdir -p "$ARTIFACT_DIR"

PHASE_COUNT=0
PASS_COUNT=0
FAIL_COUNT=0

run_phase() {
  local name="$1"
  shift
  PHASE_COUNT=$((PHASE_COUNT + 1))
  echo ""
  echo "=== Phase $PHASE_COUNT: $name ==="
  if "$@"; then
    echo "  PASS: $name"
    PASS_COUNT=$((PASS_COUNT + 1))
  else
    echo "  FAIL: $name"
    FAIL_COUNT=$((FAIL_COUNT + 1))
    echo ""
    echo "*** Regression gate FAILED at phase $PHASE_COUNT ($name) ***"
    exit 1
  fi
}

# --- Phase 1: Python compile/import checks --------------------------------
phase_python_compile() {
  python -m compileall -q zara scripts
  python -c "import zara.wake; import zara.dictate; import zara.console; import zara.agent_cli; import zara.__main__"
  python -m zara --help >/dev/null 2>&1 || true
  python -m zara --help 2>&1 | grep -q "Zarathustra Voice Assistant"
}

run_phase "Python compile/import checks" phase_python_compile

# --- Phase 1b: Generate deterministic audio fixtures ----------------------
phase_generate_fixtures() {
  python "$repo_root/scripts/generate-audio-fixtures.py" "$repo_root/t/fixtures/audio" >/dev/null
}

run_phase "Generate audio fixtures" phase_generate_fixtures

# --- Phase 2: Prolog module load and resolver corpus ----------------------
phase_prolog_load() {
  swipl -q -g "consult('main.pl'), halt" -t "halt(1)" 2>&1
}

run_phase "Prolog module load" phase_prolog_load

phase_resolver_corpus() {
  bash "$repo_root/scripts/test-intents.sh"
}

run_phase "Prolog resolver corpus" phase_resolver_corpus

# --- Phase 3: Focused pytest suite ---------------------------------------
phase_pytest() {
  python -m pytest -q --junit-xml="$ARTIFACT_DIR/junit.xml" t/
}

run_phase "Pytest suite" phase_pytest

# --- Phase 4: Config/process/file-tool security scripts -------------------
phase_security_scripts() {
  local security_scripts=(
    scripts/test-config.sh
    scripts/test-process-safety.sh
    scripts/test-file-tools.sh
    scripts/test-command-routing.sh
    scripts/test-replies.sh
    scripts/test-prolog-engine.sh
    scripts/test-prolog-config.sh
    scripts/test-alert-sounds.sh
    scripts/test-dictation-lifecycle.sh
    scripts/test-dictation.sh
    scripts/test-timers.sh
    scripts/test-todos.sh
    scripts/test-llm-clients.sh
    scripts/test-memory.sh
    scripts/test-tts.sh
    scripts/test-wake-lifecycle.sh
    scripts/test-agent-history.sh
  )
  for script in "${security_scripts[@]}"; do
    echo "  running $script..."
    bash "$repo_root/$script"
  done
}

run_phase "Config/process/file-tool security scripts" phase_security_scripts

# --- Phase 5: Packaging/Nix checks ----------------------------------------
phase_packaging() {
  bash "$repo_root/scripts/test-packaging.sh"
  bash "$repo_root/scripts/test-canonical-paths.sh"
  bash "$repo_root/scripts/test-entrypoints.sh"
}

run_phase "Packaging/Nix checks" phase_packaging

# --- Summary --------------------------------------------------------------
echo ""
echo "=== Regression Gate Summary ==="
echo "Phases run:    $PHASE_COUNT"
echo "Phases passed: $PASS_COUNT"
echo "Phases failed: $FAIL_COUNT"
echo "JUnit XML:     $ARTIFACT_DIR/junit.xml"
echo "Test root:     $TEST_ROOT (cleaned up on exit)"
echo ""
if [ "$FAIL_COUNT" -eq 0 ]; then
  echo "ALL PHASES PASSED"
else
  echo "SOME PHASES FAILED"
  exit 1
fi
