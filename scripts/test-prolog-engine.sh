#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export XDG_CONFIG_HOME="$test_root/config"
export PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"
export ZARA_PROLOG_TEST_ROOT="$test_root"

cat >"$test_root/fixture.pl" <<'PL'
:- module(intent_resolver, [resolve/4]).

resolve(Raw, State, echoed, [Raw, State]).
no_solution :- fail.
explode :- throw(error(fixture_failure, context(test, runtime))).
value(1).
value(2).
PL

cat >"$test_root/commands.pl" <<'PL'
:- module(commands, [execute/2]).

execute(echo, [Value]) :- nb_setval(captured_argument, Value).
PL

cat >"$test_root/capability_resolver.pl" <<'PL'
:- module(capability_resolver, [candidate/4, select/3]).

candidate(open, [vim], mapped_app, 100).
candidate(open, [vim], direct_app, 50).
candidate(open, [vim], executable_fallback, 10).
select(open, [vim], mapped_app).
PL

cat >"$test_root/syntax-error.pl" <<'PL'
:- module(broken, [value/1]).
value(.
PL

python - <<'PY'
import os
from pathlib import Path

from zara.prolog_engine import (
    CapabilityCandidate,
    PrologEngine,
    PrologQueryError,
    PrologStartupError,
)

test_root = Path(os.environ["ZARA_PROLOG_TEST_ROOT"])

try:
    PrologEngine(test_root / "missing.pl")
except PrologStartupError:
    pass
else:
    raise AssertionError("missing program did not fail startup")

try:
    PrologEngine(test_root / "syntax-error.pl")
except PrologStartupError:
    pass
else:
    raise AssertionError("syntax error did not fail startup")

engine = PrologEngine(test_root / "fixture.pl")
engine.consult(test_root / "commands.pl")
engine.consult(test_root / "capability_resolver.pl")
assert engine.query_once("intent_resolver:no_solution") is None

try:
    engine.query_once("intent_resolver:explode")
except PrologQueryError:
    pass
else:
    raise AssertionError("runtime exception was mistaken for no solution")

payload = 'quoted " text \\ path\nand ), throw(injected), ('
result = engine.resolve_intent(payload, state="conversation")
assert result is not None
assert result.name == "echoed"
assert result.args == [payload, "conversation"]

atom_payload = "quoted' atom \\ path\nand ), throw(injected), ("
assert engine.execute_intent("echo", [atom_payload])
captured = engine.query_once("nb_getval(captured_argument, Value)")
assert captured == {"Value": atom_payload}

values = engine.query_iter("intent_resolver:value(Value)")
assert next(values) == {"Value": 1}
values.close()
assert engine.query_once("true") == {}

assert engine.capability_candidates("open", ["vim"]) == [
    CapabilityCandidate("mapped_app", 100),
    CapabilityCandidate("direct_app", 50),
    CapabilityCandidate("executable_fallback", 10),
]
assert engine.selected_capability("open", ["vim"]) == "mapped_app"
assert engine.capability_candidates("open", ["missing"]) == []
assert engine.selected_capability("open", ["missing"]) is None
PY
