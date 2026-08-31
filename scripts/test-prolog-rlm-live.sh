#!/usr/bin/env bash
set -euo pipefail

# Live Prolog-RLM direct-mode rewrite smoke test over OpenRouter.
#
# Requires a real credential and the pinned Prolog-RLM runtime from the Nix
# dev shell; this is never part of the deterministic gate.
#
# Run with:
#   export OPENROUTER_API_KEY='sk-or-...'
#   nix develop -c bash scripts/test-prolog-rlm-live.sh
#
# Optional environment:
#   ZARA_RLM_LIVE_MODEL   OpenRouter model id (default: openrouter/free)

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

if [ -z "${OPENROUTER_API_KEY:-}" ]; then
    echo "OPENROUTER_API_KEY is not set; export it before running this live smoke" >&2
    exit 1
fi
if [ -z "${ZARA_PROLOG_RLM_ROOT:-}" ]; then
    echo "ZARA_PROLOG_RLM_ROOT is not set; run through the Nix dev shell (nix develop)" >&2
    exit 1
fi

test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT
export XDG_CONFIG_HOME="$test_root/config"
export ZARA_LIVE_REPO_ROOT="$repo_root"
mkdir -p "$XDG_CONFIG_HOME/zarathushtra"

{
    echo "prolog_rlm_enabled(true)."
    if [ -n "${ZARA_RLM_LIVE_MODEL:-}" ]; then
        printf 'prolog_rlm_model("%s").\n' "$ZARA_RLM_LIVE_MODEL"
    fi
} > "$XDG_CONFIG_HOME/zarathushtra/config.pl"

cat > "$test_root/rlm_live.pl" <<'PROLOG'
load_runtime :-
    getenv('ZARA_LIVE_REPO_ROOT', RepoRoot),
    catch(
        ( use_module(RepoRoot/modules/command_loop),
          use_module(RepoRoot/kb/config) ),
        Error,
        ( format(user_error, "FAIL: Prolog load failed: ~w~n", [Error]), halt(1) )
    ).

live :-
    (   load_runtime
    ->  true
    ;   format(user_error, "FAIL: runtime load failed~n", []),
        halt(1)
    ),
    (   kb_config:prolog_rlm_model(Model) -> true ; Model = "openrouter/free" ),
    format("runtime=~w model=~w~n",
           ['pinned prolog-rlm (ZARA_PROLOG_RLM_ROOT)', Model]),
    get_time(Start),
    catch(
        command_loop:rewrite_with_llm("play some spotify music", Intent, Args),
        Error,
        ( format(user_error, "FAIL: rewrite raised ~w~n", [Error]), halt(1) )
    ),
    get_time(End),
    Duration is End - Start,
    format("intent=~w args=~w duration=~2f s~n", [Intent, Args, Duration]),
    (   Intent == ask
    ->  format(user_error,
               "WARNING: model returned a non-canonical rewrite (ask fallback)~n", [])
    ;   true
    ),
    (   ground(Intent), is_list(Args), Args \== []
    ->  format("PASS: Prolog-RLM direct-mode rewrite over OpenRouter~n")
    ;   format(user_error, "FAIL: unground rewrite result~n", []),
        halt(1)
    ),
    halt.

:- catch(live, Error,
         ( ( Error = unwind(_) -> true
           ; format(user_error, "FAIL: ~w~n", [Error]) ),
           halt(1))).
PROLOG

swipl -q -t halt "$test_root/rlm_live.pl"
