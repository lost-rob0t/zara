#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
semantic_core="$repo_root/android/app/src/main/assets/prolog/portable/semantic_core.pl"
semantic_corpus="$repo_root/kb/semantic_corpus.pl"
report_dir="$repo_root/android/app/build/reports/semantic-parity"

: "${ZARA_TREALLA_SOURCE_DIR:?ZARA_TREALLA_SOURCE_DIR must point to the pinned Trealla source}"
command -v swipl >/dev/null
command -v make >/dev/null
command -v gcc >/dev/null
command -v python3 >/dev/null

test -f "$semantic_core"
test -f "$semantic_corpus"
test -f "$repo_root/modules/intent_frames.pl"
test -f "$repo_root/modules/normalizer.pl"
test -f "$repo_root/modules/symbolic_dialogue.pl"
test -f "$repo_root/modules/symbolic_dialogue_turn.pl"
test -f "$repo_root/kb/intents.pl"

trealla_embed_sample="$ZARA_TREALLA_SOURCE_DIR/samples/embed.c"
test -f "$trealla_embed_sample"
if ! grep -Fq 'if (!pl_query(pl, goal, &q, 0))' "$trealla_embed_sample"; then
  echo "semantic parity FAILED: pinned Trealla pl_query success contract changed" >&2
  exit 1
fi

rm -rf "$report_dir"
mkdir -p "$report_dir"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

trealla="$tmp/trealla"
cp -R "$ZARA_TREALLA_SOURCE_DIR" "$trealla"
chmod -R u+w "$trealla"
bash "$repo_root/android/patch-trealla-module-path.sh" "$trealla"
if ! make -C "$trealla" -f GNUmakefile -j2 \
  NOSSL=1 NOFFI=1 NOTHREADS=1 NOTTY=1 NONETWORK=1 tpl \
  >"$report_dir/trealla-build.stdout" 2>"$report_dir/trealla-build.stderr"; then
  echo "semantic parity FAILED: pinned Trealla host build failed" >&2
  cat "$report_dir/trealla-build.stderr" >&2
  exit 1
fi

stage="$tmp/prolog"
mkdir -p "$stage/portable" "$stage/shared/modules" "$stage/shared/kb"
cp "$semantic_core" "$stage/portable/semantic_core.pl"
cp "$repo_root/modules/intent_frames.pl" "$stage/shared/modules/intent_frames.pl"
cp "$repo_root/modules/normalizer.pl" "$stage/shared/modules/normalizer.pl"
cp "$repo_root/modules/symbolic_dialogue.pl" "$stage/shared/modules/symbolic_dialogue.pl"
cp "$repo_root/modules/symbolic_dialogue_turn.pl" "$stage/shared/modules/symbolic_dialogue_turn.pl"
cp "$repo_root/kb/intents.pl" "$stage/shared/kb/intents.pl"
cp "$semantic_corpus" "$stage/shared/kb/semantic_corpus.pl"

cat >"$stage/parity_driver.pl" <<'PL'
parity_main :-
    parity_dialogue_envelope,
    findall(Id, corpus_case(Id, _, _, _, _, _), Ids),
    sort(Ids, UniqueIds),
    length(Ids, Count),
    length(UniqueIds, Count),
    Count > 0,
    parity_cases(Ids),
    halt(0).
parity_main :-
    halt(2).

% Exercise the exact logical envelope Android sends through the JNI bridge.
% This keeps the parity gate honest: semantic_core.pl imports the canonical
% dialogue modules, Context0 is bridged from persisted string data to the atom
% input required by pinned Trealla read_term_from_atom/3, the router/renderer
% is committed once, and response + Context1 are then exposed as the two
% Result solutions consumed by the bounded native adapter.
parity_dialogue_envelope :-
    atom_string(Context0Atom, "[]"),
    read_term_from_atom(Context0Atom, Context0, []),
    findall(Result,
        ( ( ( symbolic_dialogue_turn:valid_dialogue_context(Context0),
              symbolic_dialogue_turn:dialogue_turn(
                  "timer",
                  conversation,
                  Context0,
                  turn(_Frames, Act, Context1)
              ),
              symbolic_dialogue_turn:valid_dialogue_context(Context1),
              symbolic_dialogue:render_response(Act, Response)
            ) -> true ; fail
          ),
          ( Result = Response ; Result = dialogue_context(Context1) )
        ),
        Results),
    Results = [Rendered, dialogue_context(Context)],
    string_codes(Rendered, RenderedCodes),
    string_codes("How long should I set the timer for?", ExpectedCodes),
    RenderedCodes == ExpectedCodes,
    Context = partial_frame(
        frame(intent(ns(device), name('timer.set')), [], missing([duration])),
        [duration]
    ),
    write_canonical(dialogue(timer_envelope, verified)),
    nl.

parity_cases([]).
parity_cases([Id|Rest]) :-
    corpus_case(Id, Utterance, State, Context, Expected, _Tags),
    zara_portable_semantic_core:resolve_frames(Utterance, State, Context, Frames),
    zara_portable_semantic_core:normalize_frames(Frames, Normalized),
    zara_portable_semantic_core:normalize_frames(Expected, ExpectedNormalized),
    ( Normalized == ExpectedNormalized ->
        % SWI and Trealla intentionally render list syntax differently even
        % under write_canonical/1. Emit an engine-neutral attestation only
        % after each engine has independently proven the full normalized term.
        write_canonical(case(Id, verified)),
        nl
    ;
        write_canonical(mismatch(Id, ExpectedNormalized, Normalized)),
        nl,
        halt(3)
    ),
    parity_cases(Rest).
PL

swi_out="$report_dir/swi.stdout"
swi_err="$report_dir/swi.stderr"
trealla_out="$report_dir/trealla.stdout"
trealla_err="$report_dir/trealla.stderr"

if ! swipl -q -f none \
    -s "$stage/portable/semantic_core.pl" \
    -s "$stage/shared/kb/semantic_corpus.pl" \
    -s "$stage/parity_driver.pl" \
    -g parity_main >"$swi_out" 2>"$swi_err"; then
  echo "semantic parity FAILED: SWI-Prolog corpus execution failed" >&2
  cat "$swi_err" >&2
  cat "$swi_out" >&2
  exit 1
fi

if ! "$trealla/tpl" -q -f \
    "$stage/portable/semantic_core.pl" \
    "$stage/shared/kb/semantic_corpus.pl" \
    -s "$stage/parity_driver.pl" \
    -g parity_main >"$trealla_out" 2>"$trealla_err"; then
  echo "semantic parity FAILED: Trealla corpus execution failed" >&2
  cat "$trealla_err" >&2
  cat "$trealla_out" >&2
  exit 1
fi

swi_case_count="$(wc -l <"$swi_out" | tr -d ' ')"
trealla_case_count="$(wc -l <"$trealla_out" | tr -d ' ')"
if [[ "$swi_case_count" -le 0 || "$trealla_case_count" -le 0 ]]; then
  echo "semantic parity FAILED: canonical corpus produced no cases" >&2
  exit 1
fi
if [[ "$swi_case_count" != "$trealla_case_count" ]]; then
  echo "semantic parity FAILED: engine case counts diverged (SWI=$swi_case_count Trealla=$trealla_case_count)" >&2
  exit 1
fi

if ! cmp -s "$swi_out" "$trealla_out"; then
  diff -u "$swi_out" "$trealla_out" >"$report_dir/cross-runtime.diff" || true
  echo "semantic parity FAILED: SWI-Prolog and Trealla verified different corpus cases" >&2
  cat "$report_dir/cross-runtime.diff" >&2
  exit 1
fi

echo "semantic parity gate ok: $swi_case_count canonical cases"
