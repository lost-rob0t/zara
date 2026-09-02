#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
semantic_core="$repo_root/android/app/src/main/assets/prolog/portable/semantic_core.pl"
semantic_corpus="$repo_root/kb/semantic_corpus.pl"

: "${ZARA_TREALLA_SOURCE_DIR:?ZARA_TREALLA_SOURCE_DIR must point to the pinned Trealla source}"
command -v swipl >/dev/null
command -v make >/dev/null
command -v gcc >/dev/null
command -v python3 >/dev/null

test -f "$semantic_core"
test -f "$semantic_corpus"
test -f "$repo_root/modules/intent_frames.pl"
test -f "$repo_root/modules/normalizer.pl"
test -f "$repo_root/kb/intents.pl"

tmp="$(mktemp -d)"
trap 'rm -rf "$tmp"' EXIT

trealla="$tmp/trealla"
cp -R "$ZARA_TREALLA_SOURCE_DIR" "$trealla"
chmod -R u+w "$trealla"
bash "$repo_root/android/patch-trealla-module-path.sh" "$trealla"
make -C "$trealla" -f GNUmakefile -j2 \
  NOSSL=1 NOFFI=1 NOTHREADS=1 NOTTY=1 NONETWORK=1 tpl >/dev/null

stage="$tmp/prolog"
mkdir -p "$stage/portable" "$stage/shared/modules" "$stage/shared/kb"
cp "$semantic_core" "$stage/portable/semantic_core.pl"
cp "$repo_root/modules/intent_frames.pl" "$stage/shared/modules/intent_frames.pl"
cp "$repo_root/modules/normalizer.pl" "$stage/shared/modules/normalizer.pl"
cp "$repo_root/kb/intents.pl" "$stage/shared/kb/intents.pl"
cp "$semantic_corpus" "$stage/shared/kb/semantic_corpus.pl"

cat >"$stage/parity_driver.pl" <<'PL'
parity_main :-
    findall(Id, corpus_case(Id, _, _, _, _, _), Ids),
    sort(Ids, UniqueIds),
    length(Ids, Count),
    length(UniqueIds, Count),
    Count > 0,
    parity_cases(Ids),
    halt(0).
parity_main :-
    halt(2).

parity_cases([]).
parity_cases([Id|Rest]) :-
    corpus_case(Id, Utterance, State, Context, Expected, _Tags),
    zara_portable_semantic_core:resolve_frames(Utterance, State, Context, Frames),
    zara_portable_semantic_core:normalize_frames(Frames, Normalized),
    zara_portable_semantic_core:normalize_frames(Expected, ExpectedNormalized),
    ( Normalized == ExpectedNormalized ->
        write_canonical(case(Id, Normalized)),
        nl
    ;
        write_canonical(mismatch(Id, ExpectedNormalized, Normalized)),
        nl,
        halt(3)
    ),
    parity_cases(Rest).
PL

swi_out="$tmp/swi.out"
trealla_out="$tmp/trealla.out"

if ! swipl -q -f none \
    -s "$stage/portable/semantic_core.pl" \
    -s "$stage/shared/kb/semantic_corpus.pl" \
    -s "$stage/parity_driver.pl" \
    -g parity_main >"$swi_out"; then
  echo "semantic parity FAILED: SWI-Prolog corpus execution failed" >&2
  cat "$swi_out" >&2
  exit 1
fi

if ! "$trealla/tpl" -q -f \
    "$stage/portable/semantic_core.pl" \
    "$stage/shared/kb/semantic_corpus.pl" \
    "$stage/parity_driver.pl" \
    -g parity_main >"$trealla_out"; then
  echo "semantic parity FAILED: Trealla corpus execution failed" >&2
  cat "$trealla_out" >&2
  exit 1
fi

case_count="$(wc -l <"$swi_out" | tr -d ' ')"
if [[ "$case_count" -le 0 ]]; then
  echo "semantic parity FAILED: canonical corpus produced no cases" >&2
  exit 1
fi

if ! cmp -s "$swi_out" "$trealla_out"; then
  echo "semantic parity FAILED: SWI-Prolog and Trealla diverged" >&2
  diff -u "$swi_out" "$trealla_out" >&2 || true
  exit 1
fi

echo "semantic parity gate ok: $case_count canonical cases"
