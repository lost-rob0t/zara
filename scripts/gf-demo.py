#!/usr/bin/env python3
"""Opt-in host pilot. Real PGF parsing; canonical Prolog context; no effects.

This is a bounded batch demonstration, not a second persisted conversation
store. Each semantic transition uses symbolic_dialogue_turn/4 inside Prolog.
"""
from __future__ import annotations

import argparse
import json
import os
from pathlib import Path
import re
import subprocess
import sys
import unicodedata
from typing import Any

ROOT = Path(__file__).resolve().parents[1]
MAX_INPUT_BYTES = 512
MAX_TURNS = 16
MAX_OUTPUT_BYTES = 128 * 1024
PROCESS_TIMEOUT = 5


class PilotError(RuntimeError):
    """No model/provider fallback is permitted after a pilot failure."""


def normalize(text: str) -> str:
    if not text.strip() or len(text.encode('utf-8')) > MAX_INPUT_BYTES:
        raise PilotError('Input must contain 1 through 512 UTF-8 bytes.')
    if any(unicodedata.category(char).startswith('C') for char in text):
        raise PilotError('Control and format characters are not accepted.')
    normalized = unicodedata.normalize('NFC', text).casefold().replace('\u2019', "'")
    # Remove only a vocative comma and terminal sentence punctuation. Never
    # discard negation, questions, internal punctuation, or unmatched suffixes.
    normalized = ' '.join(normalized.split())
    normalized = re.sub(r'^(hey zara|zara),\s*', r'\1 ', normalized)
    normalized = normalized.rstrip('.!?').strip()
    if not normalized or len(normalized.encode('utf-8')) > MAX_INPUT_BYTES:
        raise PilotError('Normalized input exceeds the grammar boundary.')
    return normalized


def run_process(arguments: list[str], *, input_text: str | None = None) -> subprocess.CompletedProcess[str]:
    try:
        result = subprocess.run(
            arguments, input=input_text, capture_output=True, text=True,
            encoding='utf-8', timeout=PROCESS_TIMEOUT, check=False, shell=False,
        )
    except (OSError, subprocess.TimeoutExpired, UnicodeError) as error:
        raise PilotError('The local grammar or Prolog process could not complete.') from error
    if len(result.stdout.encode('utf-8')) > MAX_OUTPUT_BYTES:
        raise PilotError('The local process exceeded the output boundary.')
    return result


def native(mode: str, text: str) -> subprocess.CompletedProcess[str]:
    binary = Path(os.environ.get('ZARA_GF_PROBE', ROOT / '.gf-build/pgf-probe'))
    grammar = ROOT / '.gf-build/Zara.pgf'
    if not binary.is_file() or not grammar.is_file():
        raise PilotError('Run scripts/test-gf.sh to build the actual PGF grammar and native probe.')
    return run_process([str(binary), mode, str(grammar), text])


def parse(text: str) -> tuple[str | None, str]:
    result = native('parse', normalize(text))
    if result.returncode == 2 and not result.stdout:
        return None, 'no_match'
    if result.returncode != 0:
        raise PilotError('PGF parsing failed or exceeded its work boundary.')
    candidates = sorted(set(result.stdout.splitlines()))
    if not candidates or any(not candidate or len(candidate.encode('utf-8')) > MAX_INPUT_BYTES for candidate in candidates):
        raise PilotError('PGF returned an invalid canonical candidate.')
    if len(candidates) != 1:
        raise PilotError('More than one meaning matched; the pilot will not choose an action.')
    return candidates[0], 'matched'


def demonstrate(utterances: list[str]) -> list[dict[str, Any]]:
    if not 1 <= len(utterances) <= MAX_TURNS:
        raise PilotError('Supply 1 through 16 utterances.')
    # Validate the entire batch before invoking native code.
    for utterance in utterances:
        normalize(utterance)
    parsed = [parse(utterance) for utterance in utterances]
    payload = json.dumps([canonical or '' for canonical, _ in parsed])
    result = run_process(
        ['swipl', '-q', '-s', str(ROOT / 'nlp/gf/demo_driver.pl')],
        input_text=payload,
    )
    if result.returncode != 0:
        raise PilotError('The canonical Prolog dialogue owner rejected the turn.')
    try:
        semantics = json.loads(result.stdout)
    except json.JSONDecodeError as error:
        raise PilotError('Invalid Prolog response document.') from error
    if not isinstance(semantics, list) or len(semantics) != len(utterances):
        raise PilotError('Prolog returned an invalid turn count.')
    outputs: list[dict[str, Any]] = []
    for utterance, (canonical, status), semantic in zip(utterances, parsed, semantics, strict=True):
        if not isinstance(semantic, dict):
            raise PilotError('Prolog returned an invalid turn.')
        tree, turn = semantic.get('reply_tree'), semantic.get('semantic_turn')
        if not isinstance(tree, str) or not isinstance(turn, str):
            raise PilotError('Prolog did not return a typed response.')
        generated = native('render', tree)
        if generated.returncode != 0 or not generated.stdout.strip():
            raise PilotError('GF could not render the canonical response act.')
        outputs.append({
            'input': utterance, 'canonical': canonical, 'status': status,
            'reply_tree': tree, 'reply': generated.stdout.strip(),
            'semantic_turn': turn, 'executed': False,
            'model_calls': 0, 'provider_calls': 0,
        })
    return outputs


def main() -> int:
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--json', action='store_true', help='Include canonical meanings and evidence.')
    parser.add_argument('utterances', nargs='+')
    args = parser.parse_args()
    try:
        results = demonstrate(args.utterances)
    except PilotError as error:
        print(str(error), file=sys.stderr)
        return 1
    if args.json:
        print(json.dumps(results, ensure_ascii=False, indent=2))
    else:
        for turn in results:
            print(f"You: {turn['input']}\nZara: {turn['reply']}\n")
    return 0


if __name__ == '__main__':
    raise SystemExit(main())
