# Zara human voice fixture policy

This directory is the repository-owned human speech regression corpus.

## Mandatory workflow

- `voice-fixtures.org` at repository root is the only Emacs/Org recording workbench. Do not add a second recorder script or `.el` helper.
- Fixture intent lives in one JSON file per case under `t/fixtures/voice/cases/`.
- Human audio lives under `t/fixtures/voice/recordings/` and must be recorded as 16 kHz, mono, signed 16-bit PCM WAV.
- Every JSON case is a hard obligation. `t/test_voice_fixture_manifest.py` must fail until its declared WAV exists and passes format validation. Do not add `pending`, `optional`, `skip`, `xfail`, or generated-silence escape hatches.
- Record only synthetic/test phrases suitable for a public repository. Never record credentials, private conversations, incidental personal data, or secrets.

## Feature changes

When a Zara change adds or materially changes behavior that is exercised by real speech—dictation, wake/STT, voice commands, transcript normalization, barge-in, multi-turn voice dialogue, or voice E2E behavior—the implementing agent must review this corpus and add/update representative JSON case(s) when human speech can catch regressions that text-only tests cannot.

Creating the JSON before the recording is intentional RED. Do not weaken the repository gate to make the implementation PR green. The recording is part of acceptance.

## Persistent recording branch

Human recordings are accumulated through the single long-lived branch:

`fixtures/voice-recordings`

Use the first Emacs Lisp block in `voice-fixtures.org` to create/open the dedicated Git worktree at `~/git/worktrees/zara-voice-recordings`. It switches the Org buffer to the worktree copy and never changes the normal/master checkout.

Use the Org blocks to add JSON cases, record/re-record WAVs, run the corpus gate, commit/push the branch, and rebase the dedicated fixture branch onto `origin/master` when master moves. The rebase uses `--force-with-lease` only on `fixtures/voice-recordings`; it never rewrites or switches the normal checkout.
