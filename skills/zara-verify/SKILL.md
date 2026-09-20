---
name: zara-verify
description: Mandatory exact-source Zara implementation verification through ZARA-VERIFY/1 and the Core verifier expert.
---
Read `contracts/zara-verify-v1/SPEC.md`. Enter `nix develop` before collecting
local evidence. From OpenCode call `zara_verify` with `operation=plan` before
edits, `operation=run` after the last change/commit, then `operation=status`.
Run IDs, source/policy digests, gate selection, commands and verdicts are
host-owned. Never synthesize them. Preserve failures and report BLOCKED while
required evidence is missing. Registry registration is trusted host bootstrap;
no model tool may register a replacement verifier or grant itself authority.
