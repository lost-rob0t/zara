## Scope

Describe the behavior changed and the exact user-visible/runtime contract affected.

## Test depth

Every behavior-changing PR must show at least two distinct verification layers in addition to the repository-wide gate. Prefer a focused unit/contract layer plus an integration/E2E/system layer. Do not count the same test rerun twice.

- [ ] Focused RED test reproduced the required behavior/regression before implementation, or the harness exception is documented.
- [ ] Focused GREEN test passes at the candidate head.
- [ ] A second distinct layer covers the same behavior or its integration boundary.
- [ ] Failure, boundary, restart/persistence, permission/offline, race/timeout, or security paths relevant to this change are covered.
- [ ] `nix develop -c bash scripts/test-all.sh` passes.
- [ ] `nix flake check` passes.
- [ ] Android/Wear gate passes when shared or Android code is affected.
- [ ] CI `deep regression` matrix passes for the exact candidate SHA.

### Focused evidence

| Layer | Command / test | Result |
| --- | --- | --- |
| 1 |  |  |
| 2 |  |  |

## Screenshot evidence

Every PR must retain screenshots from both product surfaces, even when the change is not primarily visual. Non-UI work uses the canonical smoke states to detect incidental regressions.

- [ ] Desktop screenshot artifact is attached by CI (`regression-artifacts`, `ui/`).
- [ ] Android screenshot artifact is attached by CI (`android-ui-evidence`).
- [ ] CI `dual-surface screenshot evidence` passes for the exact reviewed head SHA.
- [ ] Relevant changed states were visually reviewed rather than relying only on widget/unit tests.

### Visual review notes

Desktop:

Android:

## Regression / compatibility

- [ ] Shared behavior was checked for desktop/Android parity where applicable.
- [ ] Local-first/offline behavior remains truthful.
- [ ] Persistent history/config/data migrations remain compatible where applicable.
- [ ] No secrets, credentials, transcripts, or private user data were added to logs or artifacts.
