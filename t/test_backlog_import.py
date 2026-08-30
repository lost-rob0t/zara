import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
SCRIPT = REPO / "scripts" / "import-backlog.py"

sys.path.insert(0, str(SCRIPT.parent))

import importlib.util

spec = importlib.util.spec_from_file_location("import_backlog", SCRIPT)
mod = importlib.util.module_from_spec(spec)
spec.loader.exec_module(mod)


def test_parse_depends_extracts_issue_ids():
    body = "Parent: #152\nDepends on: #171, #156\nDepends on: #133\n"
    assert mod.parse_depends(body) == [171, 156, 133]


def test_parse_depends_handles_integrates_and_prose():
    body = (
        "Integrates with: #132, #133, #134, #150, #151, #152, #159, #170-#179\n"
        "Some prose mentioning #999 casually but not as a dependency.\n"
        "Depends on: #122\n"
    )
    assert mod.parse_depends(body) == [122]


def test_parse_depends_empty_body():
    assert mod.parse_depends("") == []
    assert mod.parse_depends(None) == []


def test_parse_parent_single_epic():
    assert mod.parse_parent("Parent: #152\nDepends on: #170") == 152
    assert mod.parse_parent("no parent here") is None


def test_parse_priority_from_title_prefix():
    assert mod.parse_priority("P0 — Daemon release gate") == 0
    assert mod.parse_priority("P1 — wire transcript normalization") == 1
    assert mod.parse_priority("P2 — add the in-repo Android project skeleton") == 2
    assert mod.parse_priority("no prefix here") is None
    assert mod.parse_priority("P4 — future") is None


def test_render_backlog_deterministic_and_complete():
    issues = [
        mod.GhIssue(171, 2, "P2 — add the in-repo Android project skeleton", "Parent: #152", True),
        mod.GhIssue(170, 2, "P2 — RAGE Android Prolog/ZeroMQ/NDK/WASM feasibility bakeoff and architecture decision", "Parent: #152\nDepends on: #133", True),
        mod.GhIssue(172, 2, "P2 — extract and prove a portable shared Prolog semantic core", "Parent: #152\nDepends on: #171, #156", False),
        mod.GhIssue(133, None, "old closed issue", "", True),
        mod.GhIssue(156, 1, "P1 — Prolog IntentFrame adaptation + corpus", "Parent: #150\nDepends on: #155", False),
        mod.GhIssue(155, 1, "P1 — Typed slots + clarification dialogue", "Parent: #150\nDepends on: #154", False),
        mod.GhIssue(154, 1, "P1 — RAGE research the post-client-split semantic intent architecture and freeze an IntentFrame contract", "Parent: #150\nDepends on: #133", False),
        mod.GhIssue(152, 1, "EPIC — Android client", "", False),
        mod.GhIssue(150, 1, "EPIC — semantic intents", "", False),
    ]
    phases = ["phase(7, [170, 171, 172]).", "phase(5, [154, 155, 156])."]
    out = mod.render_backlog(issues, phases, "2026-08-30")

    assert "closed(133). closed(170). closed(171)." in out
    assert "issue(172, p2, open," in out
    assert "depends_on(172, 171)." in out
    assert "depends_on(172, 156)." in out
    assert "epic_children(152, [170,171,172])." in out
    assert "epic_children(150, [154,155,156])." in out
    assert "master_last_merge(2026-08-30)." in out
    assert "phase(7, [170, 171, 172])." in out
    assert out.endswith("\n")
    assert out == mod.render_backlog(list(reversed(issues)), list(reversed(phases)), "2026-08-30")


def test_render_backlog_loads_in_prolog():
    issues = [
        mod.GhIssue(171, 2, "P2 — skeleton", "", False),
        mod.GhIssue(170, 2, "P2 — bakeoff", "Depends on: #133", False),
        mod.GhIssue(172, 2, "P2 — core", "Parent: #152\nDepends on: #171, #156", False),
        mod.GhIssue(156, 1, "P1 — adaptation", "Depends on: #155", False),
        mod.GhIssue(133, None, "closed", "", True),
    ]
    out = mod.render_backlog(issues, ["phase(7, [170, 171, 172])."], "2026-08-30")
    tmp = REPO / "t" / ".backlog-render-check.pl"
    tmp.write_text(out)
    try:
        proc = subprocess.run(
            ["swipl", "-q", "-g", "consult('t/.backlog-render-check.pl'), "
             "(blockers(172, Bs) -> format(\"~w\", [Bs]) ; write(none)), halt",
             "-t", "halt(1)"],
            capture_output=True, text=True, cwd=REPO, timeout=60,
        )
        assert proc.returncode == 0, proc.stderr
        assert "156" in proc.stdout
    finally:
        tmp.unlink(missing_ok=True)
