from pathlib import Path


ROOT = Path(__file__).resolve().parents[1]
FACTORY = (
    ROOT
    / "android"
    / "app"
    / "src"
    / "main"
    / "java"
    / "ai"
    / "zara"
    / "app"
    / "prolog"
    / "AndroidPureSymbolicConversationFactory.kt"
)
INSTALLED_ACCEPTANCE = ROOT / "android" / "integration" / "device_pure_symbolic_acceptance.py"


def test_natural_expert_body_cannot_execute_through_raw_goal_shortcut() -> None:
    """Keep the canonical expert-admission blocker independent of DTO/source-shape checks."""
    factory = FACTORY.read_text(encoding="utf-8")

    assert "expertTurnEnvelopeQuery(" not in factory, (
        "Natural pure-symbolic expert execution must not run a selected raw Prolog goal. "
        "Selection may stay deterministic, but execution must cross the canonical "
        "ZARA-EXPERT/1 activation/invoke authority first."
    )
    assert "naturalExpertEvidenceRef(" not in factory, (
        "Expert evidence must come from the canonical invocation result/receipt; "
        "the conversation adapter must not fabricate evidence from predicate + turn id."
    )


def test_installed_expert_fixture_does_not_depend_on_run_as_base64_binary() -> None:
    """Pin the exact installed-emulator failure from the current exact-head artifact."""
    source = INSTALLED_ACCEPTANCE.read_text(encoding="utf-8")
    start = source.index("def install_acceptance_expert")
    end = source.index("\ndef ", start + 1)
    installer = source[start:end]

    assert "base64 -d" not in installer, (
        "The installed emulator failed before the expert transcript because the app-private "
        "run-as shell fixture depended on an external base64 command. Install the ordinary "
        "workspace fixture with a deterministic app-private write path instead."
    )
