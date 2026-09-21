from pathlib import Path


ROOT = Path(__file__).resolve().parent.parent
CONTROLLER = ROOT / "android/app/src/main/java/ai/zara/app/prolog/PureSymbolicConversationController.kt"
FACTORY = ROOT / "android/app/src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"


def test_natural_symbolic_result_reuses_canonical_persisted_turn_identity():
    controller = CONTROLLER.read_text(encoding="utf-8")
    factory = FACTORY.read_text(encoding="utf-8")

    assert "data class PureSymbolicResolution(" in controller
    assert "val resolve: (String, String) -> PureSymbolicResolution" in controller
    assert "val turnId: String?" in controller
    assert "turnId = routed.turnId ?: nextTurnId()" in controller
    assert "turnId: String? = null" in controller

    assert "private fun resolvePersistedTurn(" in factory
    assert "): PureSymbolicResolution" in factory
    assert "PureSymbolicResolution(" in factory
    assert "turnId = turnId" in factory


def test_natural_symbolic_failure_keeps_canonical_identity_when_resolution_started():
    controller = CONTROLLER.read_text(encoding="utf-8")

    assert "failure(" in controller
    assert "turnId = routed.turnId" in controller
    assert "turnId = turnId ?: nextTurnId()" in controller
