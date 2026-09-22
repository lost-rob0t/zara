from __future__ import annotations

from zara.runtime.pure_symbolic_backend import PURE_SYMBOLIC_RENDER_ERROR, _resolve_turn


class _NoResultEngine:
    def query_once(self, _goal: str):
        return None


def test_desktop_renderer_gap_is_explicit_zero_model_error_turn() -> None:
    turn = _resolve_turn(
        _NoResultEngine(),
        "timer",
        "partial_frame(frame(intent(ns(timer),name(set)),[],missing([duration])),[duration])",
    )

    assert turn.response == PURE_SYMBOLIC_RENDER_ERROR
    assert turn.act_term == "error(renderer_unavailable)"
    assert turn.context_term == "[]"
    assert turn.provider_calls == 0
    assert turn.model_calls == 0
