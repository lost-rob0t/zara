from __future__ import annotations

import pytest

from zara.runtime.pure_symbolic_backend import _resolve_turn


class _NoResultEngine:
    def query_once(self, _goal: str):
        return None


def test_desktop_renderer_gap_fails_closed_instead_of_returning_terminal_success() -> None:
    with pytest.raises(RuntimeError, match="canonical symbolic dialogue result"):
        _resolve_turn(
            _NoResultEngine(),
            "timer",
            "partial_frame(frame(intent(ns(timer),name(set)),[],missing([duration])),[duration])",
        )
