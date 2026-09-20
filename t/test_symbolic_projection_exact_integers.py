from __future__ import annotations

from dataclasses import replace

import pytest

from zara.desktop.conversation import SymbolicConversationProjection


def _projection() -> SymbolicConversationProjection:
    return SymbolicConversationProjection(
        conversation_id="conv-exact-int",
        projection_generation=1,
        runtime_generation=1,
        project_generation=1,
        providers_enabled=False,
        max_model_calls=0,
        provider_calls=0,
        model_calls=0,
    )


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("provider_calls", False),
        ("provider_calls", 0.0),
        ("model_calls", False),
        ("model_calls", 0.0),
    ],
)
def test_pure_symbolic_zero_ledger_requires_exact_integer_zero(field: str, value: object) -> None:
    projection = replace(_projection(), **{field: value})

    with pytest.raises(TypeError, match=rf"{field} must be an exact integer"):
        projection.validate()

    with pytest.raises(AssertionError, match=field):
        projection.assert_pure_symbolic()


@pytest.mark.parametrize(
    ("field", "value"),
    [
        ("projection_generation", True),
        ("projection_generation", 1.0),
        ("runtime_generation", False),
        ("runtime_generation", 0.0),
        ("project_generation", False),
        ("project_generation", 0.0),
    ],
)
def test_generation_fences_require_exact_integer_values(field: str, value: object) -> None:
    projection = replace(_projection(), **{field: value})

    with pytest.raises(TypeError, match=rf"{field} must be an exact integer"):
        projection.validate()
