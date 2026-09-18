from __future__ import annotations

import pytest

from zara.runtime.prolog_rlm import PrologRlmRuntimeBackend


@pytest.mark.parametrize(
    "principal_id",
    (
        "p" * 129,
        "local\nadmin",
        "local\x00admin",
    ),
)
def test_prolog_rlm_rejects_unbounded_or_controlled_principal_ids(principal_id: str) -> None:
    with pytest.raises(ValueError):
        PrologRlmRuntimeBackend(client=object(), principal_id=principal_id)
