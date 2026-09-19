from __future__ import annotations

from types import SimpleNamespace

import pytest

from zara.runtime.prolog_rlm import PrologRlmRuntimeBackend


@pytest.mark.parametrize(
    "name",
    (
        " plugin.lookup",
        "plugin.lookup ",
        "plugin.\tlookup",
        "plugin.\x7flookup",
    ),
)
def test_host_tool_names_fail_closed_instead_of_normalizing_authority(name: str) -> None:
    backend = PrologRlmRuntimeBackend(client=object())

    with pytest.raises(ValueError, match="bounded name"):
        backend.register_tools([SimpleNamespace(name=name)])

    assert backend._registered_tools == {}
