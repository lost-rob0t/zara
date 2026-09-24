from __future__ import annotations

import pytest

from zara.runtime.symbols import (
    ProgrammableSymbolRegistry,
    SymbolRegistrationError,
    SymbolSpec,
)


def test_replace_owner_cannot_change_kind_of_surviving_symbol():
    registry = ProgrammableSymbolRegistry()
    registry.replace_owner(
        "plugin:daily",
        (SymbolSpec("org:daily/open", "command", "v1"),),
    )

    with pytest.raises(SymbolRegistrationError, match="already registered as 'command'"):
        registry.replace_owner(
            "plugin:daily",
            (SymbolSpec("org:daily/open", "variable", "v2"),),
        )

    registration = registry.resolve("org:daily/open")
    assert registration.kind == "command"
    assert registration.value == "v1"
