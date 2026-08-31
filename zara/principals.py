"""Canonical principal identity shared by daemon entrypoints and runtime code.

The packaged server entrypoint executes ``zara/server.py`` as ``__main__``
while transport and runtime modules import it again as ``zara.server``.
Defining ``PrincipalContext`` here keeps one class object across both copies
so isinstance-based principal checks hold under ``python -m zara.server``.
"""

from __future__ import annotations

import os
from dataclasses import dataclass

__all__ = ["PrincipalContext"]


@dataclass(frozen=True)
class PrincipalContext:
    principal_id: str
    kind: str = "synthetic"

    def __post_init__(self) -> None:
        if not isinstance(self.principal_id, str) or not self.principal_id.strip():
            raise ValueError("principal_id must be a non-empty string")
        if self.principal_id != self.principal_id.strip():
            raise ValueError("principal_id must not contain leading or trailing whitespace")
        if not isinstance(self.kind, str) or not self.kind.strip():
            raise ValueError("principal kind must be a non-empty string")
        if self.kind != self.kind.strip():
            raise ValueError("principal kind must not contain leading or trailing whitespace")

    @classmethod
    def local_owner(cls) -> "PrincipalContext":
        return cls(principal_id=f"uid:{os.getuid()}", kind="local-owner")
