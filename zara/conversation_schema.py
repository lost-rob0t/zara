"""Cross-platform conversation-history schema contract.

The SQLite schema is shared byte-for-byte by Zara Desktop and Zara Android.
Android receives ``conversation_schema.sql`` as a generated APK asset; Python
loads the same packaged resource with ``importlib.resources``.
"""

from __future__ import annotations

from importlib.resources import files

CONVERSATION_SCHEMA_VERSION = 4
PORTABLE_LOCAL_PRINCIPAL_ID = "local:owner"
LEGACY_LOCAL_PRINCIPAL_ID = "__zara_legacy_local_owner__"
_SCHEMA_RESOURCE = "conversation_schema.sql"


def conversation_schema_sql() -> str:
    return files("zara").joinpath(_SCHEMA_RESOURCE).read_text(encoding="utf-8")


def conversation_schema_statements() -> tuple[str, ...]:
    """Return the idempotent v4 DDL statements in execution order."""

    without_comments = "\n".join(
        line for line in conversation_schema_sql().splitlines()
        if not line.lstrip().startswith("--")
    )
    return tuple(
        statement.strip()
        for statement in without_comments.split(";")
        if statement.strip()
    )


__all__ = [
    "CONVERSATION_SCHEMA_VERSION",
    "LEGACY_LOCAL_PRINCIPAL_ID",
    "PORTABLE_LOCAL_PRINCIPAL_ID",
    "conversation_schema_sql",
    "conversation_schema_statements",
]
