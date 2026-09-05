"""Deterministic structural limits for untrusted JSON before parsing.

The stdlib JSON decoder's recursion behaviour varies with Python version and
interpreter limits. Security boundaries must not depend on eventually hitting
that implementation detail, so Zara rejects excessive container nesting with
an iterative lexical scan before handing input to ``json.loads``.
"""

from __future__ import annotations

MAX_UNTRUSTED_JSON_NESTING = 64


class JsonNestingExceeded(ValueError):
    """Raised when an untrusted JSON document exceeds Zara's nesting limit."""


def require_bounded_json_nesting(
    text: str,
    *,
    limit: int = MAX_UNTRUSTED_JSON_NESTING,
) -> None:
    """Reject JSON container nesting deeper than ``limit`` without recursion.

    This deliberately does not validate JSON syntax; ``json.loads`` remains
    authoritative for that. Brackets inside JSON strings are ignored, including
    escaped quotes/backslashes, so the scan cannot be tricked by string content.
    """
    if not isinstance(text, str):
        raise TypeError("text must be str")
    if not isinstance(limit, int) or isinstance(limit, bool) or limit < 1:
        raise ValueError("limit must be a positive integer")

    depth = 0
    in_string = False
    escaped = False
    for character in text:
        if in_string:
            if escaped:
                escaped = False
            elif character == "\\":
                escaped = True
            elif character == '"':
                in_string = False
            continue

        if character == '"':
            in_string = True
            continue
        if character in "[{":
            depth += 1
            if depth > limit:
                raise JsonNestingExceeded("JSON nesting exceeds limit")
        elif character in "]}":
            depth = max(0, depth - 1)


__all__ = [
    "JsonNestingExceeded",
    "MAX_UNTRUSTED_JSON_NESTING",
    "require_bounded_json_nesting",
]
