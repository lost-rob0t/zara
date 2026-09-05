from __future__ import annotations

import pytest

from zara.json_limits import (
    JsonNestingExceeded,
    MAX_UNTRUSTED_JSON_NESTING,
    require_bounded_json_nesting,
)


def _nested_array(depth: int) -> str:
    return "[" * depth + "0" + "]" * depth


def test_untrusted_json_nesting_accepts_exact_boundary_and_rejects_next_level():
    require_bounded_json_nesting(_nested_array(MAX_UNTRUSTED_JSON_NESTING))
    with pytest.raises(JsonNestingExceeded, match="nesting exceeds limit"):
        require_bounded_json_nesting(_nested_array(MAX_UNTRUSTED_JSON_NESTING + 1))


def test_untrusted_json_nesting_ignores_brackets_and_escaped_quotes_inside_strings():
    payload = r'{"text":"[[[[{{{{\\\"}}}}]]]]","nested":{"ok":true}}'
    require_bounded_json_nesting(payload)


def test_untrusted_json_nesting_limit_is_explicit_and_bounded():
    assert 16 <= MAX_UNTRUSTED_JSON_NESTING <= 128
    with pytest.raises(ValueError, match="positive integer"):
        require_bounded_json_nesting("{}", limit=0)
