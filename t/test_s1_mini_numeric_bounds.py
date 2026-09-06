from __future__ import annotations

import math

import pytest

from zara.s1_mini_normalizer import S1MiniTranscriptNormalizer


@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf])
def test_s1_mini_rejects_non_finite_expansion_ratio(value: float):
    with pytest.raises(ValueError, match="max_expansion_ratio must be finite and positive"):
        S1MiniTranscriptNormalizer(
            endpoint="http://127.0.0.1:8000",
            max_expansion_ratio=value,
        )


@pytest.mark.parametrize("value", [math.nan, math.inf, -math.inf])
def test_s1_mini_rejects_non_finite_request_timeout(value: float):
    with pytest.raises(ValueError, match="request_timeout must be finite and positive"):
        S1MiniTranscriptNormalizer(
            endpoint="http://127.0.0.1:8000",
            request_timeout=value,
        )
