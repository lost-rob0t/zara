from __future__ import annotations

import pytest

from zara.security import QuotaExceeded, SecurityLimits


def test_rate_only_control_request_is_not_blocked_by_runtime_concurrency():
    quotas = SecurityLimits(
        max_connections=1,
        max_concurrent_requests=1,
        requests_per_window=4,
        request_window_seconds=10.0,
    ).new_quota_manager()

    quotas.acquire_request("owner", now=1.0)
    quotas.consume_request_rate("owner", now=1.1)
    quotas.release_request("owner")

    # The control-plane charge did not create a hidden concurrent hold.
    quotas.acquire_request("owner", now=1.2)
    quotas.release_request("owner")


def test_rate_only_control_request_still_enforces_request_window():
    quotas = SecurityLimits(
        max_connections=1,
        max_concurrent_requests=1,
        requests_per_window=2,
        request_window_seconds=10.0,
    ).new_quota_manager()

    quotas.consume_request_rate("owner", now=1.0)
    quotas.consume_request_rate("owner", now=1.1)
    with pytest.raises(QuotaExceeded, match="request rate quota exceeded"):
        quotas.consume_request_rate("owner", now=1.2)

    # Expiry of the window restores availability deterministically.
    quotas.consume_request_rate("owner", now=11.1)
