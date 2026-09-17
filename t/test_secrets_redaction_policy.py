import pytest

from zara.secrets import (
    SecretKind,
    SecretLease,
    SecretRedactor,
    SecretRef,
    SecretScope,
    SecretSink,
    SecretUseContext,
)


def _ref(name: str) -> SecretRef:
    return SecretRef(
        id=f"secret:{name.lower()}",
        name=name,
        scope=SecretScope.DEVICE,
        owner="test",
        kind=SecretKind.API_KEY,
        revision=1,
        generation=1,
        configured=True,
    )


def _context() -> SecretUseContext:
    return SecretUseContext(
        principal="local:owner",
        runtime_generation=1,
        consumer="provider:test",
        purpose="test.request",
        sink=SecretSink.HTTP_HEADER,
    )


def test_redactor_can_hide_alias_names_when_attribution_is_sensitive() -> None:
    raw = "provider-secret-material"
    redactor = SecretRedactor({_ref("PRIVATE_ACCOUNT_TOKEN"): raw}, reveal_aliases=False)

    assert redactor.mask_text(f"token={raw}") == "token=***"

    stream = redactor.streaming_filter()
    assert stream.process_chunk("provider-secret-") == ""
    assert stream.process_chunk("material") == "***"
    assert stream.finalize() == ""


def test_lease_ttl_has_hard_five_minute_cap() -> None:
    with pytest.raises(ValueError, match="300"):
        SecretLease._issue(_ref("TOKEN"), _context(), ttl_seconds=300.001)

    lease = SecretLease._issue(_ref("TOKEN"), _context(), ttl_seconds=300.0)
    lease.assert_usable(now=lease.expires_at_monotonic - 0.001)
