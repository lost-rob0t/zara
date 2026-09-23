from __future__ import annotations

import threading

import pytest

from zara.pairing_code import (
    PairingCodeError,
    PairingCodeExpired,
    PairingCodeRegistry,
    generate_pairing_code,
    normalize_pairing_code,
    render_pairing_code,
)


PAIRING_URI = (
    "zara://pair/v1?broker_host=127.0.0.1&broker_port=43210"
    "&endpoint=tcp%3A%2F%2F127.0.0.1%3A17865"
    "&server_key=" + "a" * 40 + "&token=pairing-token&expires=2000"
)


def test_pairing_code_normalizes_case_and_three_letter_display_groups() -> None:
    assert normalize_pairing_code("abc-def-ghi-jkl-mno-p") == "ABCDEFGHIJKLMNOP"
    assert normalize_pairing_code("ABC DEF GHI JKL MNO P") == "ABCDEFGHIJKLMNOP"
    assert render_pairing_code("abcdefghijklmnop") == "ABC-DEF-GHI-JKL-MNO-P"


@pytest.mark.parametrize(
    "raw",
    [
        "ABC-DEF-GHI-JKL-MNO",
        "ABC-DEF-GHI-JKL-MNO-PQ",
        "ABC-DEF-GHI-JKL-MN0-P",
        "ABC_DEF_GHI_JKL_MNO_P",
        "ÁBC-DEF-GHI-JKL-MNO-P",
    ],
)
def test_pairing_code_rejects_wrong_length_digits_punctuation_and_non_ascii(raw: str) -> None:
    with pytest.raises(PairingCodeError):
        normalize_pairing_code(raw)


def test_generated_pairing_code_is_exactly_sixteen_ascii_letters() -> None:
    code = generate_pairing_code()
    assert len(code) == 16
    assert code.isascii()
    assert code.isalpha()
    assert code == code.upper()
    assert render_pairing_code(code).count("-") == 5


def test_pairing_code_registry_claim_is_single_use_and_case_insensitive(monkeypatch) -> None:
    monkeypatch.setattr("zara.pairing_code.generate_pairing_code", lambda: "ABCDEFGHIJKLMNOP")
    registry = PairingCodeRegistry()
    lease = registry.issue(PAIRING_URI, ttl_seconds=120, now=1000)

    assert lease.code == "ABCDEFGHIJKLMNOP"
    assert lease.expires_at == 1120
    assert registry.claim("abc-def-ghi-jkl-mno-p", now=1001) == PAIRING_URI
    with pytest.raises(PairingCodeError, match="already used"):
        registry.claim("ABCDEFGHIJKLMNOP", now=1002)


def test_pairing_code_registry_expiry_consumes_the_expired_alias(monkeypatch) -> None:
    monkeypatch.setattr("zara.pairing_code.generate_pairing_code", lambda: "ABCDEFGHIJKLMNOP")
    registry = PairingCodeRegistry()
    registry.issue(PAIRING_URI, ttl_seconds=15, now=1000)

    with pytest.raises(PairingCodeExpired, match="expired"):
        registry.claim("ABC-DEF-GHI-JKL-MNO-P", now=1015)
    with pytest.raises(PairingCodeError, match="already used"):
        registry.claim("ABCDEFGHIJKLMNOP", now=1016)


def test_pairing_code_registry_allows_only_one_concurrent_claim(monkeypatch) -> None:
    monkeypatch.setattr("zara.pairing_code.generate_pairing_code", lambda: "ABCDEFGHIJKLMNOP")
    registry = PairingCodeRegistry()
    registry.issue(PAIRING_URI, ttl_seconds=120, now=1000)
    barrier = threading.Barrier(8)
    outcomes: list[str] = []
    lock = threading.Lock()

    def claim() -> None:
        barrier.wait()
        try:
            outcome = registry.claim("abc-def-ghi-jkl-mno-p", now=1001)
        except PairingCodeError:
            outcome = "rejected"
        with lock:
            outcomes.append(outcome)

    threads = [threading.Thread(target=claim) for _ in range(8)]
    for thread in threads:
        thread.start()
    for thread in threads:
        thread.join(timeout=2.0)
        assert not thread.is_alive()

    assert outcomes.count(PAIRING_URI) == 1
    assert outcomes.count("rejected") == 7


def test_pairing_code_registry_fails_closed_on_non_pairing_uri_and_bad_ttl() -> None:
    registry = PairingCodeRegistry()
    with pytest.raises(PairingCodeError, match="zara://pair/v1"):
        registry.issue("https://example.invalid/pair", ttl_seconds=120, now=1000)
    with pytest.raises(PairingCodeError, match="lifetime"):
        registry.issue(PAIRING_URI, ttl_seconds=14, now=1000)
    with pytest.raises(PairingCodeError, match="lifetime"):
        registry.issue(PAIRING_URI, ttl_seconds=601, now=1000)
