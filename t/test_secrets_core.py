from __future__ import annotations

import pickle

import pytest

from zara.secrets import (
    SecretAliasError,
    SecretKind,
    SecretLease,
    SecretLeaseError,
    SecretRedactor,
    SecretRef,
    SecretScope,
    SecretSink,
    SecretStore,
    SecretUseContext,
    alias_for_secret,
    find_secret_aliases,
    parse_secret_alias,
)


def ref(name: str, *, secret_id: str | None = None) -> SecretRef:
    return SecretRef(
        id=secret_id or f"secret:{name.lower()}",
        name=name,
        scope=SecretScope.DEVICE,
        owner="test",
        kind=SecretKind.API_KEY,
        revision=1,
        generation=1,
        configured=True,
    )


def use_context() -> SecretUseContext:
    return SecretUseContext(
        principal="local:owner",
        runtime_generation=7,
        consumer="provider:elevenlabs",
        purpose="tts.synthesize",
        sink=SecretSink.HTTP_HEADER,
        request_id="request-123",
    )


def test_alias_round_trip_is_canonical_and_plaintext_free() -> None:
    secret = ref("elevenlabs_api_key")

    assert secret.name == "ELEVENLABS_API_KEY"
    assert secret.alias == "§§secret(ELEVENLABS_API_KEY)"
    assert alias_for_secret("elevenlabs_api_key") == secret.alias
    assert parse_secret_alias(secret.alias) == "ELEVENLABS_API_KEY"
    assert secret.to_public_dict() == {
        "id": "secret:elevenlabs_api_key",
        "name": "ELEVENLABS_API_KEY",
        "alias": "§§secret(ELEVENLABS_API_KEY)",
        "scope": "device",
        "owner": "test",
        "kind": "api_key",
        "revision": 1,
        "generation": 1,
        "configured": True,
    }


def test_secret_ref_rejects_bool_as_revision_or_generation() -> None:
    with pytest.raises(ValueError):
        SecretRef(
            id="secret:x",
            name="X",
            scope=SecretScope.DEVICE,
            owner="test",
            kind=SecretKind.GENERIC,
            revision=True,
            generation=1,
            configured=True,
        )

    with pytest.raises(ValueError):
        SecretRef(
            id="secret:x",
            name="X",
            scope=SecretScope.DEVICE,
            owner="test",
            kind=SecretKind.GENERIC,
            revision=1,
            generation=False,
            configured=True,
        )


def test_alias_parser_is_strict_and_does_not_treat_arbitrary_text_as_secret() -> None:
    assert find_secret_aliases(
        "Bearer §§secret(API_TOKEN); ssh §§secret(SSH_KEY)"
    ) == ("API_TOKEN", "SSH_KEY")

    for value in (
        "secret(API_TOKEN)",
        "§§secret(api-token)",
        "§§secret(1TOKEN)",
        "§§secret(API TOKEN)",
        "§§secret()",
    ):
        with pytest.raises(SecretAliasError):
            parse_secret_alias(value)


def test_invalid_secret_names_fail_closed() -> None:
    with pytest.raises(SecretAliasError):
        ref("bad-name")
    with pytest.raises(SecretAliasError):
        alias_for_secret("1BAD")


def test_use_context_has_bounded_plaintext_free_public_projection() -> None:
    context = use_context()

    assert context.to_public_dict() == {
        "principal": "local:owner",
        "runtime_generation": 7,
        "consumer": "provider:elevenlabs",
        "purpose": "tts.synthesize",
        "sink": "http_header",
        "request_id": "request-123",
    }

    with pytest.raises(ValueError):
        SecretUseContext(
            principal="local:owner\nforged",
            runtime_generation=7,
            consumer="provider:elevenlabs",
            purpose="tts.synthesize",
            sink=SecretSink.HTTP_HEADER,
        )


def test_secret_lease_is_runtime_issued_non_serializable_and_plaintext_free() -> None:
    secret = ref("ELEVENLABS_API_KEY")
    context = use_context()

    with pytest.raises(SecretLeaseError, match="runtime-issued"):
        SecretLease()

    lease = SecretLease._issue(
        secret,
        context,
        ttl_seconds=30.0,
    )

    assert lease.secret_id == secret.id
    assert lease.secret_revision == secret.revision
    assert lease.secret_generation == secret.generation
    assert lease.consumer == context.consumer
    assert lease.sink is SecretSink.HTTP_HEADER
    assert not lease.closed
    lease.assert_usable(now=lease.expires_at_monotonic - 0.001)

    rendered = repr(lease)
    assert secret.id not in rendered
    assert secret.name not in rendered
    assert context.consumer not in rendered
    assert "SecretLease" in rendered

    with pytest.raises(TypeError, match="serialized"):
        pickle.dumps(lease)

    lease.close()
    assert lease.closed
    with pytest.raises(SecretLeaseError, match="closed"):
        lease.assert_usable(now=lease.expires_at_monotonic - 0.001)


def test_secret_lease_expiry_is_fail_closed() -> None:
    lease = SecretLease._issue(ref("TOKEN"), use_context(), ttl_seconds=1.0)

    with pytest.raises(SecretLeaseError, match="expired"):
        lease.assert_usable(now=lease.expires_at_monotonic + 0.001)


def test_secret_store_protocol_exposes_refs_and_leases_not_plaintext_getter() -> None:
    class FakeStore:
        def list_refs(self) -> tuple[SecretRef, ...]:
            return ()

        def get_ref(self, secret_id: str) -> SecretRef | None:
            return None

        def resolve(
            self,
            secret: SecretRef,
            context: SecretUseContext,
            *,
            ttl_seconds: float = 30.0,
        ) -> SecretLease:
            return SecretLease._issue(secret, context, ttl_seconds=ttl_seconds)

        def close_lease(self, lease: SecretLease) -> None:
            lease.close()

    store = FakeStore()
    assert isinstance(store, SecretStore)
    assert not hasattr(store, "get_secret_value")
    assert not hasattr(store, "materialize")


def test_redactor_masks_complete_values_longest_first() -> None:
    short = ref("SHORT")
    long = ref("LONG")
    redactor = SecretRedactor(
        {
            short: "abc123",
            long: "abc123-more-private",
        }
    )

    assert redactor.mask_text(
        "first=abc123-more-private second=abc123"
    ) == (
        "first=§§secret(LONG) second=§§secret(SHORT)"
    )


def test_mask_text_does_not_reprocess_generated_aliases_as_raw_secret_material() -> None:
    first = ref("A")
    second = ref("B")
    redactor = SecretRedactor(
        {
            first: "a-very-long-secret-value",
            second: "secret(A)",
        }
    )

    assert redactor.mask_text("a-very-long-secret-value") == "§§secret(A)"


def test_mask_text_keeps_incomplete_secret_prefix_as_ordinary_complete_text() -> None:
    secret = ref("PROVIDER_KEY")
    redactor = SecretRedactor({secret: "sk-prod-AbC123456"})

    assert redactor.mask_text("ordinary text ending in s") == "ordinary text ending in s"


def test_duplicate_material_uses_generic_redaction_instead_of_guessing_alias() -> None:
    redactor = SecretRedactor(
        {
            ref("A", secret_id="secret:a"): "same-secret-value",
            ref("B", secret_id="secret:b"): "same-secret-value",
        }
    )

    assert redactor.mask_text("same-secret-value") == "***"


def test_stream_filter_never_emits_secret_prefix_split_across_chunks() -> None:
    secret = ref("PROVIDER_KEY")
    raw = "sk-prod-AbC123456"
    stream = SecretRedactor({secret: raw}).streaming_filter()

    assert stream.process_chunk("Authorization: Bearer sk-prod-") == (
        "Authorization: Bearer "
    )
    assert stream.process_chunk("AbC123456 tail") == (
        "§§secret(PROVIDER_KEY) tail"
    )
    assert stream.finalize() == ""


def test_stream_filter_does_not_leak_even_a_short_prefix_of_scannable_secret() -> None:
    secret = ref("PROVIDER_KEY")
    stream = SecretRedactor({secret: "sk-prod-AbC123456"}).streaming_filter()

    assert stream.process_chunk("prefix s") == "prefix "
    assert stream.process_chunk("k-prod-AbC123456") == "§§secret(PROVIDER_KEY)"
    assert stream.finalize() == ""


def test_stream_filter_holds_exact_short_secret_when_it_can_extend_to_longer_secret() -> None:
    short = ref("SHORT")
    long = ref("LONG")
    stream = SecretRedactor(
        {
            short: "token-123",
            long: "token-123-more",
        }
    ).streaming_filter()

    assert stream.process_chunk("token-123") == ""
    assert stream.process_chunk("-more") == "§§secret(LONG)"
    assert stream.finalize() == ""


def test_stream_filter_emits_short_secret_once_longer_candidate_is_disproved() -> None:
    short = ref("SHORT")
    long = ref("LONG")
    stream = SecretRedactor(
        {
            short: "token-123",
            long: "token-123-more",
        }
    ).streaming_filter()

    assert stream.process_chunk("token-123") == ""
    assert stream.process_chunk("!") == "§§secret(SHORT)!"
    assert stream.finalize() == ""


def test_stream_filter_matches_full_mask_for_every_two_chunk_boundary() -> None:
    secret = ref("PROVIDER_KEY")
    raw = "sk-prod-AbC123456"
    redactor = SecretRedactor({secret: raw})
    payload = f"before {raw} after"
    expected = redactor.mask_text(payload)

    for split in range(len(payload) + 1):
        stream = redactor.streaming_filter()
        actual = (
            stream.process_chunk(payload[:split])
            + stream.process_chunk(payload[split:])
            + stream.finalize()
        )
        assert actual == expected, f"split={split}"
        assert raw not in actual


def test_stream_filter_matches_full_mask_one_character_at_a_time() -> None:
    secret = ref("PROVIDER_KEY")
    raw = "sk-prod-AbC123456"
    redactor = SecretRedactor({secret: raw})
    payload = f"before {raw} after"
    stream = redactor.streaming_filter()

    actual = "".join(stream.process_chunk(char) for char in payload)
    actual += stream.finalize()

    assert actual == redactor.mask_text(payload)
    assert raw not in actual


def test_stream_finalize_masks_unresolved_secret_prefix() -> None:
    secret = ref("PROVIDER_KEY")
    stream = SecretRedactor({secret: "sk-prod-AbC123456"}).streaming_filter()

    assert stream.process_chunk("value=sk-prod-AbC") == "value="
    assert stream.finalize() == "***"


def test_stream_finalize_masks_one_character_prefix_of_scannable_secret() -> None:
    secret = ref("PROVIDER_KEY")
    stream = SecretRedactor({secret: "sk-prod-AbC123456"}).streaming_filter()

    assert stream.process_chunk("value=s") == "value="
    assert stream.finalize() == "***"


def test_stream_filter_releases_non_secret_suffix_without_unbounded_delay() -> None:
    secret = ref("PROVIDER_KEY")
    stream = SecretRedactor({secret: "sk-prod-AbC123456"}).streaming_filter()

    assert stream.process_chunk("ordinary sk-prox text") == "ordinary sk-prox text"
    assert stream.pending_size == 0


def test_free_text_scan_ignores_very_short_material_by_default() -> None:
    secret = ref("PIN")
    redactor = SecretRedactor({secret: "abc"})

    assert redactor.mask_text("alphabet abc xyz") == "alphabet abc xyz"
    assert redactor.skipped_short_secret_count == 1


def test_redactor_bounds_secret_count_and_material_length() -> None:
    secrets = {
        ref("A", secret_id="secret:a"): "secret-a",
        ref("B", secret_id="secret:b"): "secret-b",
        ref("C", secret_id="secret:c"): "secret-c",
    }
    with pytest.raises(ValueError, match="secret count"):
        SecretRedactor(secrets, max_secret_count=2)

    with pytest.raises(ValueError, match="maximum redaction length"):
        SecretRedactor(
            {ref("LONG"): "x" * 33},
            max_secret_length=32,
        )


def test_redactor_bounds_total_scannable_material() -> None:
    secrets = {
        ref("A", secret_id="secret:a"): "abcdefgh",
        ref("B", secret_id="secret:b"): "ijklmnop",
    }
    with pytest.raises(ValueError, match="total secret material"):
        SecretRedactor(secrets, max_total_secret_chars=15)


def test_redactor_repr_never_contains_secret_material() -> None:
    secret = ref("PROVIDER_KEY")
    raw = "sk-prod-AbC123456"
    redactor = SecretRedactor({secret: raw})

    rendered = repr(redactor)
    assert raw not in rendered
    assert "PROVIDER_KEY" not in rendered
    assert "SecretRedactor" in rendered
