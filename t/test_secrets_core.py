from __future__ import annotations

import pytest

from zara.secrets import (
    SecretAliasError,
    SecretKind,
    SecretRedactor,
    SecretRef,
    SecretScope,
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


def test_redactor_repr_never_contains_secret_material() -> None:
    secret = ref("PROVIDER_KEY")
    raw = "sk-prod-AbC123456"
    redactor = SecretRedactor({secret: raw})

    rendered = repr(redactor)
    assert raw not in rendered
    assert "PROVIDER_KEY" not in rendered
    assert "SecretRedactor" in rendered
