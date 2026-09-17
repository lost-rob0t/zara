from zara.secrets import SecretKind, SecretRedactor, SecretRef, SecretScope


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


def test_stream_eof_masks_partial_longer_secret_after_complete_short_secret() -> None:
    stream = SecretRedactor(
        {
            _ref("SHORT"): "token-123",
            _ref("LONG"): "token-123-more",
        }
    ).streaming_filter()

    assert stream.process_chunk("token-123-m") == ""
    final = stream.finalize()

    assert final == "§§secret(SHORT)***"
    assert "-m" not in final
