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


def test_stream_eof_masks_every_partial_longer_secret_after_complete_short_secret() -> None:
    short = "token-123"
    long = "token-123-more"
    redactor = SecretRedactor(
        {
            _ref("SHORT"): short,
            _ref("LONG"): long,
        }
    )

    for cut in range(1, len(long)):
        prefix = long[:cut]
        stream = redactor.streaming_filter()

        assert stream.process_chunk(prefix) == ""
        final = stream.finalize()

        if cut < len(short):
            assert final == "***", f"cut={cut}"
        elif cut == len(short):
            assert final == "§§secret(SHORT)", f"cut={cut}"
        else:
            assert final == "§§secret(SHORT)***", f"cut={cut}"

        assert prefix not in final, f"raw prefix leaked at cut={cut}"
