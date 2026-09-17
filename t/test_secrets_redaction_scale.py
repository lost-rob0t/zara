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


def test_large_ordinary_chunk_round_trips_without_pending_growth() -> None:
    redactor = SecretRedactor({_ref("KEY"): "xy-secret-material"})
    payload = "x" * 65_536
    stream = redactor.streaming_filter()

    assert stream.process_chunk(payload) == payload
    assert stream.pending_size == 0
    assert stream.finalize() == ""


def test_large_complete_text_round_trips_without_alias_reprocessing() -> None:
    redactor = SecretRedactor({_ref("KEY"): "xy-secret-material"})
    payload = ("ordinary-text-" * 4096) + "tail"

    assert redactor.mask_text(payload) == payload
