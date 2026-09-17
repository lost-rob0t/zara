import pytest

from zara.secrets import SecretKind, SecretRedactor, SecretRef, SecretScope


def _ref() -> SecretRef:
    return SecretRef(
        id="secret:pathological",
        name="PATHOLOGICAL",
        scope=SecretScope.DEVICE,
        owner="test",
        kind=SecretKind.GENERIC,
        revision=1,
        generation=1,
        configured=True,
    )


def test_redaction_scan_budget_masks_unresolved_remainder_fail_closed() -> None:
    redactor = SecretRedactor(
        {_ref(): "aaaaab-secret"},
        max_scan_steps=5,
    )

    assert redactor.mask_text("aaaaac ordinary tail") == "***"

    stream = redactor.streaming_filter()
    assert stream.process_chunk("aaaaac ordinary tail") == "***"
    assert stream.pending_size == 0
    assert stream.finalize() == ""


def test_redaction_scan_budget_must_be_positive_integer() -> None:
    with pytest.raises(ValueError, match="max_scan_steps"):
        SecretRedactor({_ref(): "secret-material"}, max_scan_steps=0)

    with pytest.raises(ValueError, match="max_scan_steps"):
        SecretRedactor({_ref(): "secret-material"}, max_scan_steps=True)
