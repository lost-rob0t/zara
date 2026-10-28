"""Provider-free installed error capture, persistence and clipboard acceptance."""

from __future__ import annotations

import hashlib
import struct

from device_acceptance import Device, open_menu


PRIVATE_MARKER = 'zara_diagnostic_private_fixture_1523'
HEADER = b'ZARA-CHAT-ERROR/1\n'


def saved_error_details(data: bytes) -> str:
    if len(data) > 4 * 1024 * 1024 or data.count(HEADER) != 1:
        raise AssertionError('Expected one saved chat error snapshot in bounded test history')
    offset = data.index(HEADER)
    if offset < 4:
        raise AssertionError('Saved chat error snapshot has no length')
    length = struct.unpack('>i', data[offset - 4:offset])[0]
    if not 1 <= length <= 8192 or offset + length > len(data):
        raise AssertionError('Saved chat error snapshot has invalid length')
    result = data[offset:offset + length].decode('utf-8')
    if PRIVATE_MARKER in result or 'cause_message=' in result:
        raise AssertionError('Private exception text entered saved error details')
    if 'code=local.turn.failed\n' not in result or 'cause_class=' not in result:
        raise AssertionError('Saved snapshot did not capture the actual local failure')
    return result


def read_saved_error(device: Device) -> str:
    return saved_error_details(device.adb(
        'shell', 'run-as', 'ai.zara.app', 'cat', 'files/conversations.bin', binary=True,
    ))


def exercise_chat_error(device: Device, type_text) -> dict[str, object]:
    device.await_label('Ask anything…')
    device.tap('Ask anything…')
    type_text(device, f'?- Result is 1/0, Result = {PRIVATE_MARKER}.')
    device.press_back()
    device.tap('↑')
    # The error card can extend below the composer after preceding turns.
    # Reveal the actual action before asserting or tapping it.
    device.reveal_contains('Copy error details')
    device.await_label('Copy error details', timeout=30.0)
    original = read_saved_error(device)
    device.capture('chat-error-details')
    device.tap('Copy error details')
    device.reveal_contains('Error details copied')
    device.await_label('Error details copied')
    device.capture('chat-error-details-copied')

    open_menu(device, 'Settings')
    device.tap('Diagnostics')
    device.tap('Clear diagnostics')
    open_menu(device, 'Chat')
    device.await_label('Ask anything…')
    device.tap('Ask anything…')
    type_text(device, '?- atom_concat(zara_error_, recovery_1523, Result).')
    device.press_back()
    device.tap('↑')
    device.reveal_contains('zara_error_recovery_1523')
    device.await_contains('zara_error_recovery_1523')
    if read_saved_error(device) != original:
        raise AssertionError('Later success or diagnostics clearing replaced saved error details')
    device.recreate()
    device.await_label('Ask anything…')
    if read_saved_error(device) != original:
        raise AssertionError('Restoration replaced saved error details')

    probe = device.adb(
        'shell', 'am', 'instrument', '-w', '-e', 'class',
        'ai.zara.app.ui.ChatErrorClipboardInstrumentedTest',
        'ai.zara.app.test/androidx.test.runner.AndroidJUnitRunner', timeout=90,
    )
    if 'OK (1 test)' not in probe or 'FAILURES!!!' in probe or 'INSTRUMENTATION_FAILED' in probe:
        raise AssertionError(f'Android clipboard exact-match probe failed: {probe}')
    device.start()
    device.reveal_contains('Copy error details')
    device.tap('Copy error details')
    device.reveal_contains('Error details copied')
    device.await_label('Error details copied')
    device.capture('chat-error-details-restored')
    return {
        'clipboard_exact_match': True,
        'saved_details_unchanged': True,
        'later_success': True,
        'details_sha256': hashlib.sha256(original.encode()).hexdigest(),
        'private_exception_text_excluded': True,
    }
