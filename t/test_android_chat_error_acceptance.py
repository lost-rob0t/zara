from __future__ import annotations

import importlib.util
from pathlib import Path
import struct
import sys
import unittest
from unittest.mock import patch

ROOT = Path(__file__).resolve().parents[1]
sys.path.insert(0, str(ROOT / 'android/integration'))


class ChatErrorAcceptanceTest(unittest.TestCase):
    def setUp(self):
        spec = importlib.util.spec_from_file_location(
            'chat_error_acceptance', ROOT / 'android/integration/chat_error_acceptance.py'
        )
        self.module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(self.module)
        self.snapshot = b'ZARA-CHAT-ERROR/1\ncode=local.turn.failed\ncause_class=fixture\n'
        self.stored = b'fixture' + struct.pack('>i', len(self.snapshot)) + self.snapshot + b'end'

    def test_extracts_exact_bounded_saved_snapshot(self):
        self.assertEqual(self.snapshot.decode(), self.module.saved_error_details(self.stored))

    def test_rejects_missing_duplicate_truncated_and_oversize_snapshots(self):
        for data in (b'', self.stored * 2, self.stored[:30], b'x' * (4 * 1024 * 1024 + 1),
                     struct.pack('>i', 9000) + self.snapshot):
            with self.subTest(size=len(data)), self.assertRaises(AssertionError):
                self.module.saved_error_details(data)

    def test_emulator_flow_requires_clipboard_probe_and_retained_snapshot(self):
        device = FakeDevice(self.stored)
        with patch.object(self.module, 'open_menu', side_effect=lambda d, menu: d.tap(menu)):
            result = self.module.exercise_chat_error(device, lambda d, text: d.calls.append(('type', text)))
        self.assertTrue(result['clipboard_exact_match'])
        self.assertTrue(result['saved_details_unchanged'])
        self.assertTrue(result['later_success'])
        self.assertIn(('tap', 'Clear diagnostics'), device.calls)
        self.assertIn(('recreate',), device.calls)
        for state in ('chat-error-details', 'chat-error-details-copied', 'chat-error-details-restored'):
            self.assertIn(('capture', state), device.calls)
        probe = next(call for call in device.calls if call[:3] == ('adb', 'shell', 'am'))
        self.assertIn('ai.zara.app.ui.ChatErrorClipboardInstrumentedTest', probe)
        self.assertEqual(90, device.probe_timeout)

    def test_instrumentation_failure_is_not_success(self):
        device = FakeDevice(self.stored, probe='FAILURES!!!\nTests run: 1, Failures: 1')
        with patch.object(self.module, 'open_menu', side_effect=lambda d, menu: d.tap(menu)):
            with self.assertRaisesRegex(AssertionError, 'clipboard'):
                self.module.exercise_chat_error(device, lambda *_: None)


class FakeDevice:
    def __init__(self, stored, probe='OK (1 test)'):
        self.calls = []
        self.stored = stored
        self.probe = probe

    def adb(self, *args, **kwargs):
        self.calls.append(('adb', *args))
        if 'instrument' in args:
            self.probe_timeout = kwargs.get('timeout')
        return self.stored if kwargs.get('binary') else self.probe

    def __getattr__(self, method):
        return lambda *args, **kwargs: self.calls.append((method, *args))


if __name__ == '__main__':
    unittest.main()
