"""End-to-end GF -> canonical Prolog owner -> GF response tests."""
import json
from pathlib import Path
import subprocess
import sys
import unittest

ROOT = Path(__file__).resolve().parents[2]


class DialogueDemoTest(unittest.TestCase):
    def run_demo(self, *utterances):
        return subprocess.run(
            [sys.executable, str(ROOT / 'scripts/gf-demo.py'), '--json', *utterances],
            capture_output=True, text=True, timeout=30, check=False,
        )

    def test_addressed_help_is_human_readable(self):
        result = self.run_demo('Hey Zara, please help me!')
        self.assertEqual(result.returncode, 0, result.stderr)
        turn = json.loads(result.stdout)[0]
        self.assertEqual(turn['canonical'], 'help')
        self.assertEqual(turn['reply_tree'], 'HelpReply')
        self.assertEqual(turn['model_calls'], 0)
        self.assertEqual(turn['provider_calls'], 0)
        self.assertFalse(turn['executed'])

    def test_multiturn_clarification_and_correction(self):
        result = self.run_demo('Set a timer.', '15 minutes', 'Actually 5 minutes', 'Cancel that.')
        self.assertEqual(result.returncode, 0, result.stderr)
        turns = json.loads(result.stdout)
        self.assertEqual(turns[0]['reply'], 'How long should the timer run?')
        self.assertIn('duration(900)', turns[1]['semantic_turn'])
        self.assertIn('origin(follow_up)', turns[1]['semantic_turn'])
        self.assertIn('duration(300)', turns[2]['semantic_turn'])
        self.assertIn('origin(correction)', turns[2]['semantic_turn'])
        self.assertEqual(turns[-1]['reply_tree'], 'CancelledReply')
        self.assertTrue(all(not turn['executed'] for turn in turns))

    def test_unsupported_turn_preserves_pending_context(self):
        result = self.run_demo('Set a timer', 'something completely unknown', '2 minutes')
        self.assertEqual(result.returncode, 0, result.stderr)
        turns = json.loads(result.stdout)
        self.assertEqual(turns[1]['status'], 'no_match')
        self.assertIn('origin(follow_up)', turns[2]['semantic_turn'])
        self.assertIn('duration(120)', turns[2]['semantic_turn'])

    def test_no_false_success_for_opening_an_app(self):
        result = self.run_demo('Please open Termux.')
        self.assertEqual(result.returncode, 0, result.stderr)
        turn = json.loads(result.stdout)[0]
        self.assertIn('dispatch_required', turn['semantic_turn'])
        self.assertEqual(turn['reply_tree'], 'PendingReply')
        self.assertFalse(turn['executed'])

    def test_negation_is_never_normalized_away(self):
        for text in ["Don't open settings!", 'Do not open settings.', 'Why did you open settings?']:
            with self.subTest(text=text):
                result = self.run_demo(text)
                self.assertEqual(result.returncode, 0, result.stderr)
                turn = json.loads(result.stdout)[0]
                self.assertEqual(turn['status'], 'no_match')
                self.assertIsNone(turn['canonical'])
                self.assertFalse(turn['executed'])

    def test_oversized_input_is_an_error_not_a_reply(self):
        result = self.run_demo('a' * 513)
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, '')

    def test_control_characters_are_rejected(self):
        result = self.run_demo('open\nsettings')
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, '')

    def test_bounded_turn_count(self):
        result = self.run_demo(*(['hello'] * 17))
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, '')


if __name__ == '__main__':
    unittest.main()
