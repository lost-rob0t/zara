"""Response grammar must realize validated quantities, not claim execution."""
import os
from pathlib import Path
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[2]


class CompositionalReplyTest(unittest.TestCase):
    def test_quantities_are_generated_with_rgl_agreement(self):
        binary = str(os.environ.get('ZARA_GF_PROBE', ROOT / '.gf-build/pgf-probe'))
        examples = [
            ('TimerPendingReply (DurationOf (IDig D_1) Minutes)',
             'I understood a timer for 1 minute. It has not been started.'),
            ('TimerPendingReply (DurationOf (IIDig D_1 (IDig D_5)) Minutes)',
             'I understood a timer for 15 minutes. It has not been started.'),
            ('TimerPendingReply (DurationOf (IDig D_2) Hours)',
             'I understood a timer for 2 hours. It has not been started.'),
        ]
        for tree, expected in examples:
            with self.subTest(tree=tree):
                result = subprocess.run(
                    [binary, 'render', str(ROOT / '.gf-build/Zara.pgf'), tree],
                    capture_output=True, text=True, timeout=5, check=False,
                )
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout.strip(), expected)


if __name__ == '__main__':
    unittest.main()
