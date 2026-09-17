"""Run native Prolog/policy plunit tests as part of the existing pytest gate."""
from pathlib import Path
import shutil
import subprocess
import unittest


@unittest.skipUnless(shutil.which("swipl"), "SWI-Prolog is provided by nix develop")
class PrologPolicyIntegrationTest(unittest.TestCase):
    def test_native_mode_and_policy_plunit(self):
        root = Path(__file__).parents[1]
        result = subprocess.run(
            ["swipl", "-q", "-s", "t/prolog_mode.pl", "-s", "t/output_policy.pl",
             "-g", "(run_tests -> halt(0) ; halt(1))", "-t", "halt(2)"],
            cwd=root, capture_output=True, text=True, timeout=20,
        )
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertNotIn("ERROR:", result.stderr)
