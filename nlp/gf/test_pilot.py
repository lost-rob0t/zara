"""Real PGF tests. Missing compiler/runtime is a failure, never a skip."""
import json
import os
from pathlib import Path
import subprocess
import unittest

ROOT = Path(__file__).resolve().parents[2]
CORPUS = json.loads(Path(__file__).with_name("corpus.json").read_text())


class GfPilotTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.binary = Path(os.environ.get("ZARA_GF_PROBE", ROOT / ".gf-build/pgf-probe"))
        cls.grammar = ROOT / ".gf-build/Zara.pgf"
        if not cls.binary.is_file() or not cls.grammar.is_file():
            raise AssertionError("Real compiled PGF and C runtime probe are required; run scripts/test-gf.sh")

    def run_probe(self, mode, text):
        return subprocess.run(
            [str(self.binary), mode, str(self.grammar), text],
            capture_output=True, text=True, timeout=5, check=False,
        )

    def test_parse_corpus_through_native_pgf(self):
        for utterance, canonical in CORPUS["accepted"]:
            with self.subTest(utterance=utterance):
                result = self.run_probe("parse", utterance)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(set(result.stdout.splitlines()), {canonical})

    def test_negation_and_unknown_input_never_become_positive_commands(self):
        for utterance in CORPUS["rejected"]:
            with self.subTest(utterance=utterance):
                result = self.run_probe("parse", utterance)
                self.assertEqual(result.returncode, 2, result.stderr)
                self.assertEqual(result.stdout, "")

    def test_generation_from_explicit_response_trees(self):
        for tree, expected in CORPUS["replies"]:
            with self.subTest(tree=tree):
                result = self.run_probe("render", tree)
                self.assertEqual(result.returncode, 0, result.stderr)
                self.assertEqual(result.stdout.strip(), expected)

    def test_identity_is_not_swallowed_by_greeting(self):
        result = self.run_probe("parse", "hey zara who are you")
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(result.stdout.strip(), "who are you")

    def test_oversized_input_is_rejected_before_parsing(self):
        result = self.run_probe("parse", "a" * 513)
        self.assertEqual(result.returncode, 3)
        self.assertEqual(result.stdout, "")

    def test_empty_input_is_not_a_success(self):
        result = self.run_probe("parse", "")
        self.assertEqual(result.returncode, 2)
        self.assertEqual(result.stdout, "")

    def test_unknown_response_constructor_is_not_executed(self):
        result = self.run_probe("render", "UnknownReply")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")

    def test_command_constructor_cannot_be_rendered_as_reply(self):
        result = self.run_probe("render", "Greet")
        self.assertNotEqual(result.returncode, 0)
        self.assertEqual(result.stdout, "")


if __name__ == "__main__":
    unittest.main()
