import importlib.util
from pathlib import Path
import unittest


class ManualOpenRouterGateTest(unittest.TestCase):
    def load(self):
        path = Path(__file__).resolve().parents[1] / 'android/integration/manual_openrouter_acceptance.py'
        spec = importlib.util.spec_from_file_location('manual_openrouter_gate', path)
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module

    def test_missing_config_blocks_before_any_network_or_device_action(self):
        module = self.load()
        with self.assertRaises(ValueError):
            module.configuration({})

    def test_budget_is_explicit_finite_and_at_most_five_cents(self):
        module = self.load()
        for amount in ('', '0', '-1', 'NaN', 'Infinity', '0.051'):
            with self.subTest(amount=amount), self.assertRaises(ValueError):
                module.configuration({'ZARA_OPENROUTER_API_KEY': 'fixture', 'ZARA_OPENROUTER_MODEL': 'fixture/model', 'ZARA_OPENROUTER_BUDGET_USD': amount})
        self.assertEqual('0.05', module.configuration({'ZARA_OPENROUTER_API_KEY': 'fixture', 'ZARA_OPENROUTER_MODEL': 'fixture/model', 'ZARA_OPENROUTER_BUDGET_USD': '0.05'})['budget_usd'])

    def test_uncapped_recurring_and_byok_excluded_keys_are_rejected(self):
        module = self.load()
        valid = {'limit': 0.02, 'limit_remaining': 0.02, 'limit_reset': None, 'include_byok_in_limit': True}
        module.verify_key_limit(valid, '0.05')
        for patch in ({'limit': None}, {'limit': 1}, {'limit_reset': 'daily'}, {'include_byok_in_limit': False}, {'limit_remaining': 0}, {'limit': float('nan')}):
            with self.subTest(patch=patch), self.assertRaises(ValueError):
                module.verify_key_limit(valid | patch, '0.05')

    def test_manual_test_is_never_in_automatic_emulator_selection(self):
        root = Path(__file__).resolve().parents[1]
        self.assertNotIn('OpenRouterLiveAcceptanceTest', (root / 'scripts/test-android-emulator-install.sh').read_text())
        for path in (root / '.github/workflows').glob('*.yml'):
            self.assertNotIn('manual_openrouter_acceptance.py', path.read_text())
