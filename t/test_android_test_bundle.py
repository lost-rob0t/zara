from pathlib import Path
import importlib.util
import hashlib
import json
import tempfile
import unittest

ROOT = Path(__file__).resolve().parents[1]

class AndroidTestBundleTest(unittest.TestCase):
    def test_failed_acceptance_keeps_debug_downloads_explicitly_unverified(self):
        spec = importlib.util.spec_from_file_location('bundle', ROOT / 'scripts/prepare-android-test-artifacts.py')
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            for source, _ in module.APKS:
                path = root / source
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_bytes(source.encode())
            output = root / 'bundle'
            module.prepare_bundle(root, output, 'a' * 40, 'failure')
            manifest = json.loads((output / 'manifest.json').read_text())
            self.assertEqual('failure', manifest['acceptance_status'])
            self.assertEqual('debug-test-unverified', manifest['channel'])
            self.assertEqual('a' * 40, manifest['source_sha'])
            self.assertFalse(manifest['real_inference_verified'])
            self.assertFalse(manifest['offline_audio_verified'])
            for row in manifest['files']:
                payload = (output / row['file']).read_bytes()
                self.assertEqual(len(payload), row['size'])
                self.assertEqual(hashlib.sha256(payload).hexdigest(), row['sha256'])
            self.assertIn('NOT a validated release', (output / 'INSTALL.txt').read_text())

    def test_invalid_provenance_or_missing_inputs_cannot_create_bundle(self):
        spec = importlib.util.spec_from_file_location('bundle', ROOT / 'scripts/prepare-android-test-artifacts.py')
        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            output = root / 'bundle'
            for sha,status,error in [('master','failure',ValueError),('a'*40,'pending',ValueError),('a'*40,'success',FileNotFoundError)]:
                with self.subTest(sha=sha,status=status), self.assertRaises(error):
                    module.prepare_bundle(root,output,sha,status)
                self.assertFalse(output.exists())

    def test_ci_upload_survives_acceptance_failure_but_requires_successful_build(self):
        workflow = (ROOT / '.github/workflows/ci.yml').read_text()
        step = workflow.split('      - name: Upload unverified Android test APKs\n')[1].split('\n      - name:')[0]
        self.assertIn("always() && steps.android_build.outcome == 'success'", step)
        self.assertIn('android-test-unverified-${{ github.event.pull_request.head.sha || github.sha }}', step)
        self.assertIn('steps.android_acceptance.outcome', workflow)
