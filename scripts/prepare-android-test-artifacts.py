#!/usr/bin/env python3
"""Package explicitly unverified debug APKs without changing acceptance gates."""
from pathlib import Path
import argparse
import hashlib
import json
import re
import shutil
import subprocess

APKS = (
    ('android/app/build/outputs/apk/debug/app-debug.apk', 'zara-debug.apk'),
    ('android/llm-serve/build/outputs/apk/debug/llm-serve-debug.apk', 'zara-llm-serve-debug.apk'),
)


def prepare_bundle(root: Path, output: Path, source_sha: str, status: str) -> None:
    if not re.fullmatch(r'[0-9a-f]{40}', source_sha):
        raise ValueError('An immutable source SHA is required')
    if status not in ('success', 'failure', 'cancelled', 'skipped'):
        raise ValueError('An explicit acceptance outcome is required')
    sources = [(root / source, name) for source, name in APKS]
    if any(not path.is_file() for path, _ in sources):
        raise FileNotFoundError('Both built Android APKs are required')
    output.mkdir(parents=True, exist_ok=True)
    files = []
    for source, name in sources:
        target = output / name
        shutil.copyfile(source, target)
        files.append({'file': name, 'size': target.stat().st_size,
                      'sha256': hashlib.sha256(target.read_bytes()).hexdigest()})
    (output / 'manifest.json').write_text(json.dumps({
        'source_sha': source_sha, 'channel': 'debug-test-unverified',
        'acceptance_status': status, 'real_inference_verified': False,
        'offline_audio_verified': False, 'files': files,
    }, indent=2) + '\n')
    (output / 'INSTALL.txt').write_text(
        'Debug/test APKs — NOT a validated release.\n'
        f'Source: {source_sha}\nInstalled acceptance outcome: {status}\n'
        'Real model inference and offline audio are unverified by this bundle.\n'
        'See manifest.json for sizes/checksums and the CI UI evidence for failures.\n'
        'Install both APKs with the Android package installer, or adb install -r.\n'
        'Packages: ai.zara.app and ai.zara.llmserve. Do not uninstall existing data\n'
        'to bypass a signing mismatch. Open Settings > Runtime & local AI in Zara\n'
        'for provider/model setup and offline voice readiness. Import a compatible\n'
        '.litertlm model in Zara before starting the LLM Serve companion.\n'
    )


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--source-sha', required=True)
    parser.add_argument('--acceptance-status', required=True)
    parser.add_argument('--output', type=Path, required=True)
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[1]
    actual = subprocess.check_output(['git', 'rev-parse', 'HEAD'], cwd=root, text=True).strip()
    if actual != args.source_sha:
        raise SystemExit('Source SHA does not match the checked-out repository')
    prepare_bundle(root, args.output, args.source_sha, args.acceptance_status)
