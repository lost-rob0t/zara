"""Explicit manual Android OpenRouter probe; never invoked by recurring/PR CI."""
from __future__ import annotations

import argparse
from decimal import Decimal, InvalidOperation
import json
import os
from pathlib import Path
import re
import subprocess
import urllib.request


def amount(value):
    try:
        result = Decimal(str(value))
    except InvalidOperation as error:
        raise ValueError('Invalid spending ceiling') from error
    if not result.is_finite() or result <= 0:
        raise ValueError('Spending ceiling must be finite and positive')
    return result


def configuration(environ):
    key = environ.get('ZARA_OPENROUTER_API_KEY', '').strip()
    model = environ.get('ZARA_OPENROUTER_MODEL', '').strip()
    budget = amount(environ.get('ZARA_OPENROUTER_BUDGET_USD', ''))
    if not key or len(key) > 4096 or any(c in key for c in '\r\n'):
        raise ValueError('Dedicated OpenRouter key required')
    if not model or len(model) > 256 or any(ord(c) < 32 for c in model):
        raise ValueError('Explicit OpenRouter model required')
    if budget > Decimal('0.05'):
        raise ValueError('Manual verification ceiling is at most USD 0.05')
    return {'key': key, 'model': model, 'budget_usd': str(budget)}


def verify_key_limit(data, budget):
    limit = amount(data.get('limit'))
    remaining = amount(data.get('limit_remaining'))
    if limit > amount(budget) or remaining > limit:
        raise ValueError('Dedicated key cap exceeds approved ceiling')
    if 'limit_reset' not in data or data['limit_reset'] is not None:
        raise ValueError('Key cap must not reset')
    if data.get('include_byok_in_limit') is not True:
        raise ValueError('Key cap must include BYOK spend')


class NoRedirect(urllib.request.HTTPRedirectHandler):
    def redirect_request(self, req, fp, code, msg, headers, newurl):
        raise ValueError('OpenRouter metadata redirect refused')


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--manual', action='store_true', required=True)
    parser.add_argument('--serial', required=True)
    parser.add_argument('--test-apk', type=Path, required=True)
    parser.add_argument('--source-sha', required=True)
    parser.add_argument('--receipt', type=Path, required=True)
    args = parser.parse_args()
    config = configuration(os.environ)
    if os.environ.get('GITHUB_ACTIONS') == 'true' and os.environ.get('GITHUB_EVENT_NAME') not in {'workflow_dispatch', 'release'}:
        raise ValueError('Paid probe forbidden in automatic CI')
    if not re.fullmatch(r'emulator-\d+', args.serial):
        raise ValueError('Explicit emulator serial required')
    if not re.fullmatch(r'[0-9a-f]{40}', args.source_sha) or not args.test_apk.is_file():
        raise ValueError('Exact source SHA and built instrumentation APK required')

    def adb(*arguments, input=None, timeout=30):
        result = subprocess.run(['adb', '-s', args.serial, *arguments], input=input,
                                stdout=subprocess.PIPE, stderr=subprocess.PIPE, timeout=timeout)
        if result.returncode:
            raise RuntimeError('ADB operation failed; paid acceptance is not proven')
        return result.stdout

    if adb('get-state').strip() != b'device':
        raise ValueError('Selected emulator is unavailable')
    request = urllib.request.Request('https://openrouter.ai/api/v1/key', headers={'Authorization': 'Bearer ' + config['key']})
    with urllib.request.build_opener(NoRedirect).open(request, timeout=15) as response:
        metadata = json.loads(response.read(65537))
    verify_key_limit(metadata['data'], config['budget_usd'])
    config['source_sha'] = args.source_sha
    adb('install', '-r', '-t', str(args.test_apk), timeout=120)
    try:
        adb('shell', "run-as ai.zara.app sh -c 'umask 077; cat > cache/openrouter-live.json'",
            input=json.dumps(config).encode())
        output = adb('shell', 'am', 'instrument', '-w', '-r', '-e', 'class',
                     'ai.zara.app.model.OpenRouterLiveAcceptanceTest',
                     'ai.zara.app.test/androidx.test.runner.AndroidJUnitRunner', timeout=180)
        if b'OK (1 test)' not in output or b'FAILURES' in output or b'INSTRUMENTATION_FAILED' in output:
            raise RuntimeError('Installed live test failed; no retry or promotion')
        receipt = json.loads(adb('shell', 'run-as', 'ai.zara.app', 'cat', 'cache/openrouter-live-receipt.json'))
        if receipt.get('source_sha') != args.source_sha or receipt.get('passed') is not True:
            raise RuntimeError('Live receipt provenance mismatch')
        args.receipt.write_text(json.dumps(receipt, indent=2) + '\n')
        print('Installed direct-cloud live probe passed; receipt:', args.receipt)
    finally:
        adb('shell', 'run-as', 'ai.zara.app', 'rm', '-f', 'cache/openrouter-live.json', 'cache/openrouter-live-receipt.json')


if __name__ == '__main__':
    try:
        main()
    except Exception as error:
        raise SystemExit('BLOCKED/FAILED: manual OpenRouter acceptance (' + type(error).__name__ + '); no readiness claim')
