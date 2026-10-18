#!/usr/bin/env bash
set -euo pipefail
root="$(cd "$(dirname "$0")/.." && pwd)"
cd "$root"
: "${GF_RGL_ROOT:?Set GF_RGL_ROOT to the pinned gf-rgl checkout or use nix/gf-pilot.nix}"
for tool in gf cc pkg-config python3 swipl; do
    command -v "$tool" >/dev/null || { echo "Missing required tool: $tool" >&2; exit 1; }
done
mkdir -p .gf-build/gfo
build="$root/.gf-build"
rm -f "$build/Zara.pgf" "$build/pgf-probe" "$build/provenance.json"
gf --version | tee "$build/compiler.log"
grep -q 'version 3.12.0' "$build/compiler.log" || { echo 'GF 3.12.0 required' >&2; exit 1; }
rgl_revision="${GF_RGL_REV:-$(git -C "$GF_RGL_ROOT" rev-parse HEAD)}"
[[ "$rgl_revision" == caaa5ab8716b74bb7c043ec5efc5ae37c025db9e ]] || { echo 'Unexpected RGL revision' >&2; exit 1; }
search="$root/nlp/gf"
for directory in api english abstract common prelude alltenses; do
    search="$search:$GF_RGL_ROOT/src/$directory"
done
gf -make -path="$search" -gfo-dir="$build/gfo" -output-dir="$build" \
    nlp/gf/ZaraEng.gf nlp/gf/ZaraCanonical.gf 2>&1 | tee "$build/grammar.log"
[[ -s "$build/Zara.pgf" ]]
# Upstream pkg-config supplies both include paths and the matching C runtime.
read -ra cflags <<< "$(pkg-config --cflags libpgf)"
read -ra libs <<< "$(pkg-config --libs libpgf libgu)"
cc -std=c11 -Wall -Wextra -Werror -O2 "${cflags[@]}" \
    nlp/gf/pgf_probe.c "${libs[@]}" -o "$build/pgf-probe" 2>&1 | tee "$build/native.log"
python3 -m unittest discover -s nlp/gf -p 'test_*.py' -v 2>&1 | tee "$build/corpus.log"
swipl -q -s nlp/gf/test_frontend.pl -g 'run_tests,halt' -t 'halt(1)' 2>&1 | tee "$build/prolog.log"
python3 scripts/gf-demo.py \
    'Hey Zara, who are you?' 'Set a timer.' '15 minutes' \
    'Actually 5 minutes' 'Cancel that.' 2>&1 | tee "$build/demo.log"
python3 - <<'PY'
import hashlib, json, pathlib, subprocess
root = pathlib.Path.cwd()
files = sorted(root.joinpath('nlp/gf').glob('*.gf')) + [root / '.gf-build/Zara.pgf']
report = {
    'source_sha': subprocess.check_output(['git', 'rev-parse', 'HEAD'], text=True).strip(),
    'gf': subprocess.check_output(['gf', '--version'], text=True).strip(),
    'gf_binary_distribution_sha256': '17aa5452b713f1e00a0755a1bad998a926acbffb96eefde2c3800ca72536b4d8',
    'rgl_revision': 'caaa5ab8716b74bb7c043ec5efc5ae37c025db9e',
    'sha256': {str(p.relative_to(root)): hashlib.sha256(p.read_bytes()).hexdigest() for p in files},
    'pgf_bytes': (root / '.gf-build/Zara.pgf').stat().st_size,
    'model_calls': 0, 'provider_calls': 0,
    'android_device_validated': False,
}
(root / '.gf-build/provenance.json').write_text(json.dumps(report, indent=2) + '\n')
print(json.dumps(report, indent=2))
PY
