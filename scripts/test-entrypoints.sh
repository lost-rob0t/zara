#!/usr/bin/env bash
set -euo pipefail

python -m compileall -q zara scripts
python -c 'import zara.wake; import zara.desktop.app; assert callable(zara.desktop.app.main)'
python -m zara --help | grep -q -- "--desktop"

# This script runs inside `nix develop`, so these assertions cover the actual
# development surface rather than a host-global install.
command -v zara >/dev/null
command -v zara-desktop >/dev/null

system="$(nix eval --impure --raw --expr builtins.currentSystem)"
nix eval --raw ".#apps.${system}.zara-desktop.program" >/dev/null
nix build .#zara-desktop --no-link
