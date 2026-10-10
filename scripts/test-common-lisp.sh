#!/usr/bin/env bash
set -euo pipefail

root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$root"

# Resolve the Lisp wrapper from this checkout's locked nixpkgs input.  Keep
# dependency selection in lisp/deps.nix so both normal and packaged-daemon
# FiveAM passes consume the same immutable closure.
lisp_expr='
let
  flake = builtins.getFlake (toString ./.);
  pkgs = import flake.inputs.nixpkgs { system = builtins.currentSystem; };
in
import ./lisp/deps.nix { inherit pkgs; }
'

exec nix shell --impure --expr "$lisp_expr" --command \
  sbcl --non-interactive \
    --eval '(load (sb-ext:posix-getenv "ASDF"))' \
    --eval '(asdf:load-asd (truename "lisp/zara.asd"))' \
    --eval '(asdf:test-system :zara)'
