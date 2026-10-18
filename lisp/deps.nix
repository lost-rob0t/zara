{ pkgs }:

# This package set is resolved from Zara's flake.lock by
# scripts/test-common-lisp.sh.  Nixpkgs' Lisp importer pins each Quicklisp
# archive by URL + hash, so an unchanged Zara SHA cannot silently resolve a
# newer live Quicklisp distribution.
pkgs.sbcl.withPackages (ps: with ps; [
  pzmq
  com_dot_inuoe_dot_jzon
  babel
  bordeaux-threads
  fiveam
])
