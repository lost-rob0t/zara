#!/usr/bin/env bash
# Read-only branch audit for ZARA-020.
#
# Lists every local and remote ref, ahead/behind counts vs. the current HEAD,
# unique commit subjects, and the dirty-tree status. The script never writes
# to refs, never fetches, and never modifies the working tree.
#
# Run with: nix develop -c scripts/audit-branches.sh
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$repo_root"

current_branch=$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo "(detached)")
head_sha=$(git rev-parse --short HEAD 2>/dev/null || echo "unknown")

echo "=== Branch Audit ==="
echo "Current branch: $current_branch"
echo "HEAD: $head_sha"
echo "Dirty tree: $(if [ -n "$(git status --porcelain 2>/dev/null)" ]; then echo "yes"; else echo "no"; fi)"
echo ""

print_ref() {
  local ref="$1"
  local label="$2"
  local short
  short=$(echo "$ref" | sed 's#^refs/heads/##; s#^refs/remotes/##')
  local ahead behind merged
  ahead=$(git rev-list --count HEAD.."$ref" 2>/dev/null || echo "?")
  behind=$(git rev-list --count "$ref"..HEAD 2>/dev/null || echo "?")
  if git merge-base --is-ancestor "$ref" HEAD 2>/dev/null; then
    merged="yes"
  else
    merged="no"
  fi
  printf '%-50s  ahead=%-4s  behind=%-4s  merged=%-3s  %s\n' \
    "$label/$short" "$ahead" "$behind" "$merged" \
    "$(git log -1 --format='%h %s' "$ref" 2>/dev/null || echo 'unreadable')"

  if [ "$merged" = "no" ] && [ "$ahead" != "0" ] && [ "$ahead" != "?" ]; then
    git log HEAD.."$ref" --format='    %h %s' 2>/dev/null | head -10
  fi
}

echo "--- Local branches ---"
git for-each-ref --format='%(refname)' refs/heads/ | while read -r ref; do
  print_ref "$ref" "local"
done

echo ""
echo "--- Remote branches ---"
git for-each-ref --format='%(refname)' refs/remotes/ | grep -v '/HEAD$' | while read -r ref; do
  print_ref "$ref" "remote"
done

echo ""
echo "=== Summary ==="
total_local=$(git for-each-ref refs/heads/ | wc -l)
total_remote=$(git for-each-ref refs/remotes/ | grep -v 'HEAD$' | wc -l)
echo "Local branches:  $total_local"
echo "Remote branches: $total_remote"
