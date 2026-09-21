#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "$0")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

swipl -q -g run_tests -t halt "$repo_root/t/expert_contract.pl"

swipl -q -g "use_module('$repo_root/modules/expert_contract'), expert_contract:emit_expert_corpus(user_output), halt." 2>/dev/null | grep '^expert_' > "$test_root/experts.actual"
diff -u "$repo_root/t/fixtures/experts.expected" "$test_root/experts.actual"

python3 - "$repo_root" <<'PY'
import csv
import pathlib
import sys

repo = pathlib.Path(sys.argv[1])
with (repo / "contracts/zara-expert-v1/descriptors.tsv").open() as handle:
    tsv_ids = {row["expert_id"] for row in csv.DictReader(handle, delimiter="\t")}
expected_ids = set()
for line in (repo / "t/fixtures/experts.expected").read_text().splitlines():
    if line.startswith("expert_descriptor("):
        body = line[len("expert_descriptor(") : -2]
        expected_ids.add(body.split(",", 1)[0].strip("'\""))
missing = tsv_ids - expected_ids
extra = expected_ids - tsv_ids
if missing or extra:
    sys.exit(f"expert corpus/TSV parity failure: missing={sorted(missing)} extra={sorted(extra)}")
print(f"expert corpus parity: {len(tsv_ids)} experts match descriptors.tsv")
PY
