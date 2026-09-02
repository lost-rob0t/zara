#!/usr/bin/env bash
set -euo pipefail

source_root="${1:?Trealla source root is required}"
parser="$source_root/src/parser.c"

test -f "$parser"

python3 - "$parser" <<'PY'
from pathlib import Path
import sys

path = Path(sys.argv[1])
source = path.read_text()
needle = """\t\t\tif (tmp_m != p->m)\n\t\t\t\tp->m->used[p->m->idx_used++] = tmp_m;\n\n\t\t\tp->m = tmp_m;\n"""
replacement = """\t\t\t/* Preserve the consulted source path when a module declaration\n\t\t\t * swaps parsers to the newly-created module. Relative use_module/1\n\t\t\t * directives must resolve from the source file, not the module atom. */\n\t\t\ttmp_m->filename = save_m->filename;\n\t\t\ttmp_m->actual_filename = save_m->actual_filename\n\t\t\t\t? save_m->actual_filename : save_m->filename;\n\n\t\t\tif (tmp_m != p->m)\n\t\t\t\tp->m->used[p->m->idx_used++] = tmp_m;\n\n\t\t\tp->m = tmp_m;\n"""

count = source.count(needle)
if count != 1:
    raise SystemExit(f"Trealla module-path patch expected one pinned source match, found {count}")

path.write_text(source.replace(needle, replacement, 1))
PY
