#!/usr/bin/env bash
set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
test_root="$(mktemp -d)"
trap 'rm -rf "$test_root"' EXIT

export HOME="$test_root/home"
export XDG_CONFIG_HOME="$test_root/config"
export PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"

pytest -q "$repo_root/t/test_todos.py"
python -c '
from zara.python_skills import python_skills

captured = python_skills.execute("capture_todo", ["public skill todo"])
todo_id = captured.removeprefix("Todo captured (id ").removesuffix(").")
assert python_skills.execute("edit_todo", [todo_id, "edited public todo"]) == "Todo updated."
assert python_skills.execute("schedule_todo", [todo_id, "2026-08-01 10:00"]) == "Todo scheduled."
assert "edited public todo" in python_skills.execute("list_todos", [])
assert python_skills.execute("complete_todo", [todo_id]) == "Todo completed."
assert python_skills.execute("reopen_todo", [todo_id]) == "Todo reopened."
assert "edited public todo" in python_skills.execute("search_todos", ["edited"])
'
