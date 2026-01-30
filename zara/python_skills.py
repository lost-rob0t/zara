"""
Python skills registry for Zara.

These skills are invoked when Prolog resolves an intent to python(Name).
"""

import os
from pathlib import Path
from typing import Any, Callable, Dict, Iterable, List, Optional, Sequence

# orgparse is added via nix; local tooling may not resolve it.
from orgparse import load as org_load  # type: ignore[import-not-found]
from .config import get_config
from .noaa import get_noaa_weather


def say_hello(args: List[Any]) -> str:
    name = args[0] if args else "there"
    return f"Hello, {name}!"


def noaa_weather(args: List[Any]) -> str:
    return get_noaa_weather()



def capture_todo(args: List[Any]) -> str:
    text = " ".join(str(arg) for arg in args if arg is not None).strip()
    if not text:
        return "No todo content provided."

    config = get_config()
    todo_config = config.get_section("todo")
    path = todo_config.get("path", "~/todo.org")
    format_name = str(todo_config.get("format", "org")).lower()

    if format_name not in {"org", "markdown"}:
        return f"Unsupported todo format: {format_name}."

    if format_name == "markdown":
        entry = f"- [ ] {text}\n"
    else:
        entry = f"* TODO {text}\n"

    output_path = Path(os.path.expanduser(os.path.expandvars(str(path))))
    output_path.parent.mkdir(parents=True, exist_ok=True)
    with output_path.open("a", encoding="utf-8") as handle:
        handle.write(entry)

    return f"Todo captured to {output_path}."


def list_todos(args: List[Any]) -> str:
    config = get_config()
    todo_config = config.get_section("todo")
    path = todo_config.get("path", "~/todo.org")
    format_name = str(todo_config.get("format", "org")).lower()
    statuses = _normalize_statuses(args)

    if format_name not in {"org", "markdown"}:
        return f"Unsupported todo format: {format_name}."

    output_path = Path(os.path.expanduser(os.path.expandvars(str(path))))
    if not output_path.exists():
        return f"Todo file not found: {output_path}."

    if format_name == "markdown":
        return _list_markdown_todos(output_path, statuses)

    return _list_org_todos(output_path, statuses)




def _normalize_statuses(args: Sequence[Any]) -> Optional[Iterable[str]]:
    if not args:
        return None
    if len(args) == 1 and isinstance(args[0], str) and "," in args[0]:
        args = [status.strip() for status in args[0].split(",") if status.strip()]
    ignore = {"LIST", "TASKS", "TODOS", "SHOW"}
    statuses = [
        str(status).strip().upper()
        for status in args
        if str(status).strip() and str(status).strip().upper() not in ignore
    ]
    return statuses or None


def _list_markdown_todos(path: Path, statuses: Optional[Iterable[str]]) -> str:
    lines = path.read_text(encoding="utf-8").splitlines()
    results = []
    for line in lines:
        stripped = line.strip()
        if not stripped.startswith("-"):
            continue
        if stripped.startswith("- [x]") or stripped.startswith("- [X]"):
            status = "DONE"
        elif stripped.startswith("- [ ]"):
            status = "TODO"
        else:
            continue
        if statuses and status not in statuses:
            continue
        text = stripped.split("]", 1)[-1].strip()
        results.append(f"- [{status}] {text}")
    if not results:
        return "No todos found."
    return "\n".join(results) + "\n"


def _list_org_todos(path: Path, statuses: Optional[Iterable[str]]) -> str:
    if org_load is None:
        return "Org parser unavailable."
    root = org_load(str(path))
    nodes = list(root[1:])
    results = []
    for node in nodes:
        status = node.todo
        if not status:
            continue
        status = status.upper()
        if statuses and status not in statuses:
            continue
        heading = node.heading.strip() if node.heading else ""
        if not heading:
            continue
        indent = "  " * max(node.level - 1, 0)
        line = _format_org_line(indent, status, heading, node)
        results.append(line)
    if not results:
        return "No todos found."
    return "\n".join(results) + "\n"


def _format_org_line(indent: str, status: str, heading: str, node) -> str:
    tags = _format_org_tags(getattr(node, "tags", None))
    priority = _format_org_priority(getattr(node, "priority", None))
    scheduled = _format_org_timestamp(getattr(node, "scheduled", None), "SCHEDULED")
    deadline = _format_org_timestamp(getattr(node, "deadline", None), "DEADLINE")
    return f"{indent}- [{status}] {heading}{tags}{priority}{scheduled}{deadline}"


def _format_org_tags(tags: Optional[Iterable[str]]) -> str:
    if not tags:
        return ""
    tag_list = [tag for tag in tags if tag]
    if not tag_list:
        return ""
    return f" :{':'.join(tag_list)}:"


def _format_org_priority(priority: Optional[object]) -> str:
    if priority is None:
        return ""
    if isinstance(priority, int):
        if 65 <= priority <= 90:
            value = chr(priority)
        else:
            value = str(priority)
    else:
        value = str(priority).strip()
    if not value:
        return ""
    return f" [#{value}]"


def _format_org_timestamp(value: Optional[object], label: str) -> str:
    if not value:
        return ""
    text = str(value)
    if "<" not in text:
        text = f"<{text}>"
    return f" {label}: {text}"


class PythonSkillRegistry:
    def __init__(self) -> None:
        self._skills: Dict[str, Callable[[List[Any]], str]] = {
            "say_hello": say_hello,
            "noaa_weather": noaa_weather,
            "capture_todo": capture_todo,
            "list_todos": list_todos,
        }

    def register(self, name: str, func: Callable[[List[Any]], str]) -> None:
        self._skills[name] = func

    def execute(self, skill_name: str, args: List[Any]) -> str:
        func = self._skills.get(skill_name)
        if func is None:
            raise NotImplementedError(f"Python skill '{skill_name}' is not implemented")
        return func(args)

    def list_skills(self) -> List[str]:
        return sorted(self._skills.keys())


python_skills = PythonSkillRegistry()
