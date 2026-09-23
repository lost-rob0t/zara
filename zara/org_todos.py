"""Org-mode backed human todo storage.

Org files are the canonical record. SQLite TodoStore remains available as the
legacy backend and as a possible derived index, but mutations through this
store always round-trip the Org source.
"""

from __future__ import annotations

import calendar
import re
import tempfile
import uuid
from dataclasses import dataclass
from datetime import datetime, timedelta
from pathlib import Path
from typing import Any, Iterable, Mapping, Optional, Sequence, Union

from .todo_storage import CLEAR, PRESERVE, DEFAULT_DURATION_MINUTES, PatchMode, TodoRecord


ORG_STATUSES = (
    "TODO",
    "NEXT",
    "STRT",
    "WAIT",
    "WAITING",
    "HOLD",
    "IDEA",
    "LOOP",
    "SOMEDAY",
    "DONE",
    "KILL",
    "CANCELED",
    "CANCELLED",
)
TERMINAL_STATUSES = {"DONE", "KILL", "CANCELED", "CANCELLED"}
DEFAULT_TODO_DIRECTIVE = (
    "#+todo: TODO(t) STRT(s!) WAIT(w@/!) HOLD(h@/!) IDEA(i) LOOP(l!) "
    "| DONE(d!) KILL(k@/!)"
)
_ORG_REPEATER = re.compile(r"^(?:\+|\+\+|\.\+)([1-9][0-9]*)(min|h|d|w|m|y)$")
_HEADING = re.compile(r"^(?P<stars>\*+)\s+(?P<status>[A-Z][A-Z0-9_-]*)\s+(?P<rest>.+?)\s*$")
_TAG_SUFFIX = re.compile(r"\s+(?P<tags>:[A-Za-z0-9_@#%+.-]+(?::[A-Za-z0-9_@#%+.-]+)*:)\s*$")
_PROPERTY = re.compile(r"^\s*:(?P<key>[A-Za-z0-9_@#%+.-]+):(?P<spacing>\s*)(?P<value>.*?)\s*$")
_SCHEDULED = re.compile(r"^\s*SCHEDULED:\s*(?P<stamp><[^>]+>)\s*$")
_DEADLINE = re.compile(r"^\s*DEADLINE:\s*(?P<stamp><[^>]+>)\s*$")
_CLOSED = re.compile(r"^\s*CLOSED:\s*(?P<stamp>\[[^]]+\])\s*$")
_ORG_STAMP = re.compile(
    r"^[<[]"
    r"(?P<date>\d{4}-\d{2}-\d{2})"
    r"(?:\s+[A-Za-z]{3})?"
    r"(?:\s+(?P<time>\d{1,2}:\d{2}))?"
    r"(?:\s+(?P<repeater>(?:\+|\+\+|\.\+)[1-9][0-9]*(?:min|h|d|w|m|y)))?"
    r"[^>\]]*[>\]]$"
)

TodoId = Union[int, str]
PatchValue = Union[Any, PatchMode]


@dataclass(frozen=True)
class _OrgTask:
    path: Path
    level: int
    start: int
    end: int
    record: TodoRecord
    tags: tuple[str, ...]
    property_lines: tuple[str, ...]
    properties: Mapping[str, str]
    content_lines: tuple[str, ...]


class OrgTodoStore:
    """Read and mutate one Org file or an agenda directory."""

    def __init__(self, root: Union[str, Path], default_file: str = "inbox.org") -> None:
        self.root = Path(root).expanduser()
        self.default_file = default_file

    def add_todo(
        self,
        title: str,
        status: str,
        priority: Optional[str] = None,
        scheduled_at: Optional[str] = None,
        deadline_at: Optional[str] = None,
        repeater: Optional[str] = None,
        duration_minutes: Optional[int] = None,
        tags: Optional[Iterable[str]] = None,
        notes: Optional[str] = None,
    ) -> str:
        title = _validate_title(title)
        status = _validate_org_status(status)
        priority = _validate_priority(priority)
        scheduled_at = _validate_optional_timestamp("scheduled_at", scheduled_at)
        deadline_at = _validate_optional_timestamp("deadline_at", deadline_at)
        repeater = _validate_org_repeater(repeater)
        duration_minutes = _validate_optional_duration(duration_minutes)
        if repeater is not None and scheduled_at is None:
            raise ValueError("repeater requires scheduled_at")
        clean_tags = _clean_tags(tags or ())
        todo_id = str(uuid.uuid4())
        created_at = _now_iso()
        record = TodoRecord(
            id=todo_id,
            title=title,
            status=status,
            priority=priority,
            scheduled_at=scheduled_at,
            deadline_at=deadline_at,
            repeater=repeater,
            duration_minutes=duration_minutes,
            created_at=created_at,
            updated_at=created_at,
            completed_at=created_at if status in TERMINAL_STATUSES else None,
            notes=notes,
        )
        properties = [
            f":ID:       {todo_id}\n",
            f":CREATED:  {_format_inactive_timestamp(created_at)}\n",
        ]
        if duration_minutes is not None:
            properties.append(f":EFFORT:   {_format_effort(duration_minutes)}\n")
        block = _render_task_block(record, clean_tags, 1, properties, notes or "")
        target = self._default_path()
        self._ensure_document(target)
        current = target.read_text(encoding="utf-8")
        if current and not current.endswith("\n"):
            current += "\n"
        if current and not current.endswith("\n\n"):
            current += "\n"
        _atomic_write(target, current + block)
        return todo_id

    def update_todo(
        self,
        todo_id: TodoId,
        title: PatchValue = PRESERVE,
        status: PatchValue = PRESERVE,
        priority: PatchValue = PRESERVE,
        scheduled_at: PatchValue = PRESERVE,
        deadline_at: PatchValue = PRESERVE,
        repeater: PatchValue = PRESERVE,
        duration_minutes: PatchValue = PRESERVE,
        tags: PatchValue = PRESERVE,
        notes: PatchValue = PRESERVE,
    ) -> None:
        task = self._find_task(todo_id)
        if task is None:
            raise ValueError("Todo not found")
        current = task.record
        next_title = current.title if title is PRESERVE else _required_value("title", title)
        next_status = current.status if status is PRESERVE else _required_value("status", status)
        next_priority = _patched_optional(priority, current.priority)
        next_scheduled = _patched_optional(scheduled_at, current.scheduled_at)
        next_deadline = _patched_optional(deadline_at, current.deadline_at)
        next_repeater = _patched_optional(repeater, current.repeater)
        next_duration = _patched_optional(duration_minutes, current.duration_minutes)
        next_notes = _patched_optional(notes, current.notes)
        next_tags = list(task.tags) if tags is PRESERVE else _clean_tags(() if tags is CLEAR else tags)

        next_title = _validate_title(next_title)
        next_status = _validate_org_status(next_status)
        next_priority = _validate_priority(next_priority)
        next_scheduled = _validate_optional_timestamp("scheduled_at", next_scheduled)
        next_deadline = _validate_optional_timestamp("deadline_at", next_deadline)
        next_repeater = _validate_org_repeater(next_repeater)
        next_duration = _validate_optional_duration(next_duration)
        if next_repeater is not None and next_scheduled is None:
            raise ValueError("repeater requires scheduled_at")
        if next_notes is not None and not isinstance(next_notes, str):
            raise ValueError("notes must be a string or cleared")

        completed_at = current.completed_at
        if next_status in TERMINAL_STATUSES and completed_at is None:
            completed_at = _now_iso()
        elif next_status not in TERMINAL_STATUSES:
            completed_at = None
        updated_at = _now_iso()
        record = TodoRecord(
            id=current.id,
            title=next_title,
            status=next_status,
            priority=next_priority,
            scheduled_at=next_scheduled,
            deadline_at=next_deadline,
            repeater=next_repeater,
            duration_minutes=next_duration,
            created_at=current.created_at,
            updated_at=updated_at,
            completed_at=completed_at,
            notes=next_notes,
        )
        property_lines = _rewrite_property_lines(
            task.property_lines,
            str(record.id),
            record.created_at,
            record.duration_minutes,
        )
        block = _render_task_block(
            record,
            next_tags,
            task.level,
            property_lines,
            next_notes or "",
        )
        lines = task.path.read_text(encoding="utf-8").splitlines(keepends=True)
        lines[task.start : task.end] = block.splitlines(keepends=True)
        _atomic_write(task.path, "".join(lines))

    def get_todo(self, todo_id: TodoId) -> Optional[TodoRecord]:
        task = self._find_task(todo_id)
        return task.record if task is not None else None

    def list_todos(
        self,
        statuses: Optional[Iterable[str]] = None,
        tags: Optional[Iterable[str]] = None,
        include_done: bool = False,
    ) -> list[TodoRecord]:
        status_values = {str(value).upper() for value in statuses or ()}
        tag_values = set(_clean_tags(tags or ()))
        tasks = self._all_tasks()
        selected: list[TodoRecord] = []
        for task in tasks:
            if status_values and task.record.status not in status_values:
                continue
            if not include_done and task.record.status in TERMINAL_STATUSES:
                continue
            if tag_values and not tag_values.intersection(task.tags):
                continue
            selected.append(task.record)
        return selected

    def search_todos(self, query: str) -> list[TodoRecord]:
        needle = query.strip().casefold()
        if not needle:
            return []
        results = []
        for task in self._all_tasks():
            haystack = f"{task.record.title}\n{task.record.notes or ''}".casefold()
            if needle in haystack:
                results.append(task.record)
        return results

    def tags_for(self, todo_id: TodoId) -> list[str]:
        task = self._find_task(todo_id)
        if task is None:
            return []
        return sorted(task.tags)

    def list_occurrences(self, since: Optional[str] = None) -> list[tuple[TodoId, str]]:
        since_value = _validate_optional_timestamp("since", since)
        rows: list[tuple[TodoId, str]] = []
        for task in self._all_tasks():
            record = task.record
            if record.scheduled_at is None or record.status in TERMINAL_STATUSES:
                continue
            occurrences = _occurrences(record.scheduled_at, record.repeater)
            for occurrence in occurrences:
                if since_value is None or occurrence >= since_value:
                    rows.append((record.id, occurrence))
        return sorted(rows, key=lambda item: item[1])

    def schedule_conflicts(
        self,
        scheduled_at: str,
        duration_minutes: int,
        exclude_todo_id: Optional[TodoId] = None,
        default_duration_minutes: int = DEFAULT_DURATION_MINUTES,
    ) -> bool:
        proposed_start = _comparable_datetime(_validate_timestamp("scheduled_at", scheduled_at))
        duration = _validate_duration(duration_minutes)
        default_duration = _validate_duration(default_duration_minutes)
        proposed_end = proposed_start + timedelta(minutes=duration)
        excluded = str(exclude_todo_id) if exclude_todo_id is not None else None
        for task in self._all_tasks():
            record = task.record
            if record.status in TERMINAL_STATUSES or record.scheduled_at is None:
                continue
            if excluded is not None and _id_matches(excluded, str(record.id)):
                continue
            existing_duration = record.duration_minutes or default_duration
            for occurrence in _occurrences(record.scheduled_at, record.repeater):
                existing_start = _comparable_datetime(occurrence)
                existing_end = existing_start + timedelta(minutes=existing_duration)
                if proposed_start < existing_end and existing_start < proposed_end:
                    return True
        return False

    def _default_path(self) -> Path:
        if self.root.suffix.lower() == ".org":
            return self.root
        return self.root / self.default_file

    def _ensure_document(self, path: Path) -> None:
        if path.exists():
            return
        path.parent.mkdir(parents=True, exist_ok=True)
        title = path.stem.replace("-", " ").title() or "Todos"
        _atomic_write(path, f"#+title: {title}\n{DEFAULT_TODO_DIRECTIVE}\n\n")

    def _paths(self) -> list[Path]:
        if self.root.suffix.lower() == ".org":
            return [self.root] if self.root.exists() else []
        if not self.root.exists():
            return []
        return sorted(path for path in self.root.rglob("*.org") if path.is_file())

    def _all_tasks(self) -> list[_OrgTask]:
        tasks: list[_OrgTask] = []
        for path in self._paths():
            tasks.extend(_parse_file(path))
        return tasks

    def _find_task(self, todo_id: TodoId) -> Optional[_OrgTask]:
        needle = str(todo_id).strip().lstrip("#")
        exact: Optional[_OrgTask] = None
        prefix: list[_OrgTask] = []
        for task in self._all_tasks():
            candidate = str(task.record.id)
            if candidate == needle:
                exact = task
                break
            if candidate.startswith(needle):
                prefix.append(task)
        if exact is not None:
            return exact
        if len(prefix) == 1:
            return prefix[0]
        return None


def resolve_org_todo_root(
    todo_config: Mapping[str, Any],
    home: Optional[Path] = None,
) -> Path:
    """Resolve the configured canonical Org source without guessing user layout."""

    home_path = (home or Path.home()).expanduser()
    configured_root = str(todo_config.get("org_root", "")).strip()
    if configured_root:
        return _expand_home(configured_root, home_path)
    configured_repo = str(todo_config.get("gpt_todos_repo", "")).strip()
    if configured_repo:
        return _expand_home(configured_repo, home_path) / "agenda"
    configured_path = str(todo_config.get("org_path", "")).strip()
    if configured_path:
        return _expand_home(configured_path, home_path)
    return home_path / ".local" / "share" / "zarathushtra" / "todos.org"


def _parse_file(path: Path) -> list[_OrgTask]:
    lines = path.read_text(encoding="utf-8").splitlines(keepends=True)
    keywords = _todo_keywords(lines)
    heading_rows: list[tuple[int, int, str, str]] = []
    for index, line in enumerate(lines):
        match = _HEADING.match(line.rstrip("\n"))
        if match is None:
            continue
        level = len(match.group("stars"))
        heading_rows.append((index, level, match.group("status"), match.group("rest")))

    tasks: list[_OrgTask] = []
    for row_index, (start, level, status, rest) in enumerate(heading_rows):
        if status not in keywords:
            continue
        end = (
            heading_rows[row_index + 1][0]
            if row_index + 1 < len(heading_rows)
            else len(lines)
        )
        task = _parse_task(path, lines, start, end, level, status, rest)
        if task is not None:
            tasks.append(task)
    return tasks


def _parse_task(
    path: Path,
    lines: Sequence[str],
    start: int,
    end: int,
    level: int,
    status: str,
    rest: str,
) -> Optional[_OrgTask]:
    priority: Optional[str] = None
    if rest.startswith("[#") and len(rest) >= 4 and rest[3] == "]":
        priority = rest[2].upper()
        rest = rest[4:].lstrip()
    tag_match = _TAG_SUFFIX.search(rest)
    tags: list[str] = []
    if tag_match is not None:
        tag_text = tag_match.group("tags")
        tags = [tag for tag in tag_text.strip(":").split(":") if tag]
        title = rest[: tag_match.start()].strip()
    else:
        title = rest.strip()
    if not title:
        return None

    property_lines: list[str] = []
    properties: dict[str, str] = {}
    managed_indices: set[int] = set()
    property_start: Optional[int] = None
    property_end: Optional[int] = None
    for index in range(start + 1, end):
        if lines[index].strip().upper() == ":PROPERTIES:":
            property_start = index
            for close_index in range(index + 1, end):
                if lines[close_index].strip().upper() == ":END:":
                    property_end = close_index
                    break
            break
    if property_start is not None and property_end is not None:
        managed_indices.update(range(property_start, property_end + 1))
        for line in lines[property_start + 1 : property_end]:
            property_lines.append(line)
            match = _PROPERTY.match(line.rstrip("\n"))
            if match is not None:
                properties[match.group("key").upper()] = match.group("value").strip()

    scheduled_at: Optional[str] = None
    deadline_at: Optional[str] = None
    repeater: Optional[str] = None
    completed_at: Optional[str] = None
    for index in range(start + 1, end):
        if index in managed_indices:
            continue
        stripped = lines[index].rstrip("\n")
        match = _SCHEDULED.match(stripped)
        if match is not None:
            scheduled_at, repeater = _parse_org_timestamp(match.group("stamp"))
            managed_indices.add(index)
            continue
        match = _DEADLINE.match(stripped)
        if match is not None:
            deadline_at, _ = _parse_org_timestamp(match.group("stamp"))
            managed_indices.add(index)
            continue
        match = _CLOSED.match(stripped)
        if match is not None:
            completed_at, _ = _parse_org_timestamp(match.group("stamp"))
            managed_indices.add(index)

    stable_id = properties.get("ID")
    if not stable_id:
        stable_id = str(uuid.uuid5(uuid.NAMESPACE_URL, f"{path.resolve()}:{start}:{title}"))
    created_at = _parse_property_timestamp(properties.get("CREATED"))
    if created_at is None:
        created_at = datetime.fromtimestamp(path.stat().st_mtime).replace(microsecond=0).isoformat()
    updated_at = _parse_property_timestamp(properties.get("UPDATED"))
    duration_minutes = _parse_effort(properties.get("EFFORT"))
    content_lines = tuple(
        lines[index]
        for index in range(start + 1, end)
        if index not in managed_indices
    )
    notes = "".join(content_lines).strip("\n") or None
    record = TodoRecord(
        id=stable_id,
        title=title,
        status=status,
        priority=priority,
        scheduled_at=scheduled_at,
        deadline_at=deadline_at,
        repeater=repeater,
        duration_minutes=duration_minutes,
        created_at=created_at,
        updated_at=updated_at,
        completed_at=completed_at,
        notes=notes,
    )
    return _OrgTask(
        path=path,
        level=level,
        start=start,
        end=end,
        record=record,
        tags=tuple(sorted(tags)),
        property_lines=tuple(property_lines),
        properties=properties,
        content_lines=content_lines,
    )


def _todo_keywords(lines: Sequence[str]) -> set[str]:
    keywords = set(ORG_STATUSES)
    for line in lines:
        stripped = line.strip()
        if not stripped.lower().startswith("#+todo:"):
            continue
        payload = stripped.split(":", 1)[1]
        for token in payload.replace("|", " ").split():
            keyword = token.split("(", 1)[0].strip().upper()
            if keyword:
                keywords.add(keyword)
    return keywords


def _render_task_block(
    record: TodoRecord,
    tags: Sequence[str],
    level: int,
    property_lines: Sequence[str],
    notes: str,
) -> str:
    priority = f" [#{record.priority}]" if record.priority else ""
    tag_suffix = f" :{':'.join(sorted(tags))}:" if tags else ""
    parts = [f"{'*' * level} {record.status}{priority} {record.title}{tag_suffix}\n"]
    if record.completed_at is not None and record.status in TERMINAL_STATUSES:
        parts.append(f"CLOSED: {_format_inactive_timestamp(record.completed_at)}\n")
    parts.append(":PROPERTIES:\n")
    parts.extend(_ensure_newlines(property_lines))
    parts.append(":END:\n")
    if record.scheduled_at is not None:
        parts.append(f"SCHEDULED: {_format_active_timestamp(record.scheduled_at, record.repeater)}\n")
    if record.deadline_at is not None:
        parts.append(f"DEADLINE: {_format_active_timestamp(record.deadline_at, None)}\n")
    if notes:
        parts.append(notes.rstrip("\n") + "\n")
    parts.append("\n")
    return "".join(parts)


def _rewrite_property_lines(
    lines: Sequence[str],
    todo_id: str,
    created_at: str,
    duration_minutes: Optional[int],
) -> list[str]:
    result: list[str] = []
    seen_id = False
    seen_created = False
    seen_effort = False
    for line in lines:
        match = _PROPERTY.match(line.rstrip("\n"))
        if match is None:
            result.append(line)
            continue
        key = match.group("key").upper()
        if key == "ID":
            result.append(_format_property_line("ID", todo_id, line))
            seen_id = True
        elif key == "CREATED":
            result.append(_format_property_line("CREATED", _format_inactive_timestamp(created_at), line))
            seen_created = True
        elif key == "EFFORT":
            seen_effort = True
            if duration_minutes is not None:
                result.append(_format_property_line("EFFORT", _format_effort(duration_minutes), line))
        else:
            result.append(line)
    if not seen_id:
        result.insert(0, f":ID:       {todo_id}\n")
    if not seen_created:
        insert_at = 1 if result else 0
        result.insert(insert_at, f":CREATED:  {_format_inactive_timestamp(created_at)}\n")
    if duration_minutes is not None and not seen_effort:
        result.append(f":EFFORT:   {_format_effort(duration_minutes)}\n")
    return result


def _format_property_line(key: str, value: str, original: str) -> str:
    match = _PROPERTY.match(original.rstrip("\n"))
    spacing = "   "
    if match is not None:
        spacing = match.group("spacing") or " "
    return f":{key}:{spacing}{value}\n"


def _parse_org_timestamp(stamp: str) -> tuple[Optional[str], Optional[str]]:
    match = _ORG_STAMP.match(stamp.strip())
    if match is None:
        return None, None
    date_text = match.group("date")
    time_text = match.group("time") or "00:00"
    value = datetime.strptime(f"{date_text} {time_text}", "%Y-%m-%d %H:%M")
    return value.isoformat(), match.group("repeater")


def _format_active_timestamp(value: str, repeater: Optional[str]) -> str:
    parsed = datetime.fromisoformat(_validate_timestamp("timestamp", value))
    suffix = f" {repeater}" if repeater else ""
    return parsed.strftime(f"<%Y-%m-%d %a %H:%M{suffix}>")


def _format_inactive_timestamp(value: str) -> str:
    parsed = datetime.fromisoformat(_validate_timestamp("timestamp", value))
    return parsed.strftime("[%Y-%m-%d %a %H:%M]")


def _parse_property_timestamp(value: Optional[str]) -> Optional[str]:
    if not value:
        return None
    parsed, _ = _parse_org_timestamp(value)
    if parsed is not None:
        return parsed
    try:
        return datetime.fromisoformat(value).replace(microsecond=0).isoformat()
    except ValueError:
        return None


def _format_effort(minutes: int) -> str:
    hours, remainder = divmod(minutes, 60)
    return f"{hours}:{remainder:02d}"


def _parse_effort(value: Optional[str]) -> Optional[int]:
    if not value:
        return None
    text = value.strip()
    if re.fullmatch(r"[0-9]+", text):
        minutes = int(text)
        return minutes if minutes > 0 else None
    match = re.fullmatch(r"(?P<hours>[0-9]+):(?P<minutes>[0-5][0-9])", text)
    if match is None:
        return None
    minutes = int(match.group("hours")) * 60 + int(match.group("minutes"))
    return minutes if minutes > 0 else None


def _occurrences(scheduled_at: str, repeater: Optional[str], count: int = 5) -> list[str]:
    start = _validate_timestamp("scheduled_at", scheduled_at)
    if repeater is None:
        return [start]
    return [start, *_expand_org_repeater(start, repeater, count)]


def _expand_org_repeater(start_iso: str, repeater: str, count: int) -> list[str]:
    match = _ORG_REPEATER.fullmatch(_validate_org_repeater(repeater) or "")
    if match is None:
        return []
    amount = int(match.group(1))
    unit = match.group(2)
    start = datetime.fromisoformat(start_iso)
    values = []
    current = start
    for _ in range(count):
        if unit == "min":
            current += timedelta(minutes=amount)
        elif unit == "h":
            current += timedelta(hours=amount)
        elif unit == "d":
            current += timedelta(days=amount)
        elif unit == "w":
            current += timedelta(weeks=amount)
        elif unit == "m":
            current = _add_months(current, amount)
        elif unit == "y":
            current = _add_years(current, amount)
        values.append(current.replace(microsecond=0).isoformat())
    return values


def _add_months(value: datetime, months: int) -> datetime:
    month_index = value.month - 1 + months
    year = value.year + month_index // 12
    month = month_index % 12 + 1
    day = min(value.day, calendar.monthrange(year, month)[1])
    return value.replace(year=year, month=month, day=day)


def _add_years(value: datetime, years: int) -> datetime:
    year = value.year + years
    day = min(value.day, calendar.monthrange(year, value.month)[1])
    return value.replace(year=year, day=day)


def _validate_title(value: Any) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError("title must not be empty")
    return value.strip()


def _validate_org_status(value: Any) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError("status must not be empty")
    status = value.strip().upper()
    if status not in ORG_STATUSES:
        raise ValueError(f"unsupported Org todo status: {status}")
    return status


def _validate_priority(value: Any) -> Optional[str]:
    if value is None:
        return None
    if not isinstance(value, str) or value.upper() not in {"A", "B", "C"}:
        raise ValueError("priority must be A, B, C, or cleared")
    return value.upper()


def _validate_optional_timestamp(name: str, value: Any) -> Optional[str]:
    if value is None:
        return None
    return _validate_timestamp(name, value)


def _validate_timestamp(name: str, value: Any) -> str:
    if not isinstance(value, str) or not value.strip():
        raise ValueError(f"{name} must be an ISO-8601 timestamp")
    try:
        parsed = datetime.fromisoformat(value.strip())
    except ValueError as error:
        raise ValueError(f"{name} must be an ISO-8601 timestamp") from error
    return parsed.replace(microsecond=0).isoformat()


def _validate_org_repeater(value: Any) -> Optional[str]:
    if value is None:
        return None
    if not isinstance(value, str) or _ORG_REPEATER.fullmatch(value.strip()) is None:
        raise ValueError("repeater must use Org syntax such as +1w, ++1w, or .+1w")
    return value.strip()


def _validate_optional_duration(value: Any) -> Optional[int]:
    if value is None:
        return None
    return _validate_duration(value)


def _validate_duration(value: Any) -> int:
    if isinstance(value, bool) or not isinstance(value, int) or value <= 0:
        raise ValueError("duration_minutes must be a positive integer")
    return value


def _clean_tags(tags: Iterable[str]) -> list[str]:
    if isinstance(tags, str):
        raise ValueError("tags must be an iterable of strings")
    cleaned = set()
    for tag in tags:
        if not isinstance(tag, str):
            raise ValueError("tags must be strings")
        value = tag.strip().strip(":")
        if value:
            cleaned.add(value)
    return sorted(cleaned)


def _required_value(name: str, value: PatchValue) -> Any:
    if value is CLEAR:
        raise ValueError(f"{name} cannot be cleared")
    return value


def _patched_optional(value: PatchValue, current: Any) -> Any:
    if value is PRESERVE:
        return current
    if value is CLEAR:
        return None
    return value


def _comparable_datetime(value: str) -> datetime:
    parsed = datetime.fromisoformat(value)
    if parsed.tzinfo is not None:
        parsed = parsed.astimezone().replace(tzinfo=None)
    return parsed


def _id_matches(needle: str, candidate: str) -> bool:
    return candidate == needle or candidate.startswith(needle)


def _expand_home(value: str, home: Path) -> Path:
    if value == "~":
        return home
    if value.startswith("~/"):
        return home / value[2:]
    return Path(value).expanduser()


def _ensure_newlines(lines: Sequence[str]) -> list[str]:
    return [line if line.endswith("\n") else line + "\n" for line in lines]


def _atomic_write(path: Path, content: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with tempfile.NamedTemporaryFile(
        mode="w",
        encoding="utf-8",
        dir=path.parent,
        prefix=f".{path.name}.",
        delete=False,
    ) as handle:
        handle.write(content)
        temp_path = Path(handle.name)
    temp_path.replace(path)


def _now_iso() -> str:
    return datetime.now().replace(microsecond=0).isoformat()