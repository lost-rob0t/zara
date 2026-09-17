"""Org/Org-roam graph, Doom-style rendering, and memory projection.

Org files are canonical. Every index, long-term-memory row, and Prolog fact
produced here is a disposable projection that can be rebuilt from the files.
"""

from __future__ import annotations

import hashlib
import html
import json
import os
import re
import shlex
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Iterable, Optional, Sequence

from .memory import MemoryManager


_HEADING_RE = re.compile(r"^(\*+)\s+(.+?)\s*$")
_LINK_RE = re.compile(r"\[\[([^\]\n]+)\](?:\[([^\]\n]*)\])?\]")
_KEYWORD_RE = re.compile(r"^#\+([A-Za-z0-9_+-]+):\s*(.*)$")
_PROPERTY_RE = re.compile(r"^:([^:]+):\s*(.*)$")
_TAG_SUFFIX_RE = re.compile(
    r"\s+(:[A-Za-z0-9_@#%+.-]+(?::[A-Za-z0-9_@#%+.-]+)*:)\s*$"
)
_TODO_WORDS = frozenset(
    {
        "TODO",
        "NEXT",
        "WAIT",
        "WAITING",
        "HOLD",
        "IDEA",
        "DOING",
        "STARTED",
        "DONE",
        "CANCELLED",
        "CANCELED",
        "DEFERRED",
    }
)


@dataclass(frozen=True)
class OrgLink:
    target: str
    description: str
    kind: str


@dataclass(frozen=True)
class OrgNode:
    key: str
    node_id: Optional[str]
    file_path: str
    title: str
    level: int
    todo: Optional[str]
    tags: tuple[str, ...]
    properties: dict[str, str]
    aliases: tuple[str, ...]
    body: str
    links: tuple[OrgLink, ...]
    parent_key: Optional[str]
    line: int

    @property
    def project(self) -> Optional[str]:
        value = self.properties.get("PROJECT") or self.properties.get("CATEGORY")
        value = (value or "").strip()
        return value or None


@dataclass(frozen=True)
class OrgDocument:
    path: str
    title: str
    keywords: dict[str, str]
    file_node: Optional[OrgNode]
    nodes: tuple[OrgNode, ...]
    text: str


@dataclass(frozen=True)
class OrgMemorySyncReport:
    created: int = 0
    updated: int = 0
    deleted: int = 0
    unchanged: int = 0


@dataclass(frozen=True)
class OrgMemoryContextBundle:
    symbolic_memories: tuple[dict[str, Any], ...]
    project_context: tuple[dict[str, Any], ...]
    facts: tuple[dict[str, Any], ...]
    recent_chats: tuple[dict[str, Any], ...]


@dataclass(frozen=True)
class OrgWorkspaceRefresh:
    changed: bool
    index: "OrgRoamIndex"
    signature: str


def _tags(value: str) -> tuple[str, ...]:
    value = (value or "").strip()
    if value.startswith(":") and value.endswith(":"):
        return tuple(tag for tag in value.strip(":").split(":") if tag)
    return tuple(tag for tag in re.split(r"[,\s]+", value) if tag)


def _aliases(value: str) -> tuple[str, ...]:
    value = (value or "").strip()
    if not value:
        return ()
    try:
        parsed = tuple(part.strip() for part in shlex.split(value) if part.strip())
    except ValueError:
        parsed = tuple(part.strip() for part in value.split() if part.strip())
    return parsed


def _split_heading(raw: str) -> tuple[Optional[str], str, tuple[str, ...]]:
    tags: tuple[str, ...] = ()
    match = _TAG_SUFFIX_RE.search(raw)
    if match:
        tags = _tags(match.group(1))
        raw = raw[: match.start()].rstrip()
    parts = raw.split(maxsplit=1)
    todo: Optional[str] = None
    if parts and parts[0].upper() in _TODO_WORDS:
        todo = parts[0].upper()
        raw = parts[1] if len(parts) > 1 else ""
    return todo, raw.strip(), tags


def _links(text: str) -> tuple[OrgLink, ...]:
    result: list[OrgLink] = []
    for match in _LINK_RE.finditer(text or ""):
        target = match.group(1).strip()
        description = (match.group(2) or target).strip()
        if ":" in target:
            kind, value = target.split(":", 1)
            kind = kind.casefold()
            if kind in {"id", "file", "https", "http", "mailto"}:
                result.append(OrgLink(value, description, kind))
                continue
        result.append(OrgLink(target, description, "fuzzy"))
    return tuple(result)


def _stable_key(path: str, line: int, title: str) -> str:
    seed = f"{path}:{line}:{title}".encode("utf-8")
    return "org:" + hashlib.blake2s(seed, digest_size=10).hexdigest()


def _make_node(
    *,
    path: str,
    title: str,
    level: int,
    todo: Optional[str],
    tags: tuple[str, ...],
    properties: dict[str, str],
    body: str,
    parent_key: Optional[str],
    line: int,
) -> OrgNode:
    node_id = (properties.get("ID") or "").strip() or None
    key = node_id or _stable_key(path, line, title)
    aliases = _aliases(properties.get("ROAM_ALIASES", ""))
    return OrgNode(
        key=key,
        node_id=node_id,
        file_path=path,
        title=title,
        level=level,
        todo=todo,
        tags=tags,
        properties=dict(properties),
        aliases=aliases,
        body=body.strip(),
        links=_links(body),
        parent_key=parent_key,
        line=line,
    )


def parse_org_text(text: str, *, path: str = "<memory>") -> OrgDocument:
    """Parse the structural Org subset used by rendering, roam, and memory.

    Unknown Org syntax is preserved in ``OrgDocument.text`` and node bodies.
    Nothing is executed while parsing, including source blocks and macros.
    """
    lines = text.splitlines()
    keywords: dict[str, str] = {}
    for line in lines:
        match = _KEYWORD_RE.match(line)
        if match:
            keywords[match.group(1).upper()] = match.group(2).strip()

    title = keywords.get("TITLE") or Path(path).stem
    file_tags = _tags(keywords.get("FILETAGS", ""))
    file_properties: dict[str, str] = {}
    file_body: list[str] = []
    heading_records: list[dict[str, Any]] = []
    stack: list[tuple[int, str]] = []
    current: Optional[dict[str, Any]] = None
    before_first_heading = True
    in_file_properties = False
    in_node_properties = False

    for index, line in enumerate(lines, start=1):
        heading = _HEADING_RE.match(line)
        if heading:
            before_first_heading = False
            in_file_properties = False
            in_node_properties = False
            level = len(heading.group(1))
            todo, heading_title, heading_tags = _split_heading(heading.group(2))
            while stack and stack[-1][0] >= level:
                stack.pop()
            parent_key = stack[-1][1] if stack else None
            current = {
                "path": path,
                "title": heading_title,
                "level": level,
                "todo": todo,
                "tags": heading_tags,
                "properties": {},
                "body": [],
                "parent_key": parent_key,
                "line": index,
            }
            provisional_key = _stable_key(path, index, heading_title)
            current["provisional_key"] = provisional_key
            heading_records.append(current)
            stack.append((level, provisional_key))
            continue

        if before_first_heading:
            if line.strip().upper() == ":PROPERTIES:":
                in_file_properties = True
                continue
            if in_file_properties and line.strip().upper() == ":END:":
                in_file_properties = False
                continue
            if in_file_properties:
                prop = _PROPERTY_RE.match(line.strip())
                if prop:
                    file_properties[prop.group(1).upper()] = prop.group(2).strip()
                continue
            if not _KEYWORD_RE.match(line):
                file_body.append(line)
            continue

        if current is None:
            continue
        if line.strip().upper() == ":PROPERTIES:":
            in_node_properties = True
            continue
        if in_node_properties and line.strip().upper() == ":END:":
            in_node_properties = False
            continue
        if in_node_properties:
            prop = _PROPERTY_RE.match(line.strip())
            if prop:
                current["properties"][prop.group(1).upper()] = prop.group(2).strip()
            continue
        current["body"].append(line)

    provisional_to_final: dict[str, str] = {}
    nodes: list[OrgNode] = []
    for record in heading_records:
        node = _make_node(
            path=record["path"],
            title=record["title"],
            level=record["level"],
            todo=record["todo"],
            tags=record["tags"],
            properties=record["properties"],
            body="\n".join(record["body"]),
            parent_key=record["parent_key"],
            line=record["line"],
        )
        provisional_to_final[record["provisional_key"]] = node.key
        nodes.append(node)

    fixed_nodes = tuple(
        OrgNode(
            key=node.key,
            node_id=node.node_id,
            file_path=node.file_path,
            title=node.title,
            level=node.level,
            todo=node.todo,
            tags=node.tags,
            properties=node.properties,
            aliases=node.aliases,
            body=node.body,
            links=node.links,
            parent_key=provisional_to_final.get(node.parent_key or "", node.parent_key),
            line=node.line,
        )
        for node in nodes
    )

    file_node = None
    if file_properties.get("ID"):
        file_node = _make_node(
            path=path,
            title=title,
            level=0,
            todo=None,
            tags=file_tags,
            properties=file_properties,
            body="\n".join(file_body),
            parent_key=None,
            line=1,
        )

    return OrgDocument(
        path=path,
        title=title,
        keywords=keywords,
        file_node=file_node,
        nodes=fixed_nodes,
        text=text,
    )


def parse_org_file(path: os.PathLike[str] | str, *, max_bytes: int = 2_000_000) -> OrgDocument:
    file_path = Path(path).expanduser().resolve()
    size = file_path.stat().st_size
    if size > max_bytes:
        raise ValueError(f"Org file exceeds {max_bytes} byte limit: {file_path}")
    return parse_org_text(file_path.read_text(encoding="utf-8"), path=str(file_path))


class OrgRoamIndex:
    def __init__(self, documents: Sequence[OrgDocument]) -> None:
        self.documents = tuple(documents)
        self.nodes = tuple(
            node
            for document in self.documents
            for node in ((document.file_node,) if document.file_node else ()) + document.nodes
        )
        by_id: dict[str, list[OrgNode]] = {}
        self._by_key = {node.key: node for node in self.nodes}
        for node in self.nodes:
            if node.node_id:
                by_id.setdefault(node.node_id, []).append(node)
        self._by_id = by_id
        self.duplicate_ids = tuple(sorted(node_id for node_id, rows in by_id.items() if len(rows) > 1))
        self._incoming: dict[str, list[OrgNode]] = {}
        for source in self.nodes:
            for link in source.links:
                target = self._resolve_link(source, link)
                if target is not None:
                    self._incoming.setdefault(target.key, []).append(source)

    @classmethod
    def empty(cls) -> "OrgRoamIndex":
        return cls(())

    @classmethod
    def from_documents(cls, documents: Sequence[OrgDocument]) -> "OrgRoamIndex":
        return cls(documents)

    @classmethod
    def from_paths(
        cls,
        roots: Iterable[os.PathLike[str] | str],
        *,
        max_files: int = 2000,
        max_file_bytes: int = 2_000_000,
    ) -> "OrgRoamIndex":
        paths: list[Path] = []
        for raw_root in roots:
            root = Path(raw_root).expanduser()
            if root.is_file() and root.suffix.casefold() == ".org":
                paths.append(root)
                continue
            if root.is_dir():
                paths.extend(
                    path
                    for path in root.rglob("*.org")
                    if ".git" not in path.parts and path.is_file()
                )
        unique = sorted({path.resolve() for path in paths}, key=str)
        if len(unique) > max_files:
            raise ValueError(f"Org workspace exceeds {max_files} file limit")
        documents = tuple(parse_org_file(path, max_bytes=max_file_bytes) for path in unique)
        return cls(documents)

    def get(self, key_or_id: str) -> Optional[OrgNode]:
        direct = self._by_key.get(key_or_id)
        if direct is not None:
            return direct
        rows = self._by_id.get(key_or_id, ())
        return rows[0] if len(rows) == 1 else None

    def backlinks(self, key_or_id: str) -> tuple[OrgNode, ...]:
        node = self.get(key_or_id)
        if node is None:
            return ()
        rows = self._incoming.get(node.key, ())
        return tuple(sorted(rows, key=lambda item: (item.file_path, item.line, item.title)))

    def neighbors(self, key_or_id: str) -> tuple[OrgNode, ...]:
        node = self.get(key_or_id)
        if node is None:
            return ()
        neighbors: dict[str, OrgNode] = {}
        if node.parent_key and node.parent_key in self._by_key:
            parent = self._by_key[node.parent_key]
            neighbors[parent.key] = parent
        for candidate in self.nodes:
            if candidate.parent_key == node.key:
                neighbors[candidate.key] = candidate
        for link in node.links:
            target = self._resolve_link(node, link)
            if target is not None:
                neighbors[target.key] = target
        for source in self.backlinks(node.key):
            neighbors[source.key] = source
        return tuple(sorted(neighbors.values(), key=lambda item: (item.level, item.title.casefold(), item.key)))

    def search(self, query: str, *, limit: int = 50) -> tuple[OrgNode, ...]:
        terms = tuple(term for term in re.findall(r"[\w@#%+.-]+", query.casefold()) if term)
        if not terms:
            return ()
        scored: list[tuple[int, str, OrgNode]] = []
        for node in self.nodes:
            title = node.title.casefold()
            aliases = " ".join(node.aliases).casefold()
            tags = " ".join(node.tags).casefold()
            body = node.body.casefold()
            fields = f"{title} {aliases} {tags} {body}"
            if not all(term in fields for term in terms):
                continue
            score = sum(8 for term in terms if term in title)
            score += sum(6 for term in terms if term in aliases)
            score += sum(4 for term in terms if term in tags)
            score += sum(1 for term in terms if term in body)
            scored.append((-score, node.title.casefold(), node))
        scored.sort(key=lambda row: (row[0], row[1], row[2].key))
        return tuple(row[2] for row in scored[: max(0, int(limit))])

    def _resolve_link(self, source: OrgNode, link: OrgLink) -> Optional[OrgNode]:
        if link.kind == "id":
            rows = self._by_id.get(link.target, ())
            return rows[0] if len(rows) == 1 else None
        if link.kind != "file":
            return None
        target, _, anchor = link.target.partition("::")
        candidate = (Path(source.file_path).parent / target).resolve()
        rows = [node for node in self.nodes if Path(node.file_path).resolve() == candidate]
        if not rows:
            return None
        if not anchor:
            file_rows = [node for node in rows if node.level == 0]
            return file_rows[0] if file_rows else rows[0]
        anchor = anchor.lstrip("*").strip().casefold()
        matches = [node for node in rows if node.title.casefold() == anchor]
        return matches[0] if len(matches) == 1 else None


class OrgRoamWorkspace:
    """Bounded configured Org workspace with change-aware reindexing."""

    def __init__(
        self,
        roots: Sequence[str],
        *,
        max_files: int = 2000,
        max_file_bytes: int = 2_000_000,
    ) -> None:
        self.roots = tuple(str(Path(root).expanduser()) for root in roots if str(root).strip())
        self.max_files = max_files
        self.max_file_bytes = max_file_bytes
        self._signature = ""
        self.index = OrgRoamIndex.empty()

    def refresh(self, *, force: bool = False) -> OrgWorkspaceRefresh:
        signature = self._source_signature()
        if not force and signature == self._signature:
            return OrgWorkspaceRefresh(False, self.index, signature)
        self.index = OrgRoamIndex.from_paths(
            self.roots,
            max_files=self.max_files,
            max_file_bytes=self.max_file_bytes,
        )
        self._signature = signature
        return OrgWorkspaceRefresh(True, self.index, signature)

    def _source_signature(self) -> str:
        rows: list[str] = []
        count = 0
        for raw_root in self.roots:
            root = Path(raw_root)
            candidates = [root] if root.is_file() else sorted(root.rglob("*.org")) if root.is_dir() else []
            for path in candidates:
                if not path.is_file() or ".git" in path.parts:
                    continue
                stat = path.stat()
                rows.append(f"{path.resolve()}:{stat.st_mtime_ns}:{stat.st_size}")
                count += 1
                if count > self.max_files:
                    raise ValueError(f"Org workspace exceeds {self.max_files} file limit")
        return hashlib.blake2s("\n".join(rows).encode("utf-8"), digest_size=16).hexdigest()


def heading_font_pt(level: int, base_font_pt: float = 12.0) -> float:
    ratios = {0: 1.55, 1: 1.45, 2: 1.30, 3: 1.18, 4: 1.10, 5: 1.04}
    return round(float(base_font_pt) * ratios.get(max(0, int(level)), 1.0), 2)


def _render_link_markup(line: str) -> str:
    result: list[str] = []
    cursor = 0
    for match in _LINK_RE.finditer(line):
        result.append(html.escape(line[cursor : match.start()]))
        raw_target = match.group(1).strip()
        label = (match.group(2) or raw_target).strip()
        result.append(
            f'<a href="{html.escape(raw_target, quote=True)}">{html.escape(label)}</a>'
        )
        cursor = match.end()
    result.append(html.escape(line[cursor:]))
    return "".join(result)


def render_org_html(document: OrgDocument, *, base_font_pt: float = 12.0) -> str:
    """Render source-preserving Org with Doom-like outline typography.

    Heading stars remain visible. The renderer changes typography, not Org
    syntax, and leaves unsupported constructs visible instead of flattening them.
    """
    rendered: list[str] = [
        "<html><head><style>",
        f"body {{ font-family: monospace; font-size: {float(base_font_pt):.2f}pt; white-space: pre-wrap; }}",
        "pre { margin: 0; font-family: monospace; white-space: pre-wrap; }",
        "a { text-decoration: underline; }",
        ".property { opacity: 0.72; }",
        ".keyword { opacity: 0.82; }",
        ".block { opacity: 0.92; }",
        "</style></head><body><pre>",
    ]
    in_block = False
    for line in document.text.splitlines():
        heading = _HEADING_RE.match(line)
        keyword = _KEYWORD_RE.match(line)
        stripped = line.strip()
        if heading:
            level = len(heading.group(1))
            rendered.append(
                f'<span style="font-size:{heading_font_pt(level, base_font_pt):.2f}pt; font-weight:600">'
                f"{_render_link_markup(line)}</span>"
            )
        elif stripped.casefold().startswith("#+begin_"):
            in_block = True
            rendered.append(f'<span class="block">{_render_link_markup(line)}</span>')
        elif stripped.casefold().startswith("#+end_"):
            rendered.append(f'<span class="block">{_render_link_markup(line)}</span>')
            in_block = False
        elif stripped.startswith(":") and stripped.endswith(":"):
            rendered.append(f'<span class="property">{_render_link_markup(line)}</span>')
        elif keyword:
            rendered.append(f'<span class="keyword">{_render_link_markup(line)}</span>')
        elif in_block:
            rendered.append(f'<span class="block">{html.escape(line)}</span>')
        else:
            rendered.append(_render_link_markup(line))
        rendered.append("\n")
    rendered.append("</pre></body></html>")
    return "".join(rendered)


def _metadata_tags(record: dict[str, Any]) -> tuple[str, ...]:
    metadata = record.get("metadata") or {}
    value = metadata.get("tags") if isinstance(metadata, dict) else None
    if isinstance(value, str):
        return tuple(tag.strip() for tag in value.split(",") if tag.strip())
    if isinstance(value, list):
        return tuple(str(tag).strip() for tag in value if str(tag).strip())
    return ()


def _node_source(node: OrgNode) -> str:
    return f"org-roam:{node.node_id or node.key}"


def _node_memory_text(node: OrgNode) -> str:
    stars = "*" * max(1, node.level)
    todo = f"{node.todo} " if node.todo else ""
    tags = f" :{':'.join(node.tags)}:" if node.tags else ""
    heading = f"{stars} {todo}{node.title}{tags}".rstrip()
    return heading if not node.body else f"{heading}\n{node.body}".strip()


class OrgRoamMemoryHook:
    """Project Org-roam nodes into Zara memory and optional Prolog facts."""

    def __init__(self, memory: MemoryManager, *, prolog_engine: Any = None) -> None:
        self.memory = memory
        self.prolog_engine = prolog_engine

    def sync_index(
        self,
        index: OrgRoamIndex,
        *,
        node_ids: Optional[Sequence[str]] = None,
        project: Optional[str] = None,
    ) -> OrgMemorySyncReport:
        selected = tuple(
            node
            for node in index.nodes
            if node.node_id and (node_ids is None or node.node_id in set(node_ids))
        )
        desired = {_node_source(node): node for node in selected}
        existing = {
            str((record.get("metadata") or {}).get("source")): record
            for record in self.memory.list_memories(limit=100_000, include_kinds=("fact",))
            if str((record.get("metadata") or {}).get("source", "")).startswith("org-roam:")
        }
        created = updated = deleted = unchanged = 0

        for source, node in desired.items():
            text = _node_memory_text(node)
            node_project = node.project or project
            tags = ["org-roam", "symbolic-memory", *node.tags]
            if node_project:
                tags.append(f"project:{node_project}")
            old = existing.get(source)
            old_tags = _metadata_tags(old) if old else ()
            if old and old.get("text") == text and set(old_tags) == set(tags):
                unchanged += 1
                continue
            if old and old.get("id"):
                self.memory.forget(memory_id=str(old["id"]))
                updated += 1
            else:
                created += 1
            self.memory.remember_fact(text, tags=tags, source=source)
            self._project_prolog(source, node, node_project, text, tags)

        if node_ids is None:
            for source, record in existing.items():
                if source in desired:
                    continue
                if record.get("id"):
                    self.memory.forget(memory_id=str(record["id"]))
                    self._remove_prolog(source)
                    deleted += 1

        self._run_prolog_hook(created, updated, deleted, unchanged)
        return OrgMemorySyncReport(created, updated, deleted, unchanged)

    def context_bundle(
        self,
        query: str,
        *,
        project: Optional[str] = None,
        limit: int = 6,
        recent_chat_limit: int = 4,
    ) -> OrgMemoryContextBundle:
        symbolic = tuple(
            self.memory.retrieve(
                query,
                k=limit,
                include_kinds=("fact",),
                tags=["symbolic-memory"],
            )
        )
        project_rows: tuple[dict[str, Any], ...] = ()
        if project:
            project_rows = tuple(
                self.memory.retrieve(
                    query,
                    k=limit,
                    include_kinds=("fact",),
                    tags=[f"project:{project}"],
                )
            )
        generic = self.memory.retrieve(query, k=max(limit * 2, limit), include_kinds=("fact",))
        facts = tuple(
            row
            for row in generic
            if "symbolic-memory" not in _metadata_tags(row)
        )[:limit]
        recent = tuple(
            self.memory.list_memories(
                limit=recent_chat_limit,
                include_kinds=("summary",),
            )
        )
        return OrgMemoryContextBundle(symbolic, project_rows, facts, recent)

    def python_after_hook(
        self,
        *,
        index: Optional[OrgRoamIndex] = None,
        workspace: Optional[OrgRoamWorkspace] = None,
        node_ids: Optional[Sequence[str]] = None,
        project: Optional[str] = None,
    ):
        if (index is None) == (workspace is None):
            raise ValueError("provide exactly one of index or workspace")

        def after(_result: Any) -> None:
            target = index
            if workspace is not None:
                refresh = workspace.refresh()
                if not refresh.changed:
                    return
                target = refresh.index
            assert target is not None
            self.sync_index(target, node_ids=node_ids, project=project)

        return after

    def _project_prolog(
        self,
        source: str,
        node: OrgNode,
        project: Optional[str],
        text: str,
        tags: Sequence[str],
    ) -> None:
        if self.prolog_engine is None:
            return
        tags_term = "[{}]".format(", ".join(json.dumps(tag, ensure_ascii=False) for tag in tags))
        goal = (
            "org_roam_memory:replace_org_memory("
            f"{json.dumps(source, ensure_ascii=False)}, "
            f"{json.dumps(node.node_id or node.key, ensure_ascii=False)}, "
            f"{json.dumps(project or '', ensure_ascii=False)}, "
            f"{json.dumps(text, ensure_ascii=False)}, {tags_term})"
        )
        self.prolog_engine.query_once(goal)

    def _remove_prolog(self, source: str) -> None:
        if self.prolog_engine is not None:
            self.prolog_engine.query_once(
                f"org_roam_memory:remove_org_memory({json.dumps(source, ensure_ascii=False)})"
            )

    def _run_prolog_hook(self, created: int, updated: int, deleted: int, unchanged: int) -> None:
        if self.prolog_engine is None:
            return
        self.prolog_engine.query_once(
            "zara_hooks:run_hook(memory_sync, "
            f"org_roam_sync({created},{updated},{deleted},{unchanged}))"
        )
