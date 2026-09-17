"""Configured execution layer for the Org browser.

This module applies OrgBrowserConfig and Python hook transforms to the existing
Org/Org-roam parser and index. It also emits bounded Prolog lifecycle events
when a canonical PrologEngine is supplied by the owning runtime.
"""

from __future__ import annotations

import html
import json
import re
from pathlib import Path
from typing import Any, Optional, Sequence

from .org_browser import OrgBrowserConfig, OrgBrowserHookRegistry
from .org_roam import (
    OrgDocument,
    OrgMemorySyncReport,
    OrgNode,
    OrgRoamIndex,
    OrgRoamMemoryHook,
    OrgRoamWorkspace,
    OrgWorkspaceRefresh,
)


_HEADING_RE = re.compile(r"^(\*+)\s+(.+?)\s*$")
_LINK_RE = re.compile(r"\[\[([^\]\n]+)\](?:\[([^\]\n]*)\])?\]")
_KEYWORD_RE = re.compile(r"^#\+([A-Za-z0-9_+-]+):\s*(.*)$")


class ConfiguredOrgRoamWorkspace(OrgRoamWorkspace):
    def __init__(
        self,
        config: OrgBrowserConfig,
        hooks: OrgBrowserHookRegistry,
        *,
        prolog_engine: Any = None,
    ) -> None:
        super().__init__(
            config.roots,
            max_files=config.max_files,
            max_file_bytes=config.max_file_bytes,
        )
        self.browser_config = config
        self.browser_hooks = hooks
        self.prolog_engine = prolog_engine

    def refresh(self, *, force: bool = False) -> OrgWorkspaceRefresh:
        _run_prolog_hook(
            self.prolog_engine,
            "org_browser_before_index",
            f"workspace({_prolog_list(self.roots)})",
        )
        refresh = super().refresh(force=force)
        if refresh.changed:
            _run_prolog_hook(
                self.prolog_engine,
                "org_browser_after_index",
                f"index({len(refresh.index.nodes)},{_prolog_string(refresh.signature)})",
            )
        return refresh

    def visible_nodes(self, query: str = "") -> tuple[OrgNode, ...]:
        if query.strip():
            rows = self.index.search(query, limit=self.browser_config.search_limit)
        else:
            rows = self.index.nodes[: self.browser_config.search_limit]
        rows = self.browser_hooks.filter_nodes(rows, self.browser_config)
        return self.browser_hooks.sort_nodes(rows, self.browser_config)


class ConfiguredOrgRoamMemoryHook(OrgRoamMemoryHook):
    def __init__(
        self,
        memory,
        config: OrgBrowserConfig,
        hooks: OrgBrowserHookRegistry,
        *,
        prolog_engine: Any = None,
    ) -> None:
        super().__init__(memory, prolog_engine=prolog_engine)
        self.browser_config = config
        self.browser_hooks = hooks

    def sync_index(
        self,
        index: OrgRoamIndex,
        *,
        node_ids: Optional[Sequence[str]] = None,
        project: Optional[str] = None,
    ) -> OrgMemorySyncReport:
        if not self.browser_config.memory_sync:
            return OrgMemorySyncReport()

        _run_prolog_hook(
            self.prolog_engine,
            "org_browser_before_memory_sync",
            f"index({len(index.nodes)})",
        )
        requested = set(node_ids) if node_ids is not None else None
        selected = tuple(
            node
            for node in index.nodes
            if node.node_id and (requested is None or node.node_id in requested)
        )
        selected = self.browser_hooks.filter_nodes(selected, self.browser_config)
        desired = {_node_source(node): node for node in selected}
        existing = {
            str((record.get("metadata") or {}).get("source")): record
            for record in self.memory.list_memories(limit=100_000, include_kinds=("fact",))
            if str((record.get("metadata") or {}).get("source", "")).startswith("org-roam:")
        }
        created = updated = deleted = unchanged = 0

        for source, node in desired.items():
            text = self.browser_hooks.memory_text(
                node,
                _node_memory_text(node),
                self.browser_config,
            )
            node_project = node.project or project or self.browser_config.default_project
            tags = ["org-roam", "symbolic-memory", *node.tags]
            if node_project:
                tags.append(f"project:{node_project}")
            projected_tags = self.browser_hooks.memory_tags(node, tags, self.browser_config)
            old = existing.get(source)
            old_tags = _metadata_tags(old) if old else ()
            if old and old.get("text") == text and set(old_tags) == set(projected_tags):
                unchanged += 1
                continue
            if old and old.get("id"):
                self.memory.forget(memory_id=str(old["id"]))
                updated += 1
            else:
                created += 1
            self.memory.remember_fact(text, tags=list(projected_tags), source=source)
            self._project_prolog(source, node, node_project, text, projected_tags)

        if node_ids is None:
            for source, record in existing.items():
                if source in desired:
                    continue
                if record.get("id"):
                    self.memory.forget(memory_id=str(record["id"]))
                    self._remove_prolog(source)
                    deleted += 1

        report = OrgMemorySyncReport(created, updated, deleted, unchanged)
        self._run_prolog_hook(created, updated, deleted, unchanged)
        _run_prolog_hook(
            self.prolog_engine,
            "org_browser_after_memory_sync",
            f"sync({created},{updated},{deleted},{unchanged})",
        )
        return report

    def context_bundle(
        self,
        query: str,
        *,
        project: Optional[str] = None,
        limit: int = 6,
        recent_chat_limit: Optional[int] = None,
    ):
        return super().context_bundle(
            query,
            project=project or self.browser_config.default_project,
            limit=limit,
            recent_chat_limit=(
                self.browser_config.recent_chat_limit
                if recent_chat_limit is None
                else recent_chat_limit
            ),
        )


def render_org_document(
    document: OrgDocument,
    config: OrgBrowserConfig,
    hooks: OrgBrowserHookRegistry,
    *,
    prolog_engine: Any = None,
) -> str:
    """Render literal Org source using configured heading scales and hooks."""
    _run_prolog_hook(
        prolog_engine,
        "org_browser_before_render",
        f"document({_prolog_string(document.path)})",
    )
    rendered = [
        "<html><head><style>",
        f"body {{ font-family: monospace; font-size: {config.base_font_pt:.2f}pt; white-space: pre-wrap; }}",
        "pre { margin: 0; font-family: monospace; white-space: pre-wrap; }",
        "a { text-decoration: underline; }",
        ".property { opacity: 0.72; }",
        ".keyword { opacity: 0.82; }",
        ".block { opacity: 0.92; }",
        "</style></head><body><pre>",
    ]
    in_block = False
    in_properties = False
    for line in document.text.splitlines():
        heading = _HEADING_RE.match(line)
        keyword = _KEYWORD_RE.match(line)
        stripped = line.strip()
        upper = stripped.upper()
        if upper == ":PROPERTIES:":
            in_properties = True
        if heading:
            level = len(heading.group(1))
            size = config.base_font_pt * config.heading_scale(level)
            rendered.append(
                f'<span style="font-size:{size:.2f}pt; font-weight:600">'
                f"{_render_link_markup(line)}</span>"
            )
        elif in_properties and not config.show_properties:
            pass
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
        if not (in_properties and not config.show_properties):
            rendered.append("\n")
        if upper == ":END:" and in_properties:
            in_properties = False
    rendered.append("</pre></body></html>")
    value = hooks.render_document(document, "".join(rendered), config)
    _run_prolog_hook(
        prolog_engine,
        "org_browser_after_render",
        f"document({_prolog_string(document.path)})",
    )
    return value


def resolve_help_sources(
    repo_root: Path,
    config: OrgBrowserConfig,
    hooks: OrgBrowserHookRegistry,
) -> tuple[Path, ...]:
    root = Path(repo_root).resolve()
    rows: list[Path] = []
    for configured in config.help_sources:
        raw = str(configured).strip()
        if not raw:
            continue
        candidate = (root / raw).resolve()
        if root not in candidate.parents and candidate != root:
            continue
        if candidate.is_file() and candidate.suffix.casefold() == ".org":
            rows.append(candidate)
        elif candidate.is_dir():
            rows.extend(path for path in candidate.rglob("*.org") if path.is_file())
    relative = tuple(str(path.relative_to(root)) for path in sorted(set(rows), key=str))
    filtered = hooks.help_sources(relative, config)
    return tuple((root / path).resolve() for path in filtered)


def notify_node_selected(prolog_engine: Any, node: OrgNode) -> None:
    _run_prolog_hook(
        prolog_engine,
        "org_browser_node_selected",
        f"node({_prolog_string(node.node_id or node.key)})",
    )


def notify_help_open(prolog_engine: Any, path: str) -> None:
    _run_prolog_hook(
        prolog_engine,
        "org_browser_help_open",
        f"source({_prolog_string(path)})",
    )


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


def _node_source(node: OrgNode) -> str:
    return f"org-roam:{node.node_id or node.key}"


def _node_memory_text(node: OrgNode) -> str:
    stars = "*" * max(1, node.level)
    todo = f"{node.todo} " if node.todo else ""
    tags = f" :{':'.join(node.tags)}:" if node.tags else ""
    heading = f"{stars} {todo}{node.title}{tags}".rstrip()
    return heading if not node.body else f"{heading}\n{node.body}".strip()


def _metadata_tags(record: Optional[dict[str, Any]]) -> tuple[str, ...]:
    if not record:
        return ()
    metadata = record.get("metadata") or {}
    value = metadata.get("tags") if isinstance(metadata, dict) else None
    if isinstance(value, str):
        return tuple(tag.strip() for tag in value.split(",") if tag.strip())
    if isinstance(value, list):
        return tuple(str(tag).strip() for tag in value if str(tag).strip())
    return ()


def _run_prolog_hook(engine: Any, stage: str, event_term: str) -> None:
    if engine is None:
        return
    engine.query_once(f"zara_hooks:run_hook({stage}, {event_term})")


def _prolog_string(value: str) -> str:
    return json.dumps(str(value), ensure_ascii=False)


def _prolog_list(values: Sequence[str]) -> str:
    return "[{}]".format(
        ",".join(_prolog_string(value) for value in values)
    )


__all__ = [
    "ConfiguredOrgRoamMemoryHook",
    "ConfiguredOrgRoamWorkspace",
    "notify_help_open",
    "notify_node_selected",
    "render_org_document",
    "resolve_help_sources",
]
