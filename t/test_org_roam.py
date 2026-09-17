from __future__ import annotations

import pytest

from zara.agent.hooks import AgentLoopAdviceRegistry
from zara.memory import MemoryManager
from zara.org_roam import (
    OrgRoamIndex,
    OrgRoamMemoryHook,
    heading_font_pt,
    parse_org_text,
    render_org_html,
)


ALPHA = """#+title: Alpha Project
#+filetags: :project:zara:
:PROPERTIES:
:ID: alpha-file
:END:

* TODO Build renderer :ui:org:
:PROPERTIES:
:ID: alpha-renderer
:PROJECT: zara
:ROAM_ALIASES: "Org renderer" "Doom view"
:END:
Keep literal stars while changing heading font size.
Link to [[id:beta-memory][memory design]].

** NEXT Add backlinks :graph:
:PROPERTIES:
:ID: alpha-backlinks
:END:
Show incoming links and project context.

#+begin_src prolog
org_memory(alpha_renderer, zara).
#+end_src
"""


BETA = """#+title: Memory Design
:PROPERTIES:
:ID: beta-file
:END:

* Facts and memory
:PROPERTIES:
:ID: beta-memory
:PROJECT: zara
:END:
Symbolic memories, project context, facts, and recent chats.
Back to [[id:alpha-renderer][the renderer]].
"""


def _local_memory(monkeypatch) -> MemoryManager:
    import zara.memory as memory_module

    monkeypatch.setattr(memory_module, "_CHROMADB_AVAILABLE", False)
    return MemoryManager(enabled=True, persist_directory=None)


def test_org_parser_preserves_roam_identity_structure_and_links():
    document = parse_org_text(ALPHA, path="/notes/alpha.org")

    assert document.title == "Alpha Project"
    assert document.file_node is not None
    assert document.file_node.node_id == "alpha-file"
    assert document.file_node.tags == ("project", "zara")

    renderer = next(node for node in document.nodes if node.node_id == "alpha-renderer")
    assert renderer.level == 1
    assert renderer.todo == "TODO"
    assert renderer.title == "Build renderer"
    assert renderer.tags == ("ui", "org")
    assert renderer.properties["PROJECT"] == "zara"
    assert renderer.aliases == ("Org renderer", "Doom view")
    assert renderer.links[0].kind == "id"
    assert renderer.links[0].target == "beta-memory"

    child = next(node for node in document.nodes if node.node_id == "alpha-backlinks")
    assert child.parent_key == renderer.key
    assert "incoming links" in child.body


def test_org_roam_index_resolves_backlinks_neighbors_search_and_duplicates():
    alpha = parse_org_text(ALPHA, path="/notes/alpha.org")
    beta = parse_org_text(BETA, path="/notes/beta.org")
    index = OrgRoamIndex.from_documents((alpha, beta))

    backlinks = index.backlinks("beta-memory")
    assert [node.node_id for node in backlinks] == ["alpha-renderer"]

    neighbors = {node.node_id for node in index.neighbors("alpha-renderer")}
    assert "beta-memory" in neighbors
    assert "alpha-backlinks" in neighbors

    results = index.search("Doom renderer", limit=5)
    assert results[0].node_id == "alpha-renderer"

    duplicate = parse_org_text(
        "#+title: Duplicate\n* Other\n:PROPERTIES:\n:ID: beta-memory\n:END:\n",
        path="/notes/duplicate.org",
    )
    duplicate_index = OrgRoamIndex.from_documents((alpha, beta, duplicate))
    assert duplicate_index.duplicate_ids == ("beta-memory",)


def test_doom_renderer_keeps_stars_but_scales_heading_fonts():
    document = parse_org_text(ALPHA, path="/notes/alpha.org")
    rendered = render_org_html(document, base_font_pt=12.0)

    assert "*" in rendered
    assert "**" in rendered
    assert "Build renderer" in rendered
    assert "[[id:beta-memory][memory design]]" in rendered
    assert "org_memory(alpha_renderer, zara)." in rendered
    assert heading_font_pt(1, 12.0) > heading_font_pt(2, 12.0)
    assert heading_font_pt(2, 12.0) > heading_font_pt(5, 12.0)
    assert heading_font_pt(8, 12.0) == 12.0


def test_org_roam_memory_hook_projects_files_without_becoming_source_of_truth(monkeypatch):
    memory = _local_memory(monkeypatch)
    index = OrgRoamIndex.from_documents(
        (
            parse_org_text(ALPHA, path="/notes/alpha.org"),
            parse_org_text(BETA, path="/notes/beta.org"),
        )
    )
    hook = OrgRoamMemoryHook(memory)

    report = hook.sync_index(index)
    assert report.created >= 4
    assert report.deleted == 0

    records = memory.list_memories(limit=50, include_kinds=("fact",))
    renderer = next(
        record
        for record in records
        if record["metadata"].get("source") == "org-roam:alpha-renderer"
    )
    assert "Keep literal stars" in renderer["text"]
    assert "org-roam" in renderer["metadata"]["tags"]
    assert "symbolic-memory" in renderer["metadata"]["tags"]
    assert "project:zara" in renderer["metadata"]["tags"]

    same = hook.sync_index(index)
    assert same.created == 0
    assert same.updated == 0
    assert same.deleted == 0


def test_org_roam_memory_hook_replaces_changed_projection_and_removes_deleted_nodes(monkeypatch):
    memory = _local_memory(monkeypatch)
    hook = OrgRoamMemoryHook(memory)
    original = OrgRoamIndex.from_documents((parse_org_text(ALPHA, path="/notes/alpha.org"),))
    hook.sync_index(original)

    changed_text = ALPHA.replace(
        "Keep literal stars while changing heading font size.",
        "Keep literal stars and use Doom-sized headings.",
    ).replace(
        "** NEXT Add backlinks :graph:\n:PROPERTIES:\n:ID: alpha-backlinks\n:END:\nShow incoming links and project context.\n\n",
        "",
    )
    changed = OrgRoamIndex.from_documents((parse_org_text(changed_text, path="/notes/alpha.org"),))
    report = hook.sync_index(changed)

    assert report.updated >= 1
    assert report.deleted >= 1
    records = memory.list_memories(limit=50, include_kinds=("fact",))
    assert not any(
        record["metadata"].get("source") == "org-roam:alpha-backlinks"
        for record in records
    )
    renderer = next(
        record
        for record in records
        if record["metadata"].get("source") == "org-roam:alpha-renderer"
    )
    assert "Doom-sized headings" in renderer["text"]
    assert "changing heading font size" not in renderer["text"]


def test_context_bundle_separates_symbolic_project_facts_and_recent_chats(monkeypatch):
    memory = _local_memory(monkeypatch)
    hook = OrgRoamMemoryHook(memory)
    index = OrgRoamIndex.from_documents((parse_org_text(ALPHA, path="/notes/alpha.org"),))
    hook.sync_index(index)
    memory.remember_fact("The desktop renderer is Qt.", tags=["desktop"], source="test")
    memory.store_session_summary("chat-1", "We discussed the renderer yesterday.", source="desktop")

    bundle = hook.context_bundle("renderer", project="zara", limit=8, recent_chat_limit=3)

    assert any("Build renderer" in item["text"] for item in bundle.symbolic_memories)
    assert any("Build renderer" in item["text"] for item in bundle.project_context)
    assert any("desktop renderer is Qt" in item["text"] for item in bundle.facts)
    assert [item["text"] for item in bundle.recent_chats] == [
        "We discussed the renderer yesterday."
    ]


@pytest.mark.asyncio
async def test_python_hook_uses_existing_agent_advice_registry(monkeypatch):
    memory = _local_memory(monkeypatch)
    index = OrgRoamIndex.from_documents((parse_org_text(ALPHA, path="/notes/alpha.org"),))
    hook = OrgRoamMemoryHook(memory)
    registry = AgentLoopAdviceRegistry(enabled=True, allow_override=False)
    registry.register(
        "after",
        "org-roam-memory",
        20,
        hook.python_after_hook(index=index, node_ids=("alpha-renderer",), project="zara"),
    )

    async def base():
        return {"response": "done"}

    result = await registry.invoke(base)

    assert result == {"response": "done"}
    assert any(
        record["metadata"].get("source") == "org-roam:alpha-renderer"
        for record in memory.list_memories(limit=20, include_kinds=("fact",))
    )
