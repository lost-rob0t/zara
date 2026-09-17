from __future__ import annotations

import json
from pathlib import Path
from types import SimpleNamespace

from zara.org_browser import (
    OrgBrowserConfig,
    OrgBrowserHookRegistry,
    build_org_browser_runtime,
)


class FakeConfig:
    def __init__(self, config_dir: Path):
        self.config_dir = config_dir

    def get_section(self, name: str):
        if name == "org":
            return {
                "enabled": True,
                "roots": ["~/toml-roam"],
                "default_project": "toml-project",
                "memory_sync": True,
                "base_font_pt": 13.0,
                "max_files": 1000,
                "max_file_bytes": 1500000,
                "search_limit": 50,
                "show_backlinks": True,
                "show_properties": True,
                "help_sources": ["README.org", "docs/README.org"],
                "heading_scales": [1.40, 1.25, 1.15, 1.08, 1.03],
                "python_config_enabled": True,
            }
        if name == "hooks":
            return {"enabled": True, "allow_override": False}
        return {}


class FakeProlog:
    def query_all(self, goal: str, max_solutions: int = 100):
        if goal == "kb_config:org_browser_setting(Key, Value)":
            return [
                {"Key": "base_font_pt", "Value": 15.0},
                {"Key": "search_limit", "Value": 25},
                {"Key": "show_backlinks", "Value": "false"},
                {"Key": "default_project", "Value": "prolog-project"},
            ]
        if goal == "kb_config:org_browser_root(Path)":
            return [{"Path": "~/prolog-roam"}]
        if goal == "kb_config:org_browser_heading_scale(Level, Scale)":
            return [{"Level": 1, "Scale": 1.8}, {"Level": 3, "Scale": 1.3}]
        if goal == "kb_config:org_browser_help_source(Path)":
            return [{"Path": "wiki/customization.org"}]
        return []


def test_config_validation_rejects_invalid_values():
    config = OrgBrowserConfig()

    try:
        config.with_setting("base_font_pt", 0)
        assert False, "zero font size must fail"
    except ValueError:
        pass

    try:
        config.with_setting("search_limit", -1)
        assert False, "negative search limit must fail"
    except ValueError:
        pass

    try:
        config.with_setting("unknown", 1)
        assert False, "unknown settings must fail"
    except KeyError:
        pass


def test_resolution_precedence_is_defaults_then_toml_then_prolog_then_python(tmp_path):
    user_python = tmp_path / "org_browser.py"
    user_python.write_text(
        """
def configure(org):
    org.set("base_font_pt", 17.0)
    org.set("show_properties", False)
    org.add_root("~/python-roam")
    org.heading_scale(2, 1.6)
    org.help_source("wiki/android.org")
""".strip()
    )

    runtime = build_org_browser_runtime(FakeConfig(tmp_path), prolog_engine=FakeProlog())
    config = runtime.config

    assert config.base_font_pt == 17.0
    assert config.search_limit == 25
    assert config.show_backlinks is False
    assert config.show_properties is False
    assert config.default_project == "prolog-project"
    assert config.roots == ("~/prolog-roam", "~/python-roam")
    assert config.heading_scale(1) == 1.8
    assert config.heading_scale(2) == 1.6
    assert config.heading_scale(3) == 1.3
    assert config.help_sources == ("wiki/customization.org", "wiki/android.org")


def test_native_process_reads_owner_local_prolog_snapshot_before_python(tmp_path, monkeypatch):
    snapshot = tmp_path / "org-browser-prolog.json"
    snapshot.write_text(
        json.dumps(
            {
                "version": 1,
                "settings": {
                    "base_font_pt": 16.0,
                    "search_limit": 24,
                    "show_backlinks": False,
                    "default_project": "snapshot-project",
                },
                "roots": ["~/snapshot-roam"],
                "heading_scales": [{"level": 1, "scale": 1.9}],
                "help_sources": ["wiki/org-workspace.org"],
            }
        )
    )
    monkeypatch.setenv("ZARA_ORG_BROWSER_PROLOG_SNAPSHOT", str(snapshot))
    (tmp_path / "org_browser.py").write_text(
        "def configure(org):\n    org.set('base_font_pt', 18.0)\n"
    )

    runtime = build_org_browser_runtime(FakeConfig(tmp_path), prolog_engine=None)

    assert runtime.config.base_font_pt == 18.0
    assert runtime.config.search_limit == 24
    assert runtime.config.show_backlinks is False
    assert runtime.config.default_project == "snapshot-project"
    assert runtime.config.roots == ("~/snapshot-roam",)
    assert runtime.config.heading_scale(1) == 1.9
    assert runtime.config.help_sources == ("wiki/org-workspace.org",)


def test_python_customization_registers_filter_sort_render_help_and_memory_hooks(tmp_path):
    (tmp_path / "org_browser.py").write_text(
        """
def configure(org):
    org.filter_node(lambda node, config: "private" not in node.tags)
    org.sort_nodes(lambda nodes, config: tuple(reversed(nodes)))
    org.render_document(lambda document, rendered, config: rendered + "<!-- python -->")
    org.filter_help_sources(lambda paths, config: [path for path in paths if "private" not in path])
    org.memory_tags(lambda node, tags, config: [*tags, "python-hook"])
    org.memory_text(lambda node, text, config: text + "\\nPYTHON-HOOK")
""".strip()
    )

    runtime = build_org_browser_runtime(FakeConfig(tmp_path), prolog_engine=None)
    hooks = runtime.hooks
    config = runtime.config
    public = SimpleNamespace(tags=("org",), title="Public")
    private = SimpleNamespace(tags=("private",), title="Private")

    assert hooks.filter_nodes((public, private), config) == (public,)
    assert hooks.sort_nodes((public, private), config) == (private, public)
    assert hooks.render_document(object(), "<html/>", config).endswith("<!-- python -->")
    assert hooks.help_sources(("README.org", "private/secret.org"), config) == ("README.org",)
    assert hooks.memory_tags(public, ("org-roam",), config) == ("org-roam", "python-hook")
    assert hooks.memory_text(public, "text", config) == "text\nPYTHON-HOOK"


def test_python_loader_is_disabled_when_global_hooks_are_disabled(tmp_path):
    class DisabledHooksConfig(FakeConfig):
        def get_section(self, name: str):
            if name == "hooks":
                return {"enabled": False, "allow_override": False}
            return super().get_section(name)

    (tmp_path / "org_browser.py").write_text(
        "def configure(org):\n    org.set('base_font_pt', 99.0)\n"
    )

    runtime = build_org_browser_runtime(DisabledHooksConfig(tmp_path), prolog_engine=None)
    assert runtime.config.base_font_pt == 13.0
    assert runtime.hooks.diagnostics() == ()


def test_hook_registry_is_priority_ordered_and_owner_clearable():
    registry = OrgBrowserHookRegistry()
    seen = []
    registry.register("render_document", "late", 20, lambda document, value, config: seen.append("late") or value)
    registry.register("render_document", "early", 10, lambda document, value, config: seen.append("early") or value)

    value = registry.render_document(object(), "x", OrgBrowserConfig())
    assert value == "x"
    assert seen == ["early", "late"]
    assert registry.clear_owner("early") == 1
    assert [item.owner for item in registry.diagnostics()] == ["late"]
