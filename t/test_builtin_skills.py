from __future__ import annotations

from pathlib import Path

from zara.agent.skills import SkillRegistry


BUILTIN_SKILLS = {
    "agent-mode",
    "command-routing",
    "configuration",
    "desktop-app-control",
    "file-operations",
    "mcp-servers",
    "memory-operations",
    "plugin-development",
    "shell-execution",
    "todos-reminders",
    "tool-approvals",
    "voice-tts",
}


def _registry() -> SkillRegistry:
    root = Path(__file__).resolve().parents[1] / "skills"
    registry = SkillRegistry([root])
    registry.discover()
    return registry


def test_builtin_operational_skill_catalog_is_discoverable():
    registry = _registry()

    assert BUILTIN_SKILLS <= {skill.name for skill in registry.skills}


def test_builtin_operational_skills_are_selected_for_matching_requests():
    registry = _registry()

    cases = {
        "Connect an MCP server and inspect its resources": "mcp-servers",
        "Remember this fact in Zara memory": "memory-operations",
        "Use the file tools to read a file": "file-operations",
        "Run this in bash from the shell": "shell-execution",
        "Schedule a recurring background task": "agent-mode",
        "Speak this aloud with TTS and keep barge-in working": "voice-tts",
        "Add a one-time todo reminder": "todos-reminders",
        "Launch an application on the desktop": "desktop-app-control",
        "Explain why this tool needs approval": "tool-approvals",
        "Build a Zara service plugin with lifecycle hooks": "plugin-development",
        "Route this deterministic command through Prolog": "command-routing",
        "Change Zara config.toml under XDG config": "configuration",
    }

    for prompt, expected in cases.items():
        selection = registry.select(prompt, max_tokens=12000)
        names = {skill.name for skill in selection.skills}
        assert expected in names, (prompt, expected, sorted(names))
