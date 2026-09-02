from pathlib import Path

import pytest

from zara.agent.hooks import AgentLoopAdviceRegistry
from zara.agent.user_hooks import UserHookLoadError, UserHookLoader


def _loader(tmp_path: Path, *, enabled: bool = True, allow_override: bool = False):
    registry = AgentLoopAdviceRegistry(
        enabled=enabled,
        allow_override=allow_override,
    )
    return UserHookLoader(config_dir=tmp_path, registry=registry), registry


def test_user_hooks_path_is_canonical_and_missing_file_is_not_created(tmp_path):
    loader, registry = _loader(tmp_path)

    assert loader.path == tmp_path / "hooks.py"
    assert loader.load() == 0
    assert not loader.path.exists()
    assert registry.list_registrations() == ()


def test_disabled_hooks_never_execute_user_python(tmp_path):
    loader, registry = _loader(tmp_path, enabled=False)
    loader.path.write_text("raise RuntimeError('must not execute')\n")

    assert loader.load() == 0
    assert registry.list_registrations() == ()


def test_user_file_registers_with_core_owned_identity(tmp_path):
    loader, registry = _loader(tmp_path)
    loader.path.write_text(
        "def register(hooks):\n"
        "    async def before_turn(*args, **kwargs):\n"
        "        return None\n"
        "    hooks.before(before_turn, priority=7)\n"
    )

    assert loader.load() == 1
    registrations = registry.list_registrations()
    assert len(registrations) == 1
    assert registrations[0].kind == "before"
    assert registrations[0].owner == "user:hooks.py"
    assert registrations[0].priority == 7


def test_reload_replaces_previous_user_generation(tmp_path):
    loader, registry = _loader(tmp_path)
    loader.path.write_text(
        "def register(hooks):\n"
        "    def before_turn(*args, **kwargs):\n"
        "        return None\n"
        "    hooks.before(before_turn, priority=1)\n"
    )
    assert loader.load() == 1

    loader.path.write_text(
        "def register(hooks):\n"
        "    def after_turn(result):\n"
        "        return None\n"
        "    hooks.after(after_turn, priority=9)\n"
    )
    assert loader.reload() == 1

    registrations = registry.list_registrations()
    assert [(item.kind, item.owner, item.priority) for item in registrations] == [
        ("after", "user:hooks.py", 9)
    ]


def test_failed_reload_preserves_last_working_generation(tmp_path):
    loader, registry = _loader(tmp_path)
    loader.path.write_text(
        "def register(hooks):\n"
        "    def before_turn(*args, **kwargs):\n"
        "        return None\n"
        "    hooks.before(before_turn)\n"
    )
    assert loader.load() == 1
    before = registry.list_registrations()

    loader.path.write_text("def register(:\n")
    with pytest.raises(UserHookLoadError, match="hooks.py"):
        loader.reload()

    assert registry.list_registrations() == before


def test_user_cannot_forge_owner_through_registration_facade(tmp_path):
    loader, registry = _loader(tmp_path)
    loader.path.write_text(
        "def register(hooks):\n"
        "    def callback(*args, **kwargs):\n"
        "        return None\n"
        "    hooks.before(callback, owner='forged')\n"
    )

    with pytest.raises(UserHookLoadError):
        loader.load()
    assert registry.list_registrations() == ()


def test_override_still_requires_existing_registry_policy(tmp_path):
    loader, registry = _loader(tmp_path, enabled=True, allow_override=False)
    loader.path.write_text(
        "def register(hooks):\n"
        "    def replacement(*args, **kwargs):\n"
        "        return None\n"
        "    hooks.override(replacement)\n"
    )

    with pytest.raises(UserHookLoadError):
        loader.load()
    assert registry.list_registrations() == ()
