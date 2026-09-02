from __future__ import annotations

import importlib.util
import uuid
from pathlib import Path
from types import ModuleType
from typing import Callable

from .hooks import AgentLoopAdviceRegistry, HookRegistration


_USER_HOOK_OWNER = "user:hooks.py"


class UserHookLoadError(RuntimeError):
    pass


class _UserHookRegistrationFacade:
    def __init__(self, registry: AgentLoopAdviceRegistry) -> None:
        self._registry = registry

    def before(self, callback: Callable, priority: int = 0) -> int:
        return self._registry.register("before", _USER_HOOK_OWNER, priority, callback)

    def after(self, callback: Callable, priority: int = 0) -> int:
        return self._registry.register("after", _USER_HOOK_OWNER, priority, callback)

    def around(self, callback: Callable, priority: int = 0) -> int:
        return self._registry.register("around", _USER_HOOK_OWNER, priority, callback)

    def override(self, callback: Callable, priority: int = 0) -> int:
        return self._registry.register("override", _USER_HOOK_OWNER, priority, callback)


class UserHookLoader:
    def __init__(self, *, config_dir: Path, registry: AgentLoopAdviceRegistry) -> None:
        self.config_dir = Path(config_dir)
        self.registry = registry

    @property
    def path(self) -> Path:
        return self.config_dir / "hooks.py"

    def load(self) -> int:
        return self._load_generation()

    def reload(self) -> int:
        return self._load_generation()

    def _load_generation(self) -> int:
        if not self.registry.enabled:
            return 0

        if not self.path.exists():
            self.registry.clear_owner(_USER_HOOK_OWNER)
            return 0

        staging = AgentLoopAdviceRegistry(
            enabled=self.registry.enabled,
            allow_override=self.registry.allow_override,
        )

        try:
            module = self._load_module()
            register = getattr(module, "register", None)
            if not callable(register):
                raise TypeError("register must be callable")
            register(_UserHookRegistrationFacade(staging))
        except Exception as error:
            raise UserHookLoadError(
                f"failed to load hooks.py ({type(error).__name__})"
            ) from error

        registrations = staging.list_registrations()
        self._replace_live_generation(registrations)
        return len(registrations)

    def _load_module(self) -> ModuleType:
        module_name = f"_zara_user_hooks_{uuid.uuid4().hex}"
        spec = importlib.util.spec_from_file_location(module_name, self.path)
        if spec is None or spec.loader is None:
            raise ImportError("unable to create user hook module spec")

        module = importlib.util.module_from_spec(spec)
        spec.loader.exec_module(module)
        return module

    def _replace_live_generation(
        self,
        registrations: tuple[HookRegistration, ...],
    ) -> None:
        self.registry.clear_owner(_USER_HOOK_OWNER)
        for registration in registrations:
            self.registry.register(
                registration.kind,
                _USER_HOOK_OWNER,
                registration.priority,
                registration.callback,
            )
