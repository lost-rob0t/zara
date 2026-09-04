"""Principal-scoped orchestration for safe user-command authoring mutations."""

from __future__ import annotations

from dataclasses import dataclass, replace
from typing import Optional

from .user_command_compiler import (
    CompiledCommand,
    CompiledCommandRegistry,
    UserCommandCompiler,
)
from .user_commands import UserCommandDefinition, UserCommandStore


@dataclass(frozen=True)
class _UndoMutation:
    kind: str
    before: Optional[UserCommandDefinition]
    after: Optional[UserCommandDefinition]


class UserCommandAuthoringService:
    """Compile-before-commit mutations over one principal-bound command store."""

    def __init__(
        self,
        store: UserCommandStore,
        *,
        compiler: UserCommandCompiler,
        registry: CompiledCommandRegistry,
    ) -> None:
        if not isinstance(store, UserCommandStore):
            raise TypeError("store must be a UserCommandStore")
        if not isinstance(compiler, UserCommandCompiler):
            raise TypeError("compiler must be a UserCommandCompiler")
        if not isinstance(registry, CompiledCommandRegistry):
            raise TypeError("registry must be a CompiledCommandRegistry")
        self._store = store
        self._compiler = compiler
        self._registry = registry
        self._undo: Optional[_UndoMutation] = None

    def preview(self, definition: UserCommandDefinition) -> CompiledCommand:
        """Validate and normalize one draft without persisting or publishing it."""

        return self._compiler.compile(definition)

    def list_commands(self) -> list[UserCommandDefinition]:
        return self._store.list()

    def describe(self, command_id: str) -> Optional[UserCommandDefinition]:
        return self._store.get(command_id)

    def create(self, definition: UserCommandDefinition) -> UserCommandDefinition:
        current = self._store.list()
        self._validate_candidate((*current, definition))
        saved = self._store.create(definition)
        try:
            self._publish_store()
        except Exception:
            self._store.delete(saved.command_id, expected_revision=saved.revision)
            self._publish_store()
            raise
        self._undo = _UndoMutation("create", before=None, after=saved)
        return saved

    def edit(
        self,
        definition: UserCommandDefinition,
        *,
        expected_revision: int,
    ) -> UserCommandDefinition:
        before = self._store.get(definition.command_id)
        if before is None:
            raise KeyError(definition.command_id)
        candidate = tuple(
            definition if item.command_id == definition.command_id else item
            for item in self._store.list()
        )
        self._validate_candidate(candidate)
        saved = self._store.update(definition, expected_revision=expected_revision)
        try:
            self._publish_store()
        except Exception:
            self._store.update(before, expected_revision=saved.revision)
            self._publish_store()
            raise
        self._undo = _UndoMutation("edit", before=before, after=saved)
        return saved

    def delete(self, command_id: str, *, expected_revision: int) -> None:
        before = self._store.get(command_id)
        if before is None:
            raise KeyError(command_id)
        candidate = tuple(
            item for item in self._store.list() if item.command_id != before.command_id
        )
        self._validate_candidate(candidate)
        self._store.delete(before.command_id, expected_revision=expected_revision)
        try:
            self._publish_store()
        except Exception:
            restored = self._store.create(_as_new_definition(before))
            self._publish_store()
            self._undo = None
            raise
        self._undo = _UndoMutation("delete", before=before, after=None)

    def undo(self) -> Optional[UserCommandDefinition]:
        mutation = self._undo
        if mutation is None:
            return None
        self._undo = None

        if mutation.kind == "create":
            created = mutation.after
            if created is None:
                raise RuntimeError("invalid create undo state")
            self._store.delete(created.command_id, expected_revision=created.revision)
            self._publish_store()
            return None

        if mutation.kind == "edit":
            before = mutation.before
            after = mutation.after
            if before is None or after is None:
                raise RuntimeError("invalid edit undo state")
            restored = self._store.update(before, expected_revision=after.revision)
            self._publish_store()
            return restored

        if mutation.kind == "delete":
            before = mutation.before
            if before is None:
                raise RuntimeError("invalid delete undo state")
            restored = self._store.create(_as_new_definition(before))
            self._publish_store()
            return restored

        raise RuntimeError(f"unsupported undo mutation: {mutation.kind}")

    def _validate_candidate(self, definitions: tuple[UserCommandDefinition, ...]) -> None:
        candidate_registry = CompiledCommandRegistry(self._compiler)
        candidate_registry.replace_all(definitions)

    def _publish_store(self) -> None:
        self._registry.replace_all(tuple(self._store.list()))


def _as_new_definition(definition: UserCommandDefinition) -> UserCommandDefinition:
    return replace(
        definition,
        owner_principal_id=None,
        revision=0,
        created_at=None,
        updated_at=None,
    )


__all__ = ["UserCommandAuthoringService"]
