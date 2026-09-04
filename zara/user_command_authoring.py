"""Principal-scoped orchestration for safe user-command authoring mutations."""

from __future__ import annotations

import re
from dataclasses import dataclass, replace
from typing import Callable, Optional

from .runtime.clarification import (
    CANCEL_PHRASES,
    ClarificationCoordinator,
    DialogueTemplate,
    SessionCloseReason,
    SlotSpec,
    SlotType,
)
from .runtime.frames import BoolValue, TextValue
from .user_command_compiler import (
    CompiledCommand,
    CompiledCommandRegistry,
    UserCommandCompiler,
)
from .user_commands import SemanticAction, UserCommandDefinition, UserCommandStore

_CREATE_TEMPLATE = DialogueTemplate(
    intent_ns="user_command",
    intent_name="create_command",
    specs=(
        SlotSpec("trigger", SlotType.TEXT, prompt="What should you say?"),
        SlotSpec("actions", SlotType.TEXT, prompt="What should it do?"),
        SlotSpec("confirm", SlotType.BOOLEAN, prompt="Save it?"),
    ),
)
_EDIT_TEMPLATE = DialogueTemplate(
    intent_ns="user_command",
    intent_name="edit_command",
    specs=(
        SlotSpec("actions", SlotType.TEXT, prompt="What should it do instead?"),
        SlotSpec("confirm", SlotType.BOOLEAN, prompt="Save this edit?"),
    ),
)
_COMMAND_ID_PARTS = re.compile(r"[^a-z0-9]+")
_NOT_FOUND_MESSAGE = "I couldn't find that command."


@dataclass(frozen=True)
class AuthoringDialogueResult:
    kind: str
    message: str
    definition: Optional[UserCommandDefinition] = None


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
            self._store.create(_as_new_definition(before))
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


class UserCommandAuthoringDialogue:
    """User-command dialogue projected through shared clarification state."""

    def __init__(
        self,
        service: UserCommandAuthoringService,
        *,
        clarifications: ClarificationCoordinator,
        action_resolver: Callable[[str], tuple[SemanticAction, ...]],
        principal_id: str,
        conversation_id: str,
    ) -> None:
        if not isinstance(service, UserCommandAuthoringService):
            raise TypeError("service must be a UserCommandAuthoringService")
        if not isinstance(clarifications, ClarificationCoordinator):
            raise TypeError("clarifications must be a ClarificationCoordinator")
        if not callable(action_resolver):
            raise TypeError("action_resolver must be callable")
        if not isinstance(principal_id, str) or not principal_id.strip():
            raise ValueError("principal_id must be non-empty")
        if not isinstance(conversation_id, str) or not conversation_id.strip():
            raise ValueError("conversation_id must be non-empty")
        self._service = service
        self._clarifications = clarifications
        self._action_resolver = action_resolver
        self._principal_id = principal_id
        self._conversation_id = conversation_id
        self._target: Optional[UserCommandDefinition] = None

    def list_commands(self) -> AuthoringDialogueResult:
        commands = self._service.list_commands()
        if not commands:
            return AuthoringDialogueResult("list", "No user commands are defined.")
        names = ", ".join(command.trigger for command in commands)
        return AuthoringDialogueResult("list", f"User commands: {names}.")

    def describe(self, command_id: str) -> AuthoringDialogueResult:
        definition = self._service.describe(command_id)
        if definition is None:
            return _not_found()
        preview = self._service.preview(definition)
        return AuthoringDialogueResult("description", _format_preview(preview), definition)

    def test(self, command_id: str) -> AuthoringDialogueResult:
        definition = self._service.describe(command_id)
        if definition is None:
            return _not_found()
        preview = self._service.preview(definition)
        return AuthoringDialogueResult(
            "preview",
            f"{_format_preview(preview)} Dry run only; nothing executed.",
            definition,
        )

    def start_create(self) -> AuthoringDialogueResult:
        self._target = None
        opened = self._clarifications.open(
            _CREATE_TEMPLATE,
            principal=self._principal_id,
            conversation_id=self._conversation_id,
        )
        if opened.kind != "opened":
            return AuthoringDialogueResult(opened.kind, opened.message)
        return AuthoringDialogueResult("question", opened.question)

    def start_edit(self, command_id: str) -> AuthoringDialogueResult:
        target = self._service.describe(command_id)
        if target is None:
            return _not_found()
        self._target = target
        opened = self._clarifications.open(
            _EDIT_TEMPLATE,
            principal=self._principal_id,
            conversation_id=self._conversation_id,
        )
        if opened.kind != "opened":
            self._target = None
            return AuthoringDialogueResult(opened.kind, opened.message)
        return AuthoringDialogueResult(
            "question", f"What should {target.trigger} do instead?", target
        )

    def start_delete(self, command_id: str) -> AuthoringDialogueResult:
        target = self._service.describe(command_id)
        if target is None:
            return _not_found()
        self._target = target
        template = DialogueTemplate(
            intent_ns="user_command",
            intent_name="delete_command",
            specs=(
                SlotSpec(
                    "confirm",
                    SlotType.BOOLEAN,
                    prompt=f'Delete command "{target.trigger}"?',
                ),
            ),
        )
        opened = self._clarifications.open(
            template,
            principal=self._principal_id,
            conversation_id=self._conversation_id,
        )
        if opened.kind != "opened":
            self._target = None
            return AuthoringDialogueResult(opened.kind, opened.message)
        return AuthoringDialogueResult("question", opened.question, target)

    def undo_last(self) -> AuthoringDialogueResult:
        restored = self._service.undo()
        if restored is None:
            return AuthoringDialogueResult("undone", "Undid the last command change.")
        return AuthoringDialogueResult(
            "undone",
            f'Restored command "{restored.trigger}".',
            restored,
        )

    def submit(self, text: str) -> AuthoringDialogueResult:
        session = self._clarifications.session_for(
            self._principal_id,
            self._conversation_id,
        )
        if session is None or session.state.value == "closed":
            return AuthoringDialogueResult("stale", self._clarifications.STALE_MESSAGE)

        normalized = " ".join(text.split()).casefold()
        if normalized in CANCEL_PHRASES:
            outcome = self._submit_follow_up(text)
            self._target = None
            return AuthoringDialogueResult(
                outcome.kind,
                outcome.message or outcome.question or "",
            )

        if session.template.intent_name == "edit_command":
            return self._submit_edit(text, session)
        if session.template.intent_name == "delete_command":
            return self._submit_delete(text)
        return self._submit_create(text, session)

    def _submit_create(self, text: str, session) -> AuthoringDialogueResult:
        active = session.active_spec()
        resolved_actions: Optional[tuple[SemanticAction, ...]] = None
        if active is not None and active.name == "actions":
            resolved_actions = self._resolve_actions_or_none(text)
            if not resolved_actions:
                trigger = _text_slot(session, "trigger") or "that command"
                return AuthoringDialogueResult(
                    "question",
                    f"I couldn't map that to a known action. What should {trigger} do?",
                )

        outcome = self._submit_follow_up(text)
        if outcome.kind in {"cancelled", "retry", "stale"}:
            return AuthoringDialogueResult(
                outcome.kind,
                outcome.message or outcome.question or "",
            )
        session = outcome.session
        if session is None:
            return AuthoringDialogueResult("stale", self._clarifications.STALE_MESSAGE)

        trigger = _text_slot(session, "trigger")
        if outcome.kind == "filled" and session.active_spec() is not None:
            next_spec = session.active_spec()
            if next_spec.name == "actions" and trigger is not None:
                return AuthoringDialogueResult("question", f"What should {trigger} do?")
            if next_spec.name == "confirm":
                definition = self._definition_from_create_session(session, resolved_actions)
                preview = self._service.preview(definition)
                return AuthoringDialogueResult(
                    "preview",
                    f"{_format_preview(preview)} Save it?",
                    definition,
                )
            return AuthoringDialogueResult("question", outcome.question or "")

        if outcome.kind != "complete":
            return AuthoringDialogueResult(outcome.kind, outcome.message or "")
        if not _confirmed(session):
            return self._cancel_current()

        definition = self._definition_from_create_session(session)
        saved = self._service.create(definition)
        self._finish_current()
        return AuthoringDialogueResult(
            "created",
            f'Created command "{saved.trigger}".',
            saved,
        )

    def _submit_edit(self, text: str, session) -> AuthoringDialogueResult:
        target = self._target
        if target is None:
            return AuthoringDialogueResult("stale", self._clarifications.STALE_MESSAGE)

        active = session.active_spec()
        resolved_actions: Optional[tuple[SemanticAction, ...]] = None
        if active is not None and active.name == "actions":
            resolved_actions = self._resolve_actions_or_none(text)
            if not resolved_actions:
                return AuthoringDialogueResult(
                    "question", f"What should {target.trigger} do instead?"
                )

        outcome = self._submit_follow_up(text)
        if outcome.kind in {"cancelled", "retry", "stale"}:
            if outcome.kind == "cancelled":
                self._target = None
            return AuthoringDialogueResult(
                outcome.kind,
                outcome.message or outcome.question or "",
            )
        session = outcome.session
        if session is None:
            return AuthoringDialogueResult("stale", self._clarifications.STALE_MESSAGE)

        if outcome.kind == "filled" and session.active_spec() is not None:
            if session.active_spec().name == "confirm":
                actions = resolved_actions or self._actions_from_session(session)
                definition = target.with_updates(actions=actions)
                preview = self._service.preview(definition)
                return AuthoringDialogueResult(
                    "preview",
                    f"{_format_preview(preview)} Save this edit?",
                    definition,
                )
            return AuthoringDialogueResult("question", outcome.question or "")

        if outcome.kind != "complete":
            return AuthoringDialogueResult(outcome.kind, outcome.message or "")
        if not _confirmed(session):
            return self._cancel_current()

        definition = target.with_updates(actions=self._actions_from_session(session))
        saved = self._service.edit(definition, expected_revision=target.revision)
        self._finish_current()
        return AuthoringDialogueResult(
            "edited", f'Edited command "{saved.trigger}".', saved
        )

    def _submit_delete(self, text: str) -> AuthoringDialogueResult:
        target = self._target
        if target is None:
            return AuthoringDialogueResult("stale", self._clarifications.STALE_MESSAGE)
        outcome = self._submit_follow_up(text)
        if outcome.kind in {"cancelled", "retry", "stale"}:
            if outcome.kind == "cancelled":
                self._target = None
            return AuthoringDialogueResult(
                outcome.kind,
                outcome.message or outcome.question or "",
            )
        session = outcome.session
        if session is None:
            return AuthoringDialogueResult("stale", self._clarifications.STALE_MESSAGE)
        if outcome.kind != "complete":
            return AuthoringDialogueResult(outcome.kind, outcome.message or "")
        if not _confirmed(session):
            return self._cancel_current()

        self._service.delete(target.command_id, expected_revision=target.revision)
        self._finish_current()
        return AuthoringDialogueResult(
            "deleted", f'Deleted command "{target.trigger}".', target
        )

    def _definition_from_create_session(
        self,
        session,
        actions: Optional[tuple[SemanticAction, ...]] = None,
    ) -> UserCommandDefinition:
        trigger = _text_slot(session, "trigger")
        action_text = _text_slot(session, "actions")
        if trigger is None or action_text is None:
            raise RuntimeError("create dialogue is missing required semantic fields")
        resolved = actions if actions is not None else tuple(self._action_resolver(action_text))
        if not resolved:
            raise ValueError("action resolver returned no semantic actions")
        return UserCommandDefinition(
            command_id=_command_id_for_trigger(trigger),
            trigger=trigger,
            actions=resolved,
        )

    def _actions_from_session(self, session) -> tuple[SemanticAction, ...]:
        action_text = _text_slot(session, "actions")
        if action_text is None:
            raise RuntimeError("edit dialogue is missing action text")
        actions = tuple(self._action_resolver(action_text))
        if not actions:
            raise ValueError("action resolver returned no semantic actions")
        return actions

    def _resolve_actions_or_none(self, text: str) -> Optional[tuple[SemanticAction, ...]]:
        try:
            actions = tuple(self._action_resolver(text))
        except (TypeError, ValueError):
            return None
        return actions or None

    def _submit_follow_up(self, text: str):
        return self._clarifications.submit_follow_up(
            text,
            principal=self._principal_id,
            conversation_id=self._conversation_id,
        )

    def _cancel_current(self) -> AuthoringDialogueResult:
        self._clarifications.cancel(
            principal=self._principal_id,
            conversation_id=self._conversation_id,
            reason=SessionCloseReason.CANCELLED,
        )
        self._target = None
        return AuthoringDialogueResult("cancelled", "Cancelled.")

    def _finish_current(self) -> None:
        self._clarifications.finish(
            principal=self._principal_id,
            conversation_id=self._conversation_id,
        )
        self._target = None


def _not_found() -> AuthoringDialogueResult:
    return AuthoringDialogueResult("not_found", _NOT_FOUND_MESSAGE)


def _confirmed(session) -> bool:
    value = session.frame.slot_value("confirm")
    return isinstance(value, BoolValue) and value.value


def _text_slot(session, name: str) -> Optional[str]:
    value = session.frame.slot_value(name)
    return value.text if isinstance(value, TextValue) else None


def _command_id_for_trigger(trigger: str) -> str:
    normalized = _COMMAND_ID_PARTS.sub("-", trigger.casefold()).strip("-")
    if not normalized:
        raise ValueError("trigger cannot produce an empty command id")
    return normalized[:64].rstrip("-")


def _format_preview(command: CompiledCommand) -> str:
    actions: list[str] = []
    for action in command.actions:
        values = []
        for name in action.contract.arguments:
            value = action.arguments.get(name)
            if isinstance(value, TextValue):
                values.append(value.text)
            elif value is not None:
                values.append(str(value))
            elif name in action.slot_bindings:
                values.append(f"{{{action.slot_bindings[name]}}}")
        actions.append(f"{action.contract.action_id}({', '.join(values)})")
    return f'Command "{command.trigger}": ' + ", ".join(actions) + "."


def _as_new_definition(definition: UserCommandDefinition) -> UserCommandDefinition:
    return replace(
        definition,
        owner_principal_id=None,
        revision=0,
        created_at=None,
        updated_at=None,
    )


__all__ = [
    "AuthoringDialogueResult",
    "UserCommandAuthoringDialogue",
    "UserCommandAuthoringService",
]
