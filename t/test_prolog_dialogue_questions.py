from __future__ import annotations

import pytest

from zara.prolog_engine import IntentResult, adapt_intent_result


class Compound:
    def __init__(self, name: str, args) -> None:
        self.name = name
        self.args = list(args)


class QuestionProlog:
    def __init__(self) -> None:
        self.resolve_calls: list[tuple[str, str]] = []
        self.answer_calls: list[tuple[str, str, str]] = []
        self.executed: list[tuple[str, list]] = []
        self.questions = {
            "theme": ("Which theme?", ("outrun", "oled")),
        }

    def resolve_intent(self, text: str, state: str = "passive"):
        self.resolve_calls.append((text, state))
        normalized = text.strip().casefold()
        if normalized == "set theme":
            return IntentResult("question", "theme", [])
        if normalized == "open firefox":
            return IntentResult("prolog", "open", ["firefox"])
        return None

    def get_question(self, question_id: str):
        return self.questions.get(question_id)

    def answer_question(self, question_id: str, answer: str, state: str = "passive"):
        self.answer_calls.append((question_id, answer, state))
        normalized = answer.strip().casefold()
        if question_id != "theme":
            return None
        if normalized in {"outrun", "oled"}:
            return IntentResult("prolog", "set_theme", [normalized])
        return None

    def execute_intent(self, name: str, args) -> bool:
        self.executed.append((name, list(args)))
        return True

    def get_app_mapping(self, target: str):
        return "firefox" if target == "firefox" else None


def build_router(prolog: QuestionProlog | None = None):
    from zara.runtime.intent_router import PrologFirstRouter

    return PrologFirstRouter(
        prolog or QuestionProlog(),
        wake_words=["zara"],
        conversation_id="conv-1",
    )


def test_question_functor_has_dedicated_result_kind():
    result = adapt_intent_result(
        {"Intent": Compound("question", ["theme"]), "Args": []}
    )

    assert result == IntentResult("question", "theme", [])


@pytest.mark.asyncio
async def test_normal_conversation_still_bypasses_prolog():
    prolog = QuestionProlog()
    router = build_router(prolog)

    decision = await router.route("I play guitar and like outrun aesthetics")

    assert decision.action == "delegate"
    assert prolog.resolve_calls == []
    assert prolog.answer_calls == []


@pytest.mark.asyncio
async def test_prolog_can_open_a_typed_question_and_resume_from_answer():
    prolog = QuestionProlog()
    router = build_router(prolog)

    question = await router.route("set theme")

    assert question.action == "respond"
    assert question.response == "Which theme?"
    assert question.input_request is not None
    assert question.input_request.kind == "prolog_question"
    assert question.input_request.question_id == "theme"
    assert question.input_request.prompt == "Which theme?"
    assert question.input_request.choices == ("outrun", "oled")

    answer = await router.route("outrun")

    assert answer.action == "respond"
    assert answer.input_request is None
    assert prolog.answer_calls == [("theme", "outrun", "passive")]
    assert prolog.executed == [("set_theme", ["outrun"])]


@pytest.mark.asyncio
async def test_invalid_answer_keeps_question_active_without_llm_hijack():
    prolog = QuestionProlog()
    router = build_router(prolog)

    await router.route("set theme")
    retry = await router.route("purple banana")

    assert retry.action == "respond"
    assert retry.response == "Which theme?"
    assert retry.input_request is not None
    assert retry.input_request.question_id == "theme"
    assert prolog.executed == []


@pytest.mark.asyncio
async def test_new_command_supersedes_prolog_question():
    prolog = QuestionProlog()
    router = build_router(prolog)

    await router.route("set theme")
    command = await router.route("open firefox")

    assert command.action == "respond"
    assert prolog.answer_calls == []
    assert prolog.executed == [("open", ["firefox"])]


@pytest.mark.asyncio
async def test_question_state_is_scoped_per_conversation():
    prolog = QuestionProlog()
    router = build_router(prolog)

    await router.route("set theme", conversation_id="conv-a")
    other = await router.route("outrun", conversation_id="conv-b")

    assert other.action == "delegate"
    assert prolog.answer_calls == []

    answer = await router.route("outrun", conversation_id="conv-a")

    assert answer.action == "respond"
    assert prolog.answer_calls == [("theme", "outrun", "passive")]
