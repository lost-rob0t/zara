"""Typed, bounded access to Prolog-owned dialogue questions."""

from __future__ import annotations

from typing import Optional

from .prolog_engine import IntentResult, _prolog_atom, _prolog_string, adapt_intent_result

MAX_QUESTION_PROMPT_CHARS = 512
MAX_QUESTION_CHOICES = 16
MAX_QUESTION_CHOICE_CHARS = 128


def _text(value) -> Optional[str]:
    if isinstance(value, bytes):
        try:
            value = value.decode("utf-8")
        except UnicodeDecodeError:
            return None
    if isinstance(value, str):
        return value
    candidate = getattr(value, "value", None)
    return candidate if isinstance(candidate, str) else None


def _normalize_question(value) -> Optional[tuple[str, tuple[str, ...]]]:
    if not isinstance(value, (tuple, list)) or len(value) != 2:
        return None
    prompt = _text(value[0])
    raw_choices = value[1]
    if prompt is None or not prompt.strip() or len(prompt) > MAX_QUESTION_PROMPT_CHARS:
        return None
    if not isinstance(raw_choices, (tuple, list)):
        return None
    choices: list[str] = []
    for raw in raw_choices[:MAX_QUESTION_CHOICES]:
        choice = _text(raw)
        if choice is None or not choice.strip() or len(choice) > MAX_QUESTION_CHOICE_CHARS:
            return None
        choices.append(choice)
    return prompt, tuple(choices)


def get_question(prolog, question_id: str) -> Optional[tuple[str, tuple[str, ...]]]:
    """Return the registered prompt and bounded choices for QUESTION_ID."""
    custom = getattr(prolog, "get_question", None)
    if callable(custom):
        return _normalize_question(custom(question_id))

    row = prolog.query_once(
        f"dialogue:question({_prolog_atom(question_id)}, Prompt, Choices)"
    )
    if row is None:
        return None
    return _normalize_question((row.get("Prompt"), row.get("Choices", [])))


def answer_question(
    prolog,
    question_id: str,
    answer: str,
    *,
    state: str,
) -> Optional[IntentResult]:
    """Resolve ANSWER to a registered Prolog question into an IntentResult."""
    custom = getattr(prolog, "answer_question", None)
    if callable(custom):
        return custom(question_id, answer, state=state)

    row = prolog.query_once(
        "dialogue:answer({}, {}, {}, Intent, Args)".format(
            _prolog_atom(question_id),
            _prolog_string(answer),
            _prolog_atom(state),
        )
    )
    if row is None:
        return None
    return adapt_intent_result(row)
