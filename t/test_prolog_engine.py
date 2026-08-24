import logging
import threading
from unittest.mock import MagicMock

import pytest

from zara.prolog_engine import (
    PrologEngine,
    PrologQueryError,
    PrologSerializationError,
    PrologStartupError,
    TimerLifecycleRecord,
)


class FakeQuery:
    def __init__(self, values=(), error=None, close_error=None):
        self.values = iter(values)
        self.error = error
        self.close_error = close_error
        self.closed = False

    def __iter__(self):
        return self

    def __next__(self):
        if self.error is not None:
            raise self.error
        return next(self.values)

    def close(self):
        self.closed = True
        if self.close_error is not None:
            raise self.close_error


def build_engine(query):
    engine = PrologEngine.__new__(PrologEngine)
    engine.prolog = MagicMock()
    engine.prolog.query.return_value = query
    engine.logger = logging.getLogger(__name__)
    engine.loaded_files = set()
    engine._principal_id = None
    return engine


def test_missing_program_raises_typed_startup_error(tmp_path):
    with pytest.raises(PrologStartupError, match="not found"):
        PrologEngine(tmp_path / "missing.pl")


def test_no_solution_is_distinct_from_query_error():
    no_solution = build_engine(FakeQuery())
    assert no_solution.query_once("fail") is None

    failed = build_engine(FakeQuery(error=RuntimeError("engine broke")))
    with pytest.raises(PrologQueryError, match="engine broke"):
        failed.query_once("broken_goal")


def test_query_iterator_closes_after_partial_consumption():
    query = FakeQuery([{"Value": 1}, {"Value": 2}])
    results = build_engine(query).query_iter("values(Value)")

    assert next(results) == {"Value": 1}
    results.close()

    assert query.closed is True


def test_query_once_closes_underlying_query():
    query = FakeQuery([{"Value": 1}, {"Value": 2}])

    assert build_engine(query).query_once("values(Value)") == {"Value": 1}
    assert query.closed is True


def test_query_close_failure_is_typed():
    query = FakeQuery([{"Value": 1}], close_error=RuntimeError("close broke"))

    with pytest.raises(PrologQueryError, match="close broke"):
        build_engine(query).query_once("values(Value)")


def test_user_text_is_serialized_as_one_string_term():
    engine = build_engine(FakeQuery())
    malicious = 'quote " slash \\ newline\n), throw(injected), ('

    assert engine.resolve_intent(malicious) is None

    goal = engine.prolog.query.call_args.args[0]
    assert goal.startswith('intent_resolver:resolve("')
    assert '\\"' in goal
    assert "\\\\" in goal
    assert "\\n" in goal
    assert goal.endswith(", passive, Intent, Args)")


def test_intent_arguments_are_serialized_without_changing_goal_shape():
    engine = build_engine(FakeQuery([{}]))

    assert engine.execute_intent("text", ["sam", "hi'), throw(injected), ('"])

    goal = engine.prolog.query.call_args.args[0]
    assert goal == (
        "commands:execute('text', ['sam', "
        "'hi\\'), throw(injected), (\\''])"
    )


def test_unsupported_values_fail_before_querying():
    engine = build_engine(FakeQuery([{}]))

    with pytest.raises(PrologSerializationError, match="Unsupported"):
        engine.execute_intent("open", [object()])

    engine.prolog.query.assert_not_called()


def test_lazy_query_serializes_concurrent_runtime_access():
    first_query = FakeQuery([{"Value": 1}, {"Value": 2}])
    first_engine = build_engine(first_query)
    second_engine = build_engine(FakeQuery([{}]))
    results = first_engine.query_iter("values(Value)")
    assert next(results) == {"Value": 1}

    completed = threading.Event()
    started = threading.Event()

    def run_second_query():
        started.set()
        second_engine.query_once("true")
        completed.set()

    thread = threading.Thread(target=run_second_query)
    thread.start()
    assert started.wait(1)
    assert completed.wait(0.05) is False

    results.close()
    thread.join(timeout=1)

    assert completed.is_set()
    assert first_query.closed is True


def test_schedule_values_use_quoted_atoms():
    engine = build_engine(FakeQuery([{}]))

    assert engine.schedule_has_no_overlap(
        "2026-07-19T01:00",
        ["2026-07-19T02:00", "quote'\\\nvalue"],
    )

    goal = engine.prolog.query.call_args.args[0]
    assert goal.startswith("todo_schedule:no_overlap('2026-07-19T01:00', [")
    assert "quote\\'\\\\\\nvalue" in goal


def test_principal_scopes_public_queries_inside_prolog_boundary():
    engine = build_engine(FakeQuery())
    engine._principal_id = "uid:1000"

    assert engine.resolve_intent("timer 10 seconds") is None

    assert engine.prolog.query.call_args.args[0] == (
        "alarm:with_principal('uid:1000', "
        "(intent_resolver:resolve(\"timer 10 seconds\", passive, Intent, Args)))"
    )


def test_execute_intent_captures_prolog_response_text():
    engine = build_engine(FakeQuery([{"Response": b"Timer set.\n"}]))
    engine._principal_id = "uid:1000"

    assert engine.execute_intent_with_response("timer", [10, "tea"]) == "Timer set.\n"

    assert engine.prolog.query.call_args.args[0] == (
        "alarm:with_principal('uid:1000', "
        "(with_output_to(string(Response), "
        "commands:execute('timer', [10, 'tea']))))"
    )


def test_timer_lifecycle_json_is_typed_and_principal_scoped():
    engine = build_engine(FakeQuery([{
        "Json": (
            b'[{"type":"scheduled","timer_id":"timer-1","name":"tea",'
            b'"created_at_ns":100,"due_at_ns":200,"revision":1},'
            b'{"type":"fired","timer_id":"timer-1","name":"tea",'
            b'"created_at_ns":100,"due_at_ns":200,"fired_at_ns":220,'
            b'"revision":2,"message":"Timer tea finished."}]'
        )
    }]))
    engine._principal_id = "uid:1000"

    assert engine.drain_timer_events() == [
        TimerLifecycleRecord(
            kind="scheduled",
            timer_id="timer-1",
            name="tea",
            created_at_ns=100,
            due_at_ns=200,
            revision=1,
        ),
        TimerLifecycleRecord(
            kind="fired",
            timer_id="timer-1",
            name="tea",
            created_at_ns=100,
            due_at_ns=200,
            revision=2,
            fired_at_ns=220,
            message="Timer tea finished.",
        ),
    ]
    assert engine.prolog.query.call_args.args[0] == (
        "alarm:with_principal('uid:1000', "
        "(alarm:drain_timer_events_json('uid:1000', Json)))"
    )


def test_malformed_timer_lifecycle_is_rejected():
    engine = build_engine(FakeQuery([{
        "Json": b'[{"type":"fired","timer_id":"timer-1","name":"tea"}]'
    }]))
    engine._principal_id = "uid:1000"

    with pytest.raises(PrologSerializationError, match="timer lifecycle"):
        engine.drain_timer_events()
