from __future__ import annotations

from pathlib import Path

import pytest
from PySide6.QtGui import QTextDocument

from zara.desktop.prolog_studio import (
    FACT_TYPES,
    ManagedFactStore,
    PrologHighlighter,
    PrologSourceRepository,
    PrologStudioError,
)


def test_fact_store_round_trips_every_loader_supported_fact_and_preserves_manual_source(tmp_path):
    path = tmp_path / "config.pl"
    manual = "% hand written\ndirect_app(manual_tool).\n"
    path.write_text(manual, encoding="utf-8")
    store = ManagedFactStore(path)

    created = [
        store.add("app_mapping", {"name": "studio", "argv": ["code", "--new-window"]}),
        store.add("direct_app", {"name": "wireshark"}),
        store.add("search_engine", {"template": "https://example.test/?q=%s"}),
        store.add("dictation_command", {"argv": ["zara-dictate", "small", "cpu"]}),
        store.add("timer_sound", {"value": "disabled"}),
        store.add("alarm_sound", {"value": "/tmp/alarm.wav"}),
        store.add("llm_provider", {"value": "ollama"}),
        store.add("llm_model", {"value": "qwen3"}),
        store.add("llm_endpoint", {"value": "http://127.0.0.1:11434/api/chat"}),
        store.add("todo_destination", {"value": "~/notes/todo.org"}),
        store.add("todo_context_mode", {"value": "infer_with_llm"}),
        store.add("verb_intent", {"phrase": "summon studio", "intent": "open", "arity": 1}),
    ]

    assert tuple(fact.kind for fact in created) == FACT_TYPES
    loaded = store.list()
    assert [(fact.id, fact.kind, fact.fields) for fact in loaded] == [
        (fact.id, fact.kind, fact.fields) for fact in created
    ]
    text = path.read_text(encoding="utf-8")
    assert text.startswith(manual)
    assert 'app_mapping(studio, ["code", "--new-window"]).' in text
    assert "verb_intent(summon_studio, open, 1)." in text
    assert text.count("% BEGIN ZARA MANAGED FACTS") == 1


def test_fact_store_edit_delete_and_validation_are_atomic(tmp_path):
    path = tmp_path / "config.pl"
    path.write_text("% manual\n", encoding="utf-8")
    store = ManagedFactStore(path)
    fact = store.add("llm_model", {"value": "one"})

    updated = store.update(fact.id, "llm_model", {"value": 'two "quoted"'})
    assert updated.id == fact.id
    assert store.list()[0].fields["value"] == 'two "quoted"'
    assert '\\"quoted\\"' in path.read_text(encoding="utf-8")

    before = path.read_bytes()
    with pytest.raises(PrologStudioError, match="app name"):
        store.add("app_mapping", {"name": "Bad;Name", "argv": ["code"]})
    assert path.read_bytes() == before

    store.delete(fact.id)
    assert store.list() == []
    assert path.read_text(encoding="utf-8").startswith("% manual\n")


def test_source_repository_is_allowlisted_bounded_and_atomic(tmp_path):
    root = tmp_path / "repo"
    (root / "kb").mkdir(parents=True)
    (root / "modules").mkdir()
    (root / "main.pl").write_text("main :- true.\n", encoding="utf-8")
    (root / "kb" / "intents.pl").write_text("intent(ok).\n", encoding="utf-8")
    (root / "modules" / "logic.pl").write_text("logic(X) :- X = ok.\n", encoding="utf-8")
    user_config = tmp_path / "xdg" / "config.pl"
    user_config.parent.mkdir()
    user_config.write_text("direct_app(code).\n", encoding="utf-8")
    repository = PrologSourceRepository(root, user_config, max_bytes=128)

    ids = [entry.id for entry in repository.list()]
    assert ids == ["user-config", "main.pl", "kb/intents.pl", "modules/logic.pl"]
    assert repository.read("kb/intents.pl") == "intent(ok).\n"
    repository.write("user-config", "direct_app(studio).\n", validator=lambda _path: None)
    assert user_config.read_text(encoding="utf-8") == "direct_app(studio).\n"

    with pytest.raises(PrologStudioError, match="approved"):
        repository.read("../secret")
    with pytest.raises(PrologStudioError, match="maximum"):
        repository.write("user-config", "x" * 129, validator=lambda _path: None)

    outside = tmp_path / "outside.pl"
    outside.write_text("outside.\n", encoding="utf-8")
    link = root / "modules" / "linked.pl"
    link.symlink_to(outside)
    repository = PrologSourceRepository(root, user_config, max_bytes=128)
    assert "modules/linked.pl" not in [entry.id for entry in repository.list()]


def test_source_repository_does_not_replace_file_when_validation_fails(tmp_path):
    root = tmp_path / "repo"
    (root / "kb").mkdir(parents=True)
    (root / "modules").mkdir()
    (root / "main.pl").write_text("main.\n", encoding="utf-8")
    user_config = tmp_path / "config.pl"
    user_config.write_text("before.\n", encoding="utf-8")
    repository = PrologSourceRepository(root, user_config)

    def reject(_path: Path) -> None:
        raise PrologStudioError("syntax error on line 1")

    with pytest.raises(PrologStudioError, match="syntax error"):
        repository.write("user-config", "broken(.", validator=reject)
    assert user_config.read_text(encoding="utf-8") == "before.\n"


def test_prolog_highlighter_formats_comments_directives_strings_variables_and_predicates():
    document = QTextDocument()
    highlighter = PrologHighlighter(document)
    document.setPlainText('% note\n:- module(test, []).\nroute(User, "studio") :- User = ready.\n')
    highlighter.rehighlight()

    categories = set()
    block = document.firstBlock()
    while block.isValid():
        for text_format in block.layout().formats():
            category = text_format.format.property(PrologHighlighter.CATEGORY_PROPERTY)
            if category:
                categories.add(category)
        block = block.next()
    assert {"comment", "directive", "string", "variable", "predicate"} <= categories
    assert highlighter.document() is document
