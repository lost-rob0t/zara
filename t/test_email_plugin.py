from pathlib import Path

import pytest
from langchain_core.messages import HumanMessage

from zara.plugins.builtin.email_mail import (
    EmailServicePlugin,
    MailAccount,
    PROLOG_API_CONTEXT,
    SpamFeedCompiler,
)


def test_mail_account_requires_supported_provider():
    with pytest.raises(ValueError):
        MailAccount.from_mapping({"id": "x", "provider": "smtp"})


def test_email_tools_expose_prolog_symbols_and_gate_mutations():
    plugin = EmailServicePlugin()
    tools = {tool.name: tool for tool in plugin.tools()}

    assert "email_spam_rule" in tools["email_prolog_api"].description
    assert "email_before_send_rule" in tools["email_send"].description
    assert tools["email_send"].metadata["zara_requires_approval"] is True
    assert tools["email_reply"].metadata["zara_requires_approval"] is True
    assert tools["email_apply_rules"].metadata["zara_requires_approval"] is True
    assert tools["email_search"].metadata.get("zara_requires_approval") is None


def test_model_context_injection_is_idempotent():
    state = {"messages": [HumanMessage(content="mail?")]}

    EmailServicePlugin._inject_model_context(None, None, state)
    EmailServicePlugin._inject_model_context(None, None, state)

    contexts = [message for message in state["messages"] if getattr(message, "id", None) == "zara-email-prolog-api"]
    assert len(contexts) == 1
    assert PROLOG_API_CONTEXT in contexts[0].content


def test_feed_compiler_rejects_non_https_before_network(tmp_path: Path):
    compiler = SpamFeedCompiler(tmp_path / "rules.pl")
    with pytest.raises(ValueError, match="HTTPS"):
        compiler.refresh([{"name": "bad", "url": "http://example.test/list", "kind": "domain"}])


def test_feed_compiler_rejects_executable_feed_kind(tmp_path: Path):
    compiler = SpamFeedCompiler(tmp_path / "rules.pl")
    with pytest.raises(ValueError, match="unsupported"):
        compiler.refresh([{"name": "bad", "url": "https://example.test/list", "kind": "prolog"}])
