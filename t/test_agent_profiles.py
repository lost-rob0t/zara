from langchain_core.tools import StructuredTool

from zara.agent.profiles import AgentProfileResolver
from zara.agent.tools.philosophy_tools import build_philosophy_tool
from zara.agent.tools.registry import ToolRegistry


class FakeProlog:
    def __init__(self, row):
        self.row = row
        self.goals = []

    def query_once(self, goal):
        self.goals.append(goal)
        return self.row


def _tool(name):
    return StructuredTool.from_function(
        func=lambda: name,
        name=name,
        description=name,
    )


def test_profile_resolver_strips_one_leading_mention_and_keeps_request():
    engine = FakeProlog(
        {
            "Id": "mara",
            "Display": "Mara",
            "Prompt": "Be direct.",
            "Tools": "all",
            "KBs": ["philosophy"],
            "Scope": "shared",
        }
    )
    resolver = AgentProfileResolver(engine)

    selected = resolver.resolve("@Mara compare stoicism and existentialism")

    assert selected is not None
    assert selected.profile.profile_id == "mara"
    assert selected.request == "compare stoicism and existentialism"
    assert selected.profile.kbs == ("philosophy",)
    assert selected.profile.tools is None
    assert "agent_profiles:resolve_mention" in engine.goals[0]


def test_profile_resolver_rejects_multiple_addressed_profiles():
    engine = FakeProlog(
        {
            "Id": "mara",
            "Display": "Mara",
            "Prompt": "Be direct.",
            "Tools": "all",
            "KBs": ["philosophy"],
            "Scope": "shared",
        }
    )
    resolver = AgentProfileResolver(engine)

    assert resolver.resolve("@Mara @ci-worker inspect this") is None
    assert engine.goals == []


def test_scoped_registry_enforces_profile_tool_allowlist():
    registry = ToolRegistry()
    registry.register_tools([_tool("read_file"), _tool("calculator"), _tool("query_prolog")])

    scoped = registry.scoped(("read_file", "query_prolog"))

    assert [tool.name for tool in scoped.to_langchain_tools()] == ["read_file", "query_prolog"]
    assert scoped.get_tool("calculator") is None
    assert scoped.requires_approval("calculator") is False


def test_philosophy_tool_uses_narrow_expert_predicate():
    engine = FakeProlog(
        {"Canonical": "virtue_ethics", "Summary": "Character matters."}
    )
    tool = build_philosophy_tool(engine)

    result = tool.invoke({"mode": "concept", "subject": "virtue ethics"})

    assert engine.goals == [
        'philosophy_expert:concept_summary("virtue ethics", Canonical, Summary)'
    ]
    assert "Canonical: virtue_ethics" in result


def test_philosophy_compare_requires_topic_and_second_philosopher():
    engine = FakeProlog({})
    tool = build_philosophy_tool(engine)

    result = tool.invoke({"mode": "compare", "subject": "kant"})

    assert "requires topic and other" in result
    assert engine.goals == []
