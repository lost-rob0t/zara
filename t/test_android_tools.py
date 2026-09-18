from zara.android_tools import (
    build_android_device_tools,
    load_android_tool_descriptors,
)


class FakePrologEngine:
    def query_iter(self, goal, max_solutions):
        assert max_solutions > 0
        if "android_tool_descriptor" in goal:
            return iter(
                [
                    {
                        "Name": "open_app",
                        "Wire": "open_app",
                        "Effect": "navigation",
                        "Authority": "standard",
                        "Permission": "none",
                        "Description": "Open a reviewed app alias.",
                    },
                    {
                        "Name": "app_search",
                        "Wire": "app_search",
                        "Effect": "navigation",
                        "Authority": "standard",
                        "Permission": "none",
                        "Description": "Search a reviewed app alias.",
                    },
                ]
            )
        if "android_tool_argument" in goal:
            return iter(
                [
                    {
                        "Name": "open_app",
                        "Arg": "app",
                        "Type": "string",
                        "Required": "required",
                        "MaxBytes": 128,
                    },
                    {
                        "Name": "app_search",
                        "Arg": "app",
                        "Type": "string",
                        "Required": "required",
                        "MaxBytes": 128,
                    },
                    {
                        "Name": "app_search",
                        "Arg": "query",
                        "Type": "string",
                        "Required": "required",
                        "MaxBytes": 512,
                    },
                ]
            )
        raise AssertionError(goal)


def test_catalog_is_loaded_from_prolog_without_python_tool_name_copy():
    descriptors = load_android_tool_descriptors(FakePrologEngine())

    assert [item.name for item in descriptors] == ["open_app", "app_search"]
    assert [arg.name for arg in descriptors[1].arguments] == ["app", "query"]


def test_tool_projection_exposes_only_live_device_capabilities():
    calls = []

    tools = build_android_device_tools(
        FakePrologEngine(),
        lambda capability, args: calls.append((capability, args)) or {"outcome": "completed"},
        {"app_search"},
    )

    assert [tool.name for tool in tools] == ["android_app_search"]
    assert tools[0].metadata["zara_android_capability"] == "app_search"
    assert tools[0].metadata["zara_requires_approval"] is False

    result = tools[0].invoke({"app": "youtube", "query": "psytrance"})
    assert result == {"outcome": "completed"}
    assert calls == [
        ("app_search", {"app": "youtube", "query": "psytrance"})
    ]


def test_projection_rejects_oversized_utf8_before_device_dispatch():
    calls = []
    tool = build_android_device_tools(
        FakePrologEngine(),
        lambda capability, args: calls.append((capability, args)),
        {"app_search"},
    )[0]

    try:
        tool.invoke({"app": "youtube", "query": "🔥" * 129})
    except ValueError as error:
        assert "byte limit" in str(error)
    else:
        raise AssertionError("oversized query must fail closed")

    assert calls == []
