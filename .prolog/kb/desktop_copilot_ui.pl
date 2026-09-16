desktop_conversation_renderer(canonical, 'zara/desktop/windows/copilot.py').
desktop_message_renderer('zara/desktop/chat_widgets.py', semantic_bubbles).
desktop_new_chat_invariant(empty_conversation, clears_rendered_message_widgets_immediately).
desktop_plugin_tool_projection(runtime_tool_event, generic_activity_bubble).
desktop_plugin_tool_invariant(tool_name, arbitrary_plugin_owned_name).
desktop_visual_fixture('zara/desktop/ui_fixtures.py', restores_qapplication_theme_state).
desktop_visual_environment(x11, display_8_supported_with_qt_xcb).
desktop_ui_regression_test('t/test_adaptive_copilot.py').
desktop_ui_regression_test('t/test_desktop_theme.py').
desktop_ui_regression_test('t/test_copilot_screenshot_fixtures.py').
