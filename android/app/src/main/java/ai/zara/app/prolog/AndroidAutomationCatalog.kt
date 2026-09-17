package ai.zara.app.prolog

object AndroidAutomationCatalog {
    val examples = listOf(
        PrologExample(
            title = "Android automation",
            summary = "Deterministic typed Android actions selected by Prolog.",
            fileName = "android_automation.pl",
            source = """
                % Zara Android automation API v1.
                %
                % Public contract:
                %   automation(+Name, -Result).
                % Result must be actions(List).
                %
                % Closed action syntax in v1:
                %   open_app(AppAlias)
                %   open_uri("https://example.com")
                %   app_search(AppAlias, "query text")
                %   ui_click(text("Visible label"))
                %   ui_click(view_id("package:id/view"))
                %   ui_click(description("Accessibility description"))
                %   ui_set_text(Selector, "text")
                %   ui_scroll_forward(Selector)
                %   global_action(back|home|recents|notifications)
                %
                % Text is represented as a Prolog string so Result round-trips preserve type.
                % AppAlias is semantic data. Android owns package resolution.
                % UI/global actions require the user-enabled Zara AccessibilityService.
                % Raw package names, Intents, shell commands and reflection are not actions.

                automation(open_bixby,
                    actions([
                        open_app(bixby)
                    ])).

                automation(youtube_psytrance,
                    actions([
                        app_search(youtube, "psytrance")
                    ])).

                automation(revanced_psytrance,
                    actions([
                        app_search(youtube_revanced, "psytrance")
                    ])).

                automation(psytrance_both,
                    actions([
                        app_search(youtube, "psytrance"),
                        app_search(youtube_revanced, "psytrance")
                    ])).

                automation(open_youtube,
                    actions([
                        open_app(youtube)
                    ])).

                automation(open_revanced,
                    actions([
                        open_app(youtube_revanced)
                    ])).

                automation(accessibility_demo,
                    actions([
                        ui_click(text("Search")),
                        ui_set_text(view_id("com.example:id/query"), "psytrance")
                    ])).
            """.trimIndent() + "\n",
            query = "automation(youtube_psytrance, Result)",
        ),
    )
}