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
                %   app_search(AppAlias, QueryText)
                %
                % AppAlias is semantic data. Android owns package resolution.
                % Raw package names, Intents, shell commands and reflection are not actions.

                automation(youtube_psytrance,
                    actions([
                        app_search(youtube, 'psytrance')
                    ])).

                automation(revanced_psytrance,
                    actions([
                        app_search(youtube_revanced, 'psytrance')
                    ])).

                automation(psytrance_both,
                    actions([
                        app_search(youtube, 'psytrance'),
                        app_search(youtube_revanced, 'psytrance')
                    ])).

                automation(open_youtube,
                    actions([
                        open_app(youtube)
                    ])).

                automation(open_revanced,
                    actions([
                        open_app(youtube_revanced)
                    ])).
            """.trimIndent() + "\n",
            query = "automation(youtube_psytrance, Result)",
        ),
    )
}
