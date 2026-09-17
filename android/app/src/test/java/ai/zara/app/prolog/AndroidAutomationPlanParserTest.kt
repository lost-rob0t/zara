package ai.zara.app.prolog

import ai.zara.app.accessibility.AccessibilityGlobalAction
import ai.zara.app.accessibility.AccessibilitySelector
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidAutomationPlanParserTest {
    @Test
    fun `youtube psytrance plan parses into typed search`() {
        val plan = AndroidAutomationPlanParser.parse(
            "youtube_psytrance",
            "actions([app_search(youtube, 'psytrance')])",
        )

        assertEquals("youtube_psytrance", plan.name)
        assertEquals(
            listOf(AndroidAutomationAction.SearchApp("youtube", "psytrance")),
            plan.actions,
        )
    }

    @Test
    fun `revanced remains a semantic alias not a package id`() {
        val plan = AndroidAutomationPlanParser.parse(
            "revanced_psytrance",
            "actions([app_search(youtube_revanced, \"psytrance\")])",
        )

        assertEquals(
            listOf(AndroidAutomationAction.SearchApp("youtube_revanced", "psytrance")),
            plan.actions,
        )
        assertRejected("actions([open_app(app.revanced.android.youtube)])")
    }

    @Test
    fun `uri and accessibility grammar stays closed and typed`() {
        val plan = AndroidAutomationPlanParser.parse(
            "ui_demo",
            "actions([open_uri('https://example.com'), ui_click(text('Play')), " +
                "ui_set_text(view_id('com.example:id/query'), 'psytrance'), " +
                "ui_scroll_forward(description('Results')), global_action(back)])",
        )

        assertEquals(AndroidAutomationAction.OpenUri("https://example.com"), plan.actions[0])
        assertEquals(
            AndroidAutomationAction.UiClick(AccessibilitySelector.Text("Play")),
            plan.actions[1],
        )
        assertEquals(
            AndroidAutomationAction.UiSetText(
                AccessibilitySelector.ViewId("com.example:id/query"),
                "psytrance",
            ),
            plan.actions[2],
        )
        assertEquals(
            AndroidAutomationAction.UiScrollForward(AccessibilitySelector.Description("Results")),
            plan.actions[3],
        )
        assertEquals(
            AndroidAutomationAction.GlobalAction(AccessibilityGlobalAction.Back),
            plan.actions[4],
        )
    }

    @Test
    fun `unknown actions raw intents and unquoted data fail closed`() {
        assertRejected("actions([shell('id')])")
        assertRejected("actions([intent('android.intent.action.VIEW')])")
        assertRejected("actions([app_search(youtube, psytrance)])")
        assertRejected("actions([global_action(power_off)])")
    }

    @Test
    fun `plan action count is bounded`() {
        val actions = List(33) { "open_app(youtube)" }.joinToString(",")
        assertRejected("actions([$actions])")
    }

    private fun assertRejected(term: String) {
        val error = runCatching { AndroidAutomationPlanParser.parse("demo", term) }.exceptionOrNull()
        assertTrue("Expected plan to fail closed: $term", error is IllegalArgumentException)
    }
}
