package ai.zara.app.prolog

import ai.zara.app.device.DeviceActionArguments
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class CalendarAutomationContractTest {
    @Test
    fun `parser emits one closed typed calendar insert action`() {
        val plan = AndroidAutomationPlanParser.parse(
            "planning_event",
            """actions([calendar_insert("Planning", 1795093200000, 1795096800000, "Room 7", "Review roadmap")])""",
        )

        assertEquals(
            listOf(
                AndroidAutomationAction.CalendarInsert(
                    DeviceActionArguments.CalendarInsert(
                        title = "Planning",
                        startMillis = 1_795_093_200_000,
                        endMillis = 1_795_096_800_000,
                        location = "Room 7",
                        description = "Review roadmap",
                    )
                )
            ),
            plan.actions,
        )
    }

    @Test
    fun `optional calendar text uses explicit none atom`() {
        val plan = AndroidAutomationPlanParser.parse(
            "planning_event",
            """actions([calendar_insert("Planning", 1795093200000, 1795096800000, none, none)])""",
        )
        val action = plan.actions.single() as AndroidAutomationAction.CalendarInsert

        assertEquals(null, action.arguments.location)
        assertEquals(null, action.arguments.description)
    }

    @Test
    fun `calendar automation rejects raw android authority and malformed integers`() {
        listOf(
            """actions([calendar_insert("Planning", startActivity, 1795096800000, none, none)])""",
            """actions([calendar_insert("Planning", 1795093200000, 1795096800000, package("evil.app"), none)])""",
            """actions([calendar_insert("Planning", 1795093200000, 1795096800000, none, intent("android.intent.action.DELETE"))])""",
        ).forEach { term ->
            assertThrows(IllegalArgumentException::class.java) {
                AndroidAutomationPlanParser.parse("planning_event", term)
            }
        }
    }

    @Test
    fun `catalog ships calendar automation source without raw Intents`() {
        val source = AndroidAutomationCatalog.examples.single {
            it.fileName == "zara_android_calendar.pl"
        }.source

        assertTrue(source.contains("calendar_insert("))
        assertTrue(source.contains("automation(calendar_planning_demo,"))
        assertTrue(!source.contains("android.intent"))
        assertTrue(!source.contains("package("))
    }
}
