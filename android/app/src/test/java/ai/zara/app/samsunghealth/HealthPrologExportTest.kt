package ai.zara.app.samsunghealth

import ai.zara.ui.health.HealthGoalMetric
import ai.zara.ui.health.HealthGoalTarget
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class HealthPrologExportTest {
    @Test
    fun rendersStrictDataOnlyPrologForCurrentReadingsAndGoals() {
        val payload = HealthPrologExport.render(
            readings = mapOf(
                SamsungHealthMetric.STEPS to SamsungHealthReading(
                    SamsungHealthMetric.STEPS,
                    mapOf("steps" to "4321", "invalid_text" to "private notes"),
                ),
            ),
            goals = listOf(HealthGoalTarget(HealthGoalMetric.STEPS, 10_000)),
            epochMs = 1_780_000_000_000,
        ).toString(Charsets.UTF_8)

        assertTrue(payload.startsWith("health_db_version(1).\n"))
        assertTrue(payload.contains("health_observation("))
        assertTrue(payload.contains(",steps,"))
        assertTrue(payload.contains("health_goal("))
        assertFalse(payload.contains("private notes"))
        assertFalse(payload.contains(":-"))
    }
}
