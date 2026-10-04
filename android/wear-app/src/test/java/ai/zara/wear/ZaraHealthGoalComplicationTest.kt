package ai.zara.wear

import ai.zara.ui.health.HealthGoalMetric
import ai.zara.ui.health.HealthGoalTarget
import ai.zara.wear.health.HealthGoalComplicationText
import org.junit.Assert.assertEquals
import org.junit.Test

class ZaraHealthGoalComplicationTest {
    @Test
    fun stepGoalUsesCompactWatchText() {
        assertEquals(
            "10K",
            HealthGoalComplicationText.short(HealthGoalTarget(HealthGoalMetric.STEPS, 10_000)),
        )
        assertEquals(
            "STEPS 10,000 steps",
            HealthGoalComplicationText.description(
                HealthGoalTarget(HealthGoalMetric.STEPS, 10_000),
            ),
        )
    }

    @Test
    fun sleepGoalUsesHourAndMinuteWatchText() {
        val goal = HealthGoalTarget(HealthGoalMetric.SLEEP, 510)

        assertEquals("8H30", HealthGoalComplicationText.short(goal))
        assertEquals("SLEEP 8 hours 30 minutes", HealthGoalComplicationText.description(goal))
    }
}
