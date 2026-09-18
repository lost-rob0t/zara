package ai.zara.org.timer

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class TimerRuntimeContractTest {
    @Test
    fun runningTimerCarriesDurableTerminalState() {
        val timer = RunningOrgTimer(
            id = 42,
            name = "Ramen",
            sourceKey = "timers.org:1:Ramen:180",
            finishEpochMs = 1_800_000L,
            remainingMs = 180_000L,
            paused = false,
        )

        assertEquals("Ramen", timer.name)
        assertTrue(timer.finishEpochMs > timer.remainingMs)
    }
}
