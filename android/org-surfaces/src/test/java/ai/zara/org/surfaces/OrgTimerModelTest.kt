package ai.zara.org.surfaces

import java.time.Duration
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgTimerModelTest {
    private fun timer(vararg overrides: Pair<String, Any>): OrgTimerRun {
        var remaining = Duration.ofMinutes(25)
        var running = false
        for ((key, value) in overrides) {
            when (key) {
                "remaining" -> remaining = value as Duration
                "running" -> running = value as Boolean
            }
        }
        return OrgTimerRun(key = "k", name = "write", total = Duration.ofMinutes(25), remaining = remaining, running = running)
    }

    @Test fun `ticking a paused timer changes nothing`() {
        assertEquals(timer(), timer().tick())
    }

    @Test fun `ticking a running timer counts down one second`() {
        val ticked = timer("remaining" to Duration.ofSeconds(5), "running" to true).tick()

        assertEquals(Duration.ofSeconds(4), ticked.remaining)
        assertTrue(ticked.running)
    }

    @Test fun `ticking stops at zero and finishes the run`() {
        val ticked = timer("remaining" to Duration.ofSeconds(1), "running" to true).tick()

        assertEquals(Duration.ZERO, ticked.remaining)
        assertFalse(ticked.running)
        assertTrue(ticked.finished)
    }

    @Test fun `start refuses to run a finished timer and reset restores the total`() {
        val finished = timer("remaining" to Duration.ZERO, "running" to false)

        assertFalse(finished.start().running)
        assertEquals(timer(), finished.reset())
    }

    @Test fun `formats hours minutes and seconds without padding hours`() {
        assertEquals("25:00", OrgTimerFormat.format(Duration.ofMinutes(25)))
        assertEquals("1:00:05", OrgTimerFormat.format(Duration.ofHours(1).plusSeconds(5)))
        assertEquals("00:00", OrgTimerFormat.format(Duration.ZERO))
    }
}
