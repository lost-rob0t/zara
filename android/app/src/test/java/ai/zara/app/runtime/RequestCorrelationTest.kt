package ai.zara.app.runtime

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class RequestCorrelationTest {

    @Test fun `current session reply completes exactly one pending request`() {
        val pending = RequestCorrelations(limit = 2)
        pending.register("req-1", generation = 4, sessionId = "session-4")

        assertEquals(
            CorrelationResult.Accepted,
            pending.complete("req-1", generation = 4, sessionId = "session-4"),
        )
        assertEquals(
            CorrelationResult.Unknown,
            pending.complete("req-1", generation = 4, sessionId = "session-4"),
        )
    }

    @Test fun `stale generation or session cannot consume current request`() {
        val pending = RequestCorrelations(limit = 2)
        pending.register("req-1", generation = 4, sessionId = "session-4")

        assertEquals(
            CorrelationResult.Stale,
            pending.complete("req-1", generation = 3, sessionId = "session-3"),
        )
        assertEquals(1, pending.size)
        assertEquals(
            CorrelationResult.Stale,
            pending.complete("req-1", generation = 4, sessionId = "other-session"),
        )
        assertEquals(1, pending.size)
    }

    @Test fun `pending table is bounded and duplicate ids fail closed`() {
        val pending = RequestCorrelations(limit = 2)
        pending.register("req-1", 1, "s1")
        assertThrows(IllegalArgumentException::class.java) {
            pending.register("req-1", 1, "s1")
        }
        pending.register("req-2", 1, "s1")
        assertThrows(IllegalStateException::class.java) {
            pending.register("req-3", 1, "s1")
        }
    }

    @Test fun `reconnect drops all old generation pending requests`() {
        val pending = RequestCorrelations(limit = 4)
        pending.register("req-1", 1, "s1")
        pending.register("req-2", 1, "s1")
        pending.register("req-3", 2, "s2")

        assertEquals(2, pending.dropGeneration(1))
        assertEquals(1, pending.size)
        assertEquals(CorrelationResult.Accepted, pending.complete("req-3", 2, "s2"))
    }
}
