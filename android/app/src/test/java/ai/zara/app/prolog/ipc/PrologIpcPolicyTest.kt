package ai.zara.app.prolog.ipc

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PrologIpcPolicyTest {
    @Test
    fun requestIdsGoalsAndDeadlinesAreBounded() {
        assertEquals("abc-123", PrologIpcPolicy.requireRequestId("abc-123"))
        assertEquals("member(Result,[a,b])", PrologIpcPolicy.requireGoal(" member(Result,[a,b]) "))

        assertThrows(IllegalArgumentException::class.java) {
            PrologIpcPolicy.requireRequestId("")
        }
        assertThrows(IllegalArgumentException::class.java) {
            PrologIpcPolicy.requireRequestId("../escape")
        }
        assertThrows(IllegalArgumentException::class.java) {
            PrologIpcPolicy.requireGoal("x".repeat(PrologIpcPolicy.MAX_GOAL_CHARS + 1))
        }

        val now = 10_000L
        assertEquals(15_000L, PrologIpcPolicy.requireDeadline(15_000L, now))
        assertThrows(IllegalArgumentException::class.java) {
            PrologIpcPolicy.requireDeadline(now, now)
        }
        assertThrows(IllegalArgumentException::class.java) {
            PrologIpcPolicy.requireDeadline(now + PrologIpcPolicy.MAX_TIMEOUT_MS + 1, now)
        }
    }

    @Test
    fun resultProjectionIsBounded() {
        val projected = PrologIpcPolicy.projectTerms(
            listOf("a", "x".repeat(PrologIpcPolicy.MAX_TERM_CHARS + 100)) +
                List(PrologIpcPolicy.MAX_TERMS + 10) { "term_$it" }
        )

        assertEquals(PrologIpcPolicy.MAX_TERM_CHARS, projected.terms[1].length)
        assert(projected.terms.size <= PrologIpcPolicy.MAX_TERMS)
        assert(projected.truncated)
    }
}
