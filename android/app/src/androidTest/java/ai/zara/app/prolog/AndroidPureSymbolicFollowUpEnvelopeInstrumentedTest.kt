package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession
import ai.zara.app.runtime.LocalServerPhase
import android.content.Context
import android.os.SystemClock
import androidx.test.platform.app.InstrumentationRegistry
import java.util.concurrent.TimeUnit
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * Pins the exact native Trealla boundary used by a durable clarification follow-up.
 *
 * The first query intentionally obtains Context1 through the same JNI Result-binding path used by
 * production. That serialized term is then fed straight back through the canonical persisted
 * Context0 parser for the second turn. This catches portability defects that an in-memory Prolog
 * term or a one-turn envelope cannot expose.
 */
class AndroidPureSymbolicFollowUpEnvelopeInstrumentedTest {
    private lateinit var context: Context
    private lateinit var session: AndroidAppSession

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        session = AndroidAppSession(context)
        awaitLocalServerReady()
    }

    @After
    fun tearDown() {
        session.close()
    }

    @Test
    fun serializedClarificationContextReentersNativeTreallaForFollowUp() {
        val firstQuery = AndroidPureSymbolicConversationFactory.dialogueTurnEnvelopeQuery(
            utterance = "timer",
            contextTerm = SymbolicDialogueContextCodec.emptyContextTerm,
        )
        val first = session.queryLocalProlog(firstQuery)
            .get(TURN_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        assertEquals("first_terms=${first.terms}", 2, first.terms.size)
        assertEquals("How long should I set the timer for?", first.terms[0])

        val wrappedContext = first.terms[1].trim()
        assertTrue(
            "first_terms=${first.terms}",
            wrappedContext.startsWith(DIALOGUE_CONTEXT_PREFIX) && wrappedContext.endsWith(')'),
        )
        val context1 = SymbolicDialogueContextCodec.requireContextTerm(
            wrappedContext.removePrefix(DIALOGUE_CONTEXT_PREFIX).dropLast(1),
        )
        assertTrue("context1=$context1", context1.startsWith("partial_frame("))

        val followUpQuery = AndroidPureSymbolicConversationFactory.dialogueTurnEnvelopeQuery(
            utterance = "5 minutes",
            contextTerm = context1,
        )
        val followUp = try {
            session.queryLocalProlog(followUpQuery)
                .get(TURN_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        } catch (error: Throwable) {
            throw AssertionError(
                "serialized clarification Context1 failed on re-entry\n" +
                    "first_terms=${first.terms}\n" +
                    "context1=$context1\n" +
                    "follow_up_query=$followUpQuery\n" +
                    session.exportDiagnostics(),
                error,
            )
        }

        val evidence =
            "first_terms=${first.terms}\ncontext1=$context1\nfollow_up_terms=${followUp.terms}\n" +
                session.exportDiagnostics()
        assertEquals(evidence, 2, followUp.terms.size)
        assertEquals(
            evidence,
            "That action needs capability-checked execution before I can report success.",
            followUp.terms[0],
        )
        assertTrue(
            evidence,
            followUp.terms[1].startsWith("dialogue_context(completed_frame("),
        )
    }

    private fun awaitLocalServerReady() {
        val deadline = SystemClock.elapsedRealtime() + SERVER_TIMEOUT_MILLIS
        while (SystemClock.elapsedRealtime() < deadline) {
            val state = session.localServerState()
            if (state.phase == LocalServerPhase.READY) return
            check(state.phase != LocalServerPhase.FAILED) {
                "Local symbolic server failed during instrumentation: ${state.failure}"
            }
            SystemClock.sleep(50)
        }
        error("Timed out waiting for local symbolic server: ${session.localServerState()}")
    }

    private companion object {
        const val DIALOGUE_CONTEXT_PREFIX = "dialogue_context("
        const val TURN_TIMEOUT_SECONDS = 15L
        const val SERVER_TIMEOUT_MILLIS = 20_000L
    }
}
