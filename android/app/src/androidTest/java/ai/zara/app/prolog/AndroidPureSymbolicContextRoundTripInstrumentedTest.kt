package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession
import ai.zara.app.runtime.LocalQueryResult
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

class AndroidPureSymbolicContextRoundTripInstrumentedTest {
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
    fun nativeTreallaCompletesSerializedClarificationContext() {
        val clarification = evaluate(
            utterance = "timer",
            contextTerm = SymbolicDialogueContextCodec.emptyContextTerm,
        )
        val context1 = extractContext(clarification)
        assertTrue(
            evidence("clarification", clarification, context1),
            context1.startsWith("partial_frame("),
        )
        assertTrue(
            "canonical Context1 must preserve quoting for dotted atoms: $context1",
            context1.contains("name('timer.set')"),
        )

        // Cross the same bounded JSON/text boundary used by the durable projection before
        // feeding Context0 back to native Trealla. This isolates runtime/ABI continuation from
        // conversation-store terminal commit behavior when the full installed test fails.
        val persisted = SymbolicDialogueContextCodec.encode(context1)
        val restoredContext0 = SymbolicDialogueContextCodec.decode(persisted)
        assertEquals(context1, restoredContext0)

        val completion = evaluate(
            utterance = "5 minutes",
            contextTerm = restoredContext0,
        )
        val completionContext = extractContext(completion)
        val completionEvidence = evidence("completion", completion, restoredContext0)
        assertEquals(
            completionEvidence,
            EXPECTED_CAPABILITY_GATE,
            completion.terms.first(),
        )
        assertTrue(
            completionEvidence,
            completionContext.startsWith("completed_frame("),
        )
        assertTrue(
            "completed Context1 must remain readable after native Trealla serialization: $completionContext",
            completionContext.contains("name('timer.set')"),
        )
    }

    private fun evaluate(
        utterance: String,
        contextTerm: String,
    ): LocalQueryResult {
        val query = AndroidPureSymbolicConversationFactory.dialogueTurnEnvelopeQuery(
            utterance = utterance,
            contextTerm = contextTerm,
        )
        return try {
            session.queryLocalProlog(query).get(TURN_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        } catch (error: Throwable) {
            throw AssertionError(
                buildString {
                    appendLine("native persisted clarification envelope failed")
                    appendLine("utterance=$utterance")
                    appendLine("context_term=$contextTerm")
                    appendLine("query=$query")
                    append(session.exportDiagnostics())
                },
                error,
            )
        }
    }

    private fun extractContext(result: LocalQueryResult): String {
        val evidence = evidence("envelope", result, null)
        assertEquals(evidence, 4, result.terms.size)
        assertEquals(
            evidence,
            1,
            result.terms.count { term -> term.startsWith(DIALOGUE_ACT_WIRE_PREFIX) },
        )
        assertEquals(
            evidence,
            1,
            result.terms.count { term -> term.startsWith(DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX) },
        )
        val evidenceWire = result.terms.single { term ->
            term.startsWith(DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX)
        }
        assertEquals(
            "non-expert timer dialogue must not smuggle expert evidence\n$evidence",
            DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX,
            evidenceWire,
        )
        val wire = result.terms.singleOrNull { term ->
            term.startsWith(DIALOGUE_CONTEXT_WIRE_PREFIX)
        } ?: throw AssertionError("missing canonical dialogue context wire\n$evidence")
        return SymbolicDialogueContextCodec.requireContextTerm(
            wire.removePrefix(DIALOGUE_CONTEXT_WIRE_PREFIX),
        )
    }

    private fun evidence(
        phase: String,
        result: LocalQueryResult,
        contextTerm: String?,
    ): String = buildString {
        appendLine("phase=$phase")
        if (contextTerm != null) appendLine("context_term=$contextTerm")
        appendLine("raw_terms=${result.terms}")
        append(session.exportDiagnostics())
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
        const val EXPECTED_CAPABILITY_GATE =
            "That action needs capability-checked execution before I can report success."
        const val DIALOGUE_ACT_WIRE_PREFIX = "__zara_act__:"
        const val DIALOGUE_CONTEXT_WIRE_PREFIX = "__zara_context__:"
        const val DIALOGUE_EXPERT_EVIDENCE_WIRE_PREFIX = "__zara_expert_evidence__:"
        const val TURN_TIMEOUT_SECONDS = 15L
        const val SERVER_TIMEOUT_MILLIS = 20_000L
    }
}
