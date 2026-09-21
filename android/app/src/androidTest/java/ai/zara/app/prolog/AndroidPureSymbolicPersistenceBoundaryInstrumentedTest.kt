package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession
import ai.zara.app.conversations.CanonicalConversationStore
import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.PortableConversationStore
import ai.zara.app.history.loadSymbolicProjection
import ai.zara.app.runtime.LocalServerPhase
import android.content.Context
import android.os.SystemClock
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import java.util.concurrent.TimeUnit
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

/**
 * Diagnostic acceptance around the remaining Android process-recreation boundary.
 *
 * This deliberately uses the same canonical zara.db owner and the same native Trealla query path
 * as production. The direct query proves whether the persisted Context1 survives a real SQLite
 * close/reopen before the factory stages the next pending generation. If the factory result still
 * fails after that direct query succeeds, the emitted evidence pins the bug to the pending/terminal
 * persistence boundary instead of the dialogue semantics or provider fallback.
 */
class AndroidPureSymbolicPersistenceBoundaryInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File
    private lateinit var session: AndroidAppSession
    private var store: PortableConversationStore? = null

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "pure-symbolic-persistence-boundary-ui.bin")
        metadataFile.delete()
        session = AndroidAppSession(context)
        awaitLocalServerReady()
    }

    @After
    fun tearDown() {
        store?.close()
        session.close()
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun reopenedSqliteContextCompletesNativelyBeforeCanonicalSecondTurnCommit() {
        var history = reopenHistory(createConversation = true)
        val clarification = runNaturalTurn(history, "timer")
        assertEquals(EXPECTED_CLARIFICATION, clarification.turn.text)
        assertZeroModel(clarification)

        history = reopenHistory(createConversation = false)
        val reopenedProjection = checkNotNull(checkNotNull(store).loadSymbolicProjection(CONVERSATION_ID))
        reopenedProjection.assertPureSymbolic()
        assertEquals("success", reopenedProjection.outcome)
        val restoredContext0 = SymbolicDialogueContextCodec.decode(reopenedProjection.dialogueStateJson)
        assertTrue(restoredContext0.startsWith("partial_frame("))

        val directResult = session.queryLocalProlog(
            AndroidPureSymbolicConversationFactory.dialogueTurnEnvelopeQuery(
                utterance = "5 minutes",
                contextTerm = restoredContext0,
            )
        ).get(TURN_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        assertEquals(
            directEvidence(reopenedProjection.toString(), restoredContext0, directResult.terms),
            EXPECTED_CAPABILITY_GATE,
            directResult.terms.firstOrNull(),
        )

        history.beginTurn(CONVERSATION_ID, "5 minutes")
        val controller = AndroidPureSymbolicConversationFactory.create(
            session = session,
            projectionStore = checkNotNull(store),
        )
        val followUp = controller.submit("5 minutes", CONVERSATION_ID)
            .get(TURN_TIMEOUT_SECONDS, TimeUnit.SECONDS)

        val afterControllerProjection = checkNotNull(store).loadSymbolicProjection(CONVERSATION_ID)
        val afterControllerMessages = checkNotNull(store).loadMessages(CONVERSATION_ID)
        val evidence = buildString {
            appendLine("reopened_projection=$reopenedProjection")
            appendLine("restored_context0=$restoredContext0")
            appendLine("direct_terms=${directResult.terms}")
            appendLine("controller_turn=${followUp.turn}")
            appendLine("controller_route=${followUp.route}")
            appendLine("controller_renderer=${followUp.renderer}")
            appendLine("after_controller_projection=$afterControllerProjection")
            appendLine("after_controller_messages=$afterControllerMessages")
            append(session.exportDiagnostics())
        }
        assertEquals(evidence, EXPECTED_CAPABILITY_GATE, followUp.turn.text)
        assertTrue(evidence, followUp.turn.success)
        assertZeroModel(followUp)

        history.completeTurn(
            conversationId = CONVERSATION_ID,
            assistantText = followUp.turn.text,
            success = followUp.turn.success,
        )
        val completed = checkNotNull(checkNotNull(store).loadSymbolicProjection(CONVERSATION_ID))
        completed.assertPureSymbolic()
        assertEquals("success", completed.outcome)
        assertTrue(
            SymbolicDialogueContextCodec.decode(completed.dialogueStateJson)
                .startsWith("completed_frame("),
        )
    }

    private fun runNaturalTurn(
        history: CanonicalConversationStore,
        text: String,
    ): PureSymbolicTurnResult {
        history.beginTurn(CONVERSATION_ID, text)
        val result = AndroidPureSymbolicConversationFactory.create(
            session = session,
            projectionStore = checkNotNull(store),
        ).submit(text, CONVERSATION_ID).get(TURN_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        history.completeTurn(
            conversationId = CONVERSATION_ID,
            assistantText = result.turn.text,
            success = result.turn.success,
        )
        return result
    }

    private fun reopenHistory(createConversation: Boolean): CanonicalConversationStore {
        store?.close()
        store = PortableConversationStore(context)
        val history = CanonicalConversationStore(
            history = checkNotNull(store),
            metadataFile = metadataFile,
            legacyFile = null,
            idFactory = { CONVERSATION_ID },
        )
        if (createConversation) {
            assertEquals(CONVERSATION_ID, history.create().id)
        }
        return history
    }

    private fun assertZeroModel(result: PureSymbolicTurnResult) {
        assertEquals(PureSymbolicRoute.FRAME_RESOLVER, result.route)
        assertEquals("symbolic-dcg/v1", result.renderer)
        assertEquals(0, result.maxModelCalls)
        assertEquals(0, result.maxProviderCalls)
        assertEquals(0, result.modelCalls)
        assertEquals(0, result.providerCalls)
        assertFalse(result.turn.text.isBlank())
    }

    private fun directEvidence(
        projection: String,
        contextTerm: String,
        terms: List<String>,
    ): String = buildString {
        appendLine("projection=$projection")
        appendLine("context_term=$contextTerm")
        appendLine("direct_terms=$terms")
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
        const val CONVERSATION_ID = "android-pure-symbolic-persistence-boundary"
        const val EXPECTED_CLARIFICATION = "How long should I set the timer for?"
        const val EXPECTED_CAPABILITY_GATE =
            "That action needs capability-checked execution before I can report success."
        const val TURN_TIMEOUT_SECONDS = 15L
        const val SERVER_TIMEOUT_MILLIS = 20_000L
    }
}
