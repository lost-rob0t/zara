package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession
import ai.zara.app.conversations.CanonicalConversationStore
import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.HistoryMessageRole
import ai.zara.app.history.HistoryMessageStatus
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
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

class AndroidPureSymbolicConversationInstrumentedTest {
    private lateinit var context: Context
    private lateinit var metadataFile: File
    private lateinit var session: AndroidAppSession
    private var store: PortableConversationStore? = null

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "pure-symbolic-conversation-ui.bin")
        metadataFile.delete()
        File(metadataFile.parentFile, "${metadataFile.name}.migrated").delete()
        session = AndroidAppSession(context)
        awaitLocalServerReady()
    }

    @After
    fun tearDown() {
        store?.close()
        session.close()
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
        File(metadataFile.parentFile, "${metadataFile.name}.migrated").delete()
    }

    @Test
    fun clarificationFollowUpAndProcessRecreationStayPureSymbolicAndDurable() {
        var history = reopenHistory(createConversation = true)

        val clarification = runNaturalTurn(history, "timer")
        assertEquals("How long should I set the timer for?", clarification.turn.text)
        assertZeroModel(clarification)
        val clarificationProjection = checkNotNull(store).loadSymbolicProjection(CONVERSATION_ID)
        assertNotNull(clarificationProjection)
        clarificationProjection!!.assertPureSymbolic()
        assertEquals("success", clarificationProjection.outcome)
        assertTrue(
            SymbolicDialogueContextCodec.decode(clarificationProjection.dialogueStateJson)
                .startsWith("partial_frame("),
        )
        assertProjectionMatchesCanonicalTurn(clarificationProjection.turnId)

        history = reopenHistory(createConversation = false)
        assertEquals(CONVERSATION_ID, history.state().selectedConversationId)
        assertEquals(1, history.state().conversation(CONVERSATION_ID)!!.turns.size)

        val followUp = runNaturalTurn(history, "5 minutes")
        assertEquals(
            "That action needs capability-checked execution before I can report success.",
            followUp.turn.text,
        )
        assertZeroModel(followUp)
        val completedProjection = checkNotNull(store).loadSymbolicProjection(CONVERSATION_ID)
        assertNotNull(completedProjection)
        completedProjection!!.assertPureSymbolic()
        assertEquals("success", completedProjection.outcome)
        assertTrue(
            SymbolicDialogueContextCodec.decode(completedProjection.dialogueStateJson)
                .startsWith("completed_frame("),
        )
        assertTrue(
            "effectful intent must not claim unverified completion",
            followUp.turn.text.contains("capability-checked execution"),
        )

        history = reopenHistory(createConversation = false)
        assertEquals(CONVERSATION_ID, history.state().selectedConversationId)
        assertEquals(2, history.state().conversation(CONVERSATION_ID)!!.turns.size)

        val acknowledgement = runNaturalTurn(history, "thanks")
        assertTrue(acknowledgement.turn.text.contains("welcome", ignoreCase = true))
        assertZeroModel(acknowledgement)
        val finalProjection = checkNotNull(
            checkNotNull(store).loadSymbolicProjection(CONVERSATION_ID),
        )
        finalProjection.assertPureSymbolic()
        assertTrue(
            "social follow-up must preserve the completed prior frame across recreation",
            SymbolicDialogueContextCodec.decode(finalProjection.dialogueStateJson)
                .startsWith("completed_frame("),
        )
        assertEquals(0L, finalProjection.maxModelCalls)
        assertEquals(0L, finalProjection.providerCalls)
        assertEquals(0L, finalProjection.modelCalls)
        assertFalse(finalProjection.providersEnabled)
    }

    @Test
    fun controllerCompletionCannotLeaveSuccessProjectionBesideRunningHistory() {
        var history = reopenHistory(createConversation = true)
        history.beginTurn(CONVERSATION_ID, "timer")
        val controller = AndroidPureSymbolicConversationFactory.create(
            session = session,
            projectionStore = checkNotNull(store),
        )

        val result = controller.submit("timer", CONVERSATION_ID)
            .get(TURN_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        assertEquals("How long should I set the timer for?", result.turn.text)
        assertZeroModel(result)

        // Deliberately do not call CanonicalConversationStore.completeTurn(). This models process
        // death after the deterministic runtime has completed but before the UI callback gets a
        // chance to write history. The runtime/store boundary must already have committed the
        // assistant message and Context1 together; otherwise restart can split visible history
        // from the symbolic brain state.
        val beforeRestartMessages = checkNotNull(store).loadMessages(CONVERSATION_ID)
        val assistantBeforeRestart = beforeRestartMessages.last {
            it.role == HistoryMessageRole.Assistant
        }
        assertEquals(result.turn.text, assistantBeforeRestart.content)
        assertEquals(HistoryMessageStatus.Complete, assistantBeforeRestart.status)

        val beforeRestartProjection = checkNotNull(
            checkNotNull(store).loadSymbolicProjection(CONVERSATION_ID),
        )
        beforeRestartProjection.assertPureSymbolic()
        assertEquals("success", beforeRestartProjection.outcome)
        assertEquals(assistantBeforeRestart.turnId, beforeRestartProjection.turnId)
        assertEquals(0L, beforeRestartProjection.maxModelCalls)
        assertEquals(0L, beforeRestartProjection.providerCalls)
        assertEquals(0L, beforeRestartProjection.modelCalls)
        assertFalse(beforeRestartProjection.providersEnabled)

        history = reopenHistory(createConversation = false)
        val recovered = history.state().conversation(CONVERSATION_ID)!!
        assertEquals(1, recovered.turns.size)
        assertEquals(result.turn.text, recovered.turns.single().assistantText)
        assertEquals(true, recovered.turns.single().success)

        val afterRestartProjection = checkNotNull(
            checkNotNull(store).loadSymbolicProjection(CONVERSATION_ID),
        )
        afterRestartProjection.assertPureSymbolic()
        assertEquals("success", afterRestartProjection.outcome)
        assertEquals(beforeRestartProjection.projectionGeneration, afterRestartProjection.projectionGeneration)
        assertEquals(beforeRestartProjection.dialogueStateJson, afterRestartProjection.dialogueStateJson)
        assertEquals(0L, afterRestartProjection.maxModelCalls)
        assertEquals(0L, afterRestartProjection.providerCalls)
        assertEquals(0L, afterRestartProjection.modelCalls)
        assertFalse(afterRestartProjection.providersEnabled)
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

    private fun runNaturalTurn(
        history: CanonicalConversationStore,
        text: String,
    ): PureSymbolicTurnResult {
        history.beginTurn(CONVERSATION_ID, text)
        val controller = AndroidPureSymbolicConversationFactory.create(
            session = session,
            projectionStore = checkNotNull(store),
        )
        val result = controller.submit(text, CONVERSATION_ID).get(TURN_TIMEOUT_SECONDS, TimeUnit.SECONDS)
        history.completeTurn(
            conversationId = CONVERSATION_ID,
            assistantText = result.turn.text,
            success = result.turn.success,
        )
        return result
    }

    private fun assertZeroModel(result: PureSymbolicTurnResult) {
        assertEquals(PureSymbolicRoute.FRAME_RESOLVER, result.route)
        assertEquals("symbolic-dcg/v1", result.renderer)
        assertEquals(0, result.maxModelCalls)
        assertEquals(0, result.maxProviderCalls)
        assertEquals(0, result.modelCalls)
        assertEquals(0, result.providerCalls)
    }

    private fun assertProjectionMatchesCanonicalTurn(projectionTurnId: String?) {
        val assistant = checkNotNull(store).loadMessages(CONVERSATION_ID).last {
            it.role == HistoryMessageRole.Assistant
        }
        assertEquals(assistant.turnId, projectionTurnId)
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
        const val CONVERSATION_ID = "android-pure-symbolic-e2e"
        const val TURN_TIMEOUT_SECONDS = 15L
        const val SERVER_TIMEOUT_MILLIS = 20_000L
    }
}
