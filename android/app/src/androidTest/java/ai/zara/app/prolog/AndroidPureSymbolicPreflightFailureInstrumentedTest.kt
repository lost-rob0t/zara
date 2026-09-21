package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession
import ai.zara.app.conversations.CanonicalConversationStore
import ai.zara.app.history.ConversationHistoryContract
import ai.zara.app.history.HistoryMessageRole
import ai.zara.app.history.HistoryMessageStatus
import ai.zara.app.history.PortableConversationStore
import ai.zara.app.history.SymbolicConversationProjection
import ai.zara.app.history.loadSymbolicProjection
import ai.zara.app.history.saveSymbolicProjection
import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import java.util.concurrent.TimeUnit
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Before
import org.junit.Test

/** Real zara.db acceptance for a failure before the pending symbolic projection exists. */
class AndroidPureSymbolicPreflightFailureInstrumentedTest {
    private lateinit var context: Context
    private lateinit var store: PortableConversationStore
    private lateinit var history: CanonicalConversationStore
    private lateinit var session: AndroidAppSession
    private lateinit var metadataFile: File

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile = File(context.cacheDir, "pure-symbolic-preflight-failure-ui.bin")
        metadataFile.delete()
        store = PortableConversationStore(context)
        history = CanonicalConversationStore(
            history = store,
            metadataFile = metadataFile,
            legacyFile = null,
            idFactory = { CONVERSATION_ID },
        )
        assertEquals(CONVERSATION_ID, history.create().id)
        session = AndroidAppSession(context)
    }

    @After
    fun tearDown() {
        session.close()
        store.close()
        context.deleteDatabase(ConversationHistoryContract.databaseName)
        metadataFile.delete()
    }

    @Test
    fun malformedPersistedContextFailsTheCanonicalAssistantWithoutInstallingPendingProjection() {
        val malformed = SymbolicConversationProjection(
            conversationId = CONVERSATION_ID,
            projectionGeneration = 1L,
            runtimeGeneration = 1L,
            turnId = "prior-turn",
            outcome = "error",
            dialogueAct = "conversation",
            dialogueStateJson = "{\"version\":\"future-context/99\",\"term\":\"[]\"}",
            providersEnabled = false,
            maxModelCalls = 0L,
            providerCalls = 0L,
            modelCalls = 0L,
        )
        store.saveSymbolicProjection(malformed, expectedGeneration = 0L)

        history.beginTurn(CONVERSATION_ID, "hello")
        val canonicalTurnId = checkNotNull(
            store.loadMessages(CONVERSATION_ID).lastOrNull { message ->
                message.role == HistoryMessageRole.Assistant &&
                    message.status == HistoryMessageStatus.Pending
            }?.turnId
        )

        val result = AndroidPureSymbolicConversationFactory.create(
            session = session,
            projectionStore = store,
        ).submit("hello", CONVERSATION_ID).get(TURN_TIMEOUT_SECONDS, TimeUnit.SECONDS)

        assertFalse(result.turn.success)
        assertEquals(canonicalTurnId, result.turn.turnId)
        assertEquals(0, result.maxModelCalls)
        assertEquals(0, result.maxProviderCalls)
        assertEquals(0, result.modelCalls)
        assertEquals(0, result.providerCalls)

        val terminalAssistant = checkNotNull(
            store.loadMessages(CONVERSATION_ID).lastOrNull { message ->
                message.role == HistoryMessageRole.Assistant && message.turnId == canonicalTurnId
            }
        )
        assertEquals(HistoryMessageStatus.Error, terminalAssistant.status)
        assertEquals("The symbolic runtime could not complete this turn.", terminalAssistant.content)

        val unchangedProjection = checkNotNull(store.loadSymbolicProjection(CONVERSATION_ID))
        assertEquals(1L, unchangedProjection.projectionGeneration)
        assertEquals(1L, unchangedProjection.runtimeGeneration)
        assertEquals("prior-turn", unchangedProjection.turnId)
        assertEquals("error", unchangedProjection.outcome)
        unchangedProjection.assertPureSymbolic()
    }

    private companion object {
        const val CONVERSATION_ID = "android-pure-symbolic-preflight-failure"
        const val TURN_TIMEOUT_SECONDS = 15L
    }
}
