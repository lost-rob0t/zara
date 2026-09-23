package ai.zara.app.history

import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Before
import org.junit.Test

class PortableConversationLegacyPrincipalInstrumentedTest {
    private lateinit var context: Context

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
    }

    @Test
    fun numericUidProjectionIsClaimedWithCanonicalLocalHistoryOnReopen() {
        val first = PortableConversationStore(context)
        first.createConversation("Legacy symbolic owner", conversationId = CONVERSATION_ID)
        first.saveSymbolicProjection(
            SymbolicConversationProjection(
                conversationId = CONVERSATION_ID,
                projectionGeneration = 1,
                runtimeGeneration = 3,
                turnId = "turn-legacy-symbolic-owner",
                outcome = "pending",
                dialogueAct = "clarify",
                dialogueStateJson = "{\"intent\":\"timer\",\"slot\":\"duration\"}",
                unresolvedQuestionsJson = "[{\"slot\":\"duration\"}]",
                rendererProvenance = "symbolic-dcg/v1",
                providersEnabled = false,
                maxModelCalls = 0,
                providerCalls = 0,
                modelCalls = 0,
            ),
            expectedGeneration = 0,
        ).assertPureSymbolic()

        first.writableDatabase.execSQL(
            "UPDATE desktop_conversations SET principal_id = ? WHERE id = ?",
            arrayOf(PREVIOUS_UID, CONVERSATION_ID),
        )
        first.writableDatabase.execSQL(
            "UPDATE desktop_symbolic_projections SET principal_id = ? WHERE conversation_id = ?",
            arrayOf(PREVIOUS_UID, CONVERSATION_ID),
        )
        first.close()

        val reopened = PortableConversationStore(context)
        try {
            val recovered = checkNotNull(reopened.loadSymbolicProjection(CONVERSATION_ID))
            recovered.assertPureSymbolic()
            assertEquals(1L, recovered.projectionGeneration)
            assertEquals(3L, recovered.runtimeGeneration)
            assertEquals("turn-legacy-symbolic-owner", recovered.turnId)
            assertEquals("clarify", recovered.dialogueAct)
            assertEquals(
                "{\"intent\":\"timer\",\"slot\":\"duration\"}",
                recovered.dialogueStateJson,
            )
            assertEquals("[{\"slot\":\"duration\"}]", recovered.unresolvedQuestionsJson)
            assertFalse(recovered.providersEnabled)
            assertEquals(0L, recovered.maxModelCalls)
            assertEquals(0L, recovered.providerCalls)
            assertEquals(0L, recovered.modelCalls)

            val owners = reopened.readableDatabase.rawQuery(
                """
                SELECT c.principal_id, p.principal_id
                FROM desktop_conversations AS c
                JOIN desktop_symbolic_projections AS p ON p.conversation_id = c.id
                WHERE c.id = ?
                """.trimIndent(),
                arrayOf(CONVERSATION_ID),
            ).use { cursor ->
                check(cursor.moveToFirst())
                cursor.getString(0) to cursor.getString(1)
            }
            assertEquals(ConversationHistoryContract.localPrincipalId, owners.first)
            assertEquals(ConversationHistoryContract.localPrincipalId, owners.second)
        } finally {
            reopened.close()
        }
    }

    private companion object {
        const val CONVERSATION_ID = "legacy-symbolic-owner"
        const val PREVIOUS_UID = "uid:4242"
    }
}
