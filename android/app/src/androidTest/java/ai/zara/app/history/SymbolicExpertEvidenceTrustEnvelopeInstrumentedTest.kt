package ai.zara.app.history

import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

class SymbolicExpertEvidenceTrustEnvelopeInstrumentedTest {
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
    fun canonicalTypedEvidenceSurvivesStoreRecreationWithZeroModelLedger() {
        val first = PortableConversationStore(context)
        first.createConversation("trusted expert evidence", conversationId = CONVERSATION_ID)
        first.saveSymbolicProjection(
            projection(CANONICAL_EVIDENCE),
            expectedGeneration = 0,
        ).assertPureSymbolic()
        first.close()

        val reopened = PortableConversationStore(context)
        try {
            val recovered = checkNotNull(reopened.loadSymbolicProjection(CONVERSATION_ID))
            recovered.assertPureSymbolic()
            assertEquals(CANONICAL_EVIDENCE, recovered.expertEvidenceJson)
            assertFalse(recovered.providersEnabled)
            assertEquals(0L, recovered.maxModelCalls)
            assertEquals(0L, recovered.providerCalls)
            assertEquals(0L, recovered.modelCalls)
        } finally {
            reopened.close()
        }
    }

    @Test
    fun providerShapedEvidenceFailsClosedAfterStoreRecreation() {
        val first = PortableConversationStore(context)
        first.createConversation("poisoned expert evidence", conversationId = CONVERSATION_ID)
        first.saveSymbolicProjection(
            projection(CANONICAL_EVIDENCE),
            expectedGeneration = 0,
        ).assertPureSymbolic()

        first.writableDatabase.execSQL(
            "UPDATE desktop_symbolic_projections SET expert_evidence_json = ? WHERE conversation_id = ? AND principal_id = ?",
            arrayOf(POISONED_EVIDENCE, CONVERSATION_ID, ConversationHistoryContract.localPrincipalId),
        )
        first.close()

        val reopened = PortableConversationStore(context)
        try {
            val rejected = runCatching { reopened.loadSymbolicProjection(CONVERSATION_ID) }
            assertTrue(
                "provider-shaped expert evidence must fail closed after canonical store recreation",
                rejected.isFailure,
            )
        } finally {
            reopened.close()
        }
    }

    private fun projection(expertEvidenceJson: String) = SymbolicConversationProjection(
        conversationId = CONVERSATION_ID,
        projectionGeneration = 1,
        runtimeGeneration = 1,
        turnId = "turn:expert-trust:1",
        outcome = "pending",
        projectId = "project:zara",
        projectGeneration = 1,
        dialogueAct = "expert.answer",
        dialogueStateJson = "{\"active_project\":\"project:zara\"}",
        expertEvidenceJson = expertEvidenceJson,
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        providerCalls = 0,
        modelCalls = 0,
    )

    private companion object {
        const val CONVERSATION_ID = "conversation:expert-trust-instrumented"
        const val CANONICAL_EVIDENCE =
            "[{\"expert_id\":\"zara:expert/python\",\"invocation_id\":\"invocation:python:1\",\"evidence_refs\":[\"evidence:python:1\"],\"verdict\":\"succeeded\",\"model_calls\":0,\"explanation\":{\"symbolic_terms\":[\"python\",\"inspect\"],\"trace\":[\"expert.invoke\",\"expert.complete\"]}}]"
        const val POISONED_EVIDENCE =
            "[{\"expert_id\":\"zara:expert/python\",\"invocation_id\":\"invocation:python:1\",\"evidence_refs\":[\"evidence:python:1\"],\"verdict\":\"succeeded\",\"model_calls\":0,\"usage\":{\"provider_calls\":1,\"provider\":\"legacy-provider\"}}]"
    }
}
