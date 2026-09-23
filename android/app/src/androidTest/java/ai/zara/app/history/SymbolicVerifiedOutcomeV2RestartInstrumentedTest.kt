package ai.zara.app.history

import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

class SymbolicVerifiedOutcomeV2RestartInstrumentedTest {
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
    fun eightyVerifiedTurnsStayBoundedAndRejectRetiredReplayAfterProcessRecreation() {
        val first = PortableConversationStore(context)
        first.createConversation("Verified v2 long run", conversationId = CONVERSATION_ID)

        var current = first.saveSymbolicProjection(
            projection(
                generation = 1,
                runtimeGeneration = 1,
                receipts = listOf(receipt(1)),
            ),
            expectedGeneration = 0,
        )
        current.assertPureSymbolic()

        for (runtimeGeneration in 2L..80L) {
            current = first.saveSymbolicProjection(
                projection(
                    generation = runtimeGeneration,
                    runtimeGeneration = runtimeGeneration,
                    receipts = nextWindow(current.verifiedOutcomeRefs, runtimeGeneration),
                ),
                expectedGeneration = current.projectionGeneration,
            )
            current.assertPureSymbolic()
            assertTrue(current.verifiedOutcomeRefs.size <= WINDOW)
            assertEquals(receipt(runtimeGeneration), current.verifiedOutcomeRefs.last())
            assertEquals(0L, current.maxModelCalls)
            assertEquals(0L, current.providerCalls)
            assertEquals(0L, current.modelCalls)
        }

        assertEquals(80L, current.projectionGeneration)
        assertEquals(80L, current.runtimeGeneration)
        assertEquals(WINDOW, current.verifiedOutcomeRefs.size)
        val retired = receipt(1)
        assertFalse(current.verifiedOutcomeRefs.contains(retired))
        first.close()

        val reopened = PortableConversationStore(context)
        try {
            val recovered = reopened.loadSymbolicProjection(CONVERSATION_ID)
            assertNotNull(recovered)
            requireNotNull(recovered)
            recovered.assertPureSymbolic()
            assertEquals(80L, recovered.projectionGeneration)
            assertEquals(80L, recovered.runtimeGeneration)
            assertEquals(current.verifiedOutcomeRefs, recovered.verifiedOutcomeRefs)
            assertEquals(0L, recovered.maxModelCalls)
            assertEquals(0L, recovered.providerCalls)
            assertEquals(0L, recovered.modelCalls)

            val edge = reopened.loadSymbolicEdgeSnapshot(CONVERSATION_ID)
            assertNotNull(edge)
            requireNotNull(edge)
            edge.assertPureSymbolic()
            assertEquals(WINDOW, edge.verifiedOutcomeRefs.size)
            assertEquals(recovered.verifiedOutcomeRefs, edge.verifiedOutcomeRefs)
            assertEquals(0L, edge.maxModelCalls)
            assertEquals(0L, edge.providerCalls)
            assertEquals(0L, edge.modelCalls)

            val fresh = reopened.saveSymbolicProjection(
                projection(
                    generation = 81,
                    runtimeGeneration = 81,
                    receipts = nextWindow(recovered.verifiedOutcomeRefs, 81),
                ),
                expectedGeneration = recovered.projectionGeneration,
            )
            fresh.assertPureSymbolic()

            val replayFailure = runCatching {
                reopened.saveSymbolicProjection(
                    projection(
                        generation = 82,
                        runtimeGeneration = 82,
                        receipts = fresh.verifiedOutcomeRefs.drop(1) + retired,
                    ),
                    expectedGeneration = fresh.projectionGeneration,
                )
            }.exceptionOrNull()
            requireNotNull(replayFailure)
            assertTrue(
                replayFailure.message.orEmpty().contains(
                    "retired verified outcome replay rejected"
                )
            )

            val staleFailure = runCatching {
                reopened.saveSymbolicProjection(
                    projection(
                        generation = 81,
                        runtimeGeneration = 82,
                        receipts = fresh.verifiedOutcomeRefs,
                    ).copy(turnId = "turn-late"),
                    expectedGeneration = 80,
                )
            }.exceptionOrNull()
            requireNotNull(staleFailure)
            assertTrue(staleFailure.message.orEmpty().contains("stale symbolic projection write"))
        } finally {
            reopened.close()
        }
    }

    private fun projection(
        generation: Long,
        runtimeGeneration: Long,
        receipts: List<String>,
    ) = SymbolicConversationProjection(
        conversationId = CONVERSATION_ID,
        projectionGeneration = generation,
        runtimeGeneration = runtimeGeneration,
        turnId = "turn-$runtimeGeneration",
        outcome = "success",
        dialogueAct = "verified",
        dialogueStateJson = "{\"act\":\"verified\"}",
        verifiedOutcomeRefs = receipts,
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        providerCalls = 0,
        modelCalls = 0,
    )

    private fun nextWindow(current: List<String>, runtimeGeneration: Long): List<String> {
        val fresh = receipt(runtimeGeneration)
        return if (current.size < WINDOW) current + fresh else current.drop(1) + fresh
    }

    private fun receipt(runtimeGeneration: Long): String =
        "zara.verified-outcome/v2:$runtimeGeneration:" +
            "outcome:postcondition/tool-run-$runtimeGeneration"

    private companion object {
        const val WINDOW = 64
        const val CONVERSATION_ID = "verified-v2-long-run"
    }
}
