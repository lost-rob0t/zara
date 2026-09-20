package ai.zara.app.history

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicConversationProjectionTest {
    private fun projection(
        generation: Long = 1,
        runtimeGeneration: Long = 7,
        turnId: String? = "turn-7",
        outcome: String = "pending",
        projectId: String? = "project-a",
        projectGeneration: Long = 1,
        dialogueStateJson: String = "{\"act\":\"clarify\"}",
        discourseEntitiesJson: String = "[{\"entity_id\":\"file:flake.nix\"}]",
        providerCalls: Long = 0,
        modelCalls: Long = 0,
    ) = SymbolicConversationProjection(
        conversationId = "conv-symbolic",
        projectionGeneration = generation,
        runtimeGeneration = runtimeGeneration,
        turnId = turnId,
        outcome = outcome,
        projectId = projectId,
        projectGeneration = projectGeneration,
        dialogueStateJson = dialogueStateJson,
        discourseEntitiesJson = discourseEntitiesJson,
        unresolvedQuestionsJson = "[{\"slot\":\"target\"}]",
        expertEvidenceJson = "[{\"evidence_id\":\"ev-1\"}]",
        verifiedFactsJson = "[{\"fact_id\":\"fact-1\"}]",
        rendererProvenance = "symbolic-nlg/v1",
        providerCalls = providerCalls,
        modelCalls = modelCalls,
    )

    @Test
    fun `initial pure symbolic projection is accepted with zero provider and model calls`() {
        val proposed = projection()

        SymbolicProjectionContract.validateWrite(null, proposed, expectedGeneration = 0)
        proposed.assertPureSymbolic()
        assertEquals("turn-7", proposed.turnId)
        assertEquals("pending", proposed.outcome)
        assertEquals(0L, proposed.providerCalls)
        assertEquals(0L, proposed.modelCalls)
    }

    @Test
    fun `late generation and runtime regressions are rejected`() {
        val current = projection(generation = 4, runtimeGeneration = 9)

        assertFailsWithMessage("stale symbolic projection write") {
            SymbolicProjectionContract.validateWrite(
                current,
                projection(generation = 4, runtimeGeneration = 10),
                expectedGeneration = 3,
            )
        }
        assertFailsWithMessage("runtimeGeneration regression") {
            SymbolicProjectionContract.validateWrite(
                current,
                projection(generation = 5, runtimeGeneration = 8),
                expectedGeneration = 4,
            )
        }
    }

    @Test
    fun `project switch must advance project generation`() {
        val current = projection(generation = 1, projectId = "project-a", projectGeneration = 4)

        assertFailsWithMessage("project switch must advance") {
            SymbolicProjectionContract.validateWrite(
                current,
                projection(
                    generation = 2,
                    runtimeGeneration = 8,
                    turnId = "turn-8",
                    outcome = "success",
                    projectId = "project-b",
                    projectGeneration = 4,
                ),
                expectedGeneration = 1,
            )
        }

        SymbolicProjectionContract.validateWrite(
            current,
            projection(
                generation = 2,
                runtimeGeneration = 8,
                turnId = "turn-8",
                outcome = "success",
                projectId = "project-b",
                projectGeneration = 5,
            ),
            expectedGeneration = 1,
        )
    }

    @Test
    fun `provider and model call ledgers cannot be rewound to fake zero`() {
        val current = projection(generation = 1, providerCalls = 1, modelCalls = 1)

        assertFailsWithMessage("provider-call ledger rewind") {
            SymbolicProjectionContract.validateWrite(
                current,
                projection(generation = 2, providerCalls = 0, modelCalls = 1),
                expectedGeneration = 1,
            )
        }
        assertFailsWithMessage("model-call ledger rewind") {
            SymbolicProjectionContract.validateWrite(
                current,
                projection(generation = 2, providerCalls = 1, modelCalls = 0),
                expectedGeneration = 1,
            )
        }
    }

    @Test
    fun `pure symbolic assertion rejects provider or model use`() {
        assertFailsWithMessage("providerCalls=1") {
            projection(providerCalls = 1).assertPureSymbolic()
        }
        assertFailsWithMessage("modelCalls=1") {
            projection(modelCalls = 1).assertPureSymbolic()
        }
    }

    @Test
    fun `turn outcome vocabulary fails closed`() {
        listOf("unknown", "pending", "success", "cancelled", "interrupted", "error").forEach { outcome ->
            SymbolicProjectionContract.validatePayload(projection(outcome = outcome))
        }
        assertFailsWithMessage("unsupported symbolic outcome") {
            SymbolicProjectionContract.validatePayload(projection(outcome = "provider_fallback"))
        }
    }

    @Test
    fun `android and desktop reject malformed or mistyped json projections`() {
        listOf(
            "{not-json}",
            "{\"ok\":true,}",
            "{\"unterminated\":\"x}",
            "[]",
        ).forEach { invalidObject ->
            assertFailsWithMessage("JSON") {
                SymbolicProjectionContract.validatePayload(
                    projection(dialogueStateJson = invalidObject)
                )
            }
        }
        listOf(
            "[}]",
            "[1,]",
            "[\"unterminated]",
            "{}",
            "[null]",
            "[1]",
            "[\"scalar\"]",
        ).forEach { invalidArray ->
            assertFailsWithMessage("JSON") {
                SymbolicProjectionContract.validatePayload(
                    projection(discourseEntitiesJson = invalidArray)
                )
            }
        }

        SymbolicProjectionContract.validatePayload(
            projection(
                dialogueStateJson = "{\"nested\":{\"n\":-1.25e+2},\"ok\":true}",
                discourseEntitiesJson =
                    "[{\"escaped\":\"line\\nvalue\",\"u\":\"\\u263A\"},{\"nested\":[null,false,3]}]",
            )
        )
    }

    @Test
    fun `android projection uses canonical portable conversation schema`() {
        val schema = File("../../zara/conversation_schema.sql").readText()
        val source = File(
            "src/main/java/ai/zara/app/history/SymbolicConversationProjection.kt"
        ).readText()

        assertTrue(schema.contains("CREATE TABLE IF NOT EXISTS desktop_symbolic_projections"))
        assertTrue(schema.contains("turn_id TEXT"))
        assertTrue(schema.contains("'cancelled', 'interrupted', 'error'"))
        assertTrue(schema.contains("provider_calls INTEGER NOT NULL DEFAULT 0"))
        assertTrue(schema.contains("model_calls INTEGER NOT NULL DEFAULT 0"))
        assertTrue(schema.contains("FOREIGN KEY(conversation_id)"))
        assertTrue(source.contains("fun PortableConversationStore.saveSymbolicProjection"))
        assertTrue(source.contains("projection_generation = ?"))
        assertTrue(source.contains("provider-call ledger rewind rejected"))
        assertTrue(source.contains("model-call ledger rewind rejected"))
        assertTrue(source.contains("PortableJsonValidator"))
        assertTrue(source.contains("parseObjectArrayDocument"))
    }

    private fun assertFailsWithMessage(fragment: String, block: () -> Unit) {
        try {
            block()
            fail("expected failure containing: $fragment")
        } catch (error: RuntimeException) {
            assertTrue(error.message.orEmpty().contains(fragment))
        }
    }
}
