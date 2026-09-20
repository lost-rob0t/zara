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
        projectId: String? = "project-a",
        projectGeneration: Long = 1,
        modelCalls: Long = 0,
    ) = SymbolicConversationProjection(
        conversationId = "conv-symbolic",
        projectionGeneration = generation,
        runtimeGeneration = runtimeGeneration,
        projectId = projectId,
        projectGeneration = projectGeneration,
        dialogueStateJson = "{\"act\":\"clarify\"}",
        discourseEntitiesJson = "[{\"entity_id\":\"file:flake.nix\"}]",
        unresolvedQuestionsJson = "[{\"slot\":\"target\"}]",
        expertEvidenceJson = "[{\"evidence_id\":\"ev-1\"}]",
        verifiedFactsJson = "[{\"fact_id\":\"fact-1\"}]",
        rendererProvenance = "symbolic-nlg/v1",
        modelCalls = modelCalls,
    )

    @Test
    fun `initial pure symbolic projection is accepted with zero model calls`() {
        val proposed = projection()

        SymbolicProjectionContract.validateWrite(null, proposed, expectedGeneration = 0)
        proposed.assertPureSymbolic()
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
                projectId = "project-b",
                projectGeneration = 5,
            ),
            expectedGeneration = 1,
        )
    }

    @Test
    fun `model call ledger cannot be rewound to fake zero`() {
        val current = projection(generation = 1, modelCalls = 1)

        assertFailsWithMessage("model-call ledger rewind") {
            SymbolicProjectionContract.validateWrite(
                current,
                projection(generation = 2, modelCalls = 0),
                expectedGeneration = 1,
            )
        }
    }

    @Test
    fun `android projection uses canonical portable conversation schema`() {
        val schema = File("../../zara/conversation_schema.sql").readText()
        val source = File(
            "src/main/java/ai/zara/app/history/SymbolicConversationProjection.kt"
        ).readText()

        assertTrue(schema.contains("CREATE TABLE IF NOT EXISTS desktop_symbolic_projections"))
        assertTrue(schema.contains("model_calls INTEGER NOT NULL DEFAULT 0"))
        assertTrue(schema.contains("FOREIGN KEY(conversation_id)"))
        assertTrue(source.contains("fun PortableConversationStore.saveSymbolicProjection"))
        assertTrue(source.contains("projection_generation = ?"))
        assertTrue(source.contains("model-call ledger rewind rejected"))
    }

    private fun assertFailsWithMessage(fragment: String, block: () -> Unit) {
        try {
            block()
            fail("expected failure containing: $fragment")
        } catch (error: IllegalStateException) {
            assertTrue(error.message.orEmpty().contains(fragment))
        }
    }
}
