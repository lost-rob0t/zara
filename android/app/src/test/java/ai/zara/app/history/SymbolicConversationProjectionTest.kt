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
        dialogueAct: String = "clarify",
        dialogueStateJson: String = "{\"act\":\"clarify\"}",
        discourseEntitiesJson: String = "[{\"entity_id\":\"file:flake.nix\"}]",
        verifiedOutcomeRefs: List<String> = listOf(VERIFIED_EFFECT_REF),
        rendererProvenance: String = SYMBOLIC_RENDERER,
        maxModelCalls: Long = 0,
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
        dialogueAct = dialogueAct,
        dialogueStateJson = dialogueStateJson,
        discourseEntitiesJson = discourseEntitiesJson,
        unresolvedQuestionsJson = "[{\"slot\":\"target\"}]",
        expertEvidenceJson = "[{\"evidence_id\":\"ev-1\"}]",
        verifiedFactsJson = "[{\"fact_id\":\"fact-1\"}]",
        verifiedOutcomeRefs = verifiedOutcomeRefs,
        rendererProvenance = rendererProvenance,
        providersEnabled = false,
        maxModelCalls = maxModelCalls,
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
        assertEquals("clarify", proposed.dialogueAct)
        assertEquals(listOf(VERIFIED_EFFECT_REF), proposed.verifiedOutcomeRefs)
        assertEquals(SYMBOLIC_RENDERER, proposed.rendererProvenance)
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
    fun `cancelled turn rejects late success while next runtime turn is allowed`() {
        val pending = projection(
            generation = 1,
            runtimeGeneration = 11,
            turnId = "turn-old",
            outcome = "pending",
        )
        val cancelled = projection(
            generation = 2,
            runtimeGeneration = 11,
            turnId = "turn-old",
            outcome = "cancelled",
        )
        SymbolicProjectionContract.validateWrite(pending, cancelled, expectedGeneration = 1)

        assertFailsWithMessage("terminal turn projection is immutable") {
            SymbolicProjectionContract.validateWrite(
                cancelled,
                projection(
                    generation = 3,
                    runtimeGeneration = 11,
                    turnId = "turn-old",
                    outcome = "success",
                ),
                expectedGeneration = 2,
            )
        }
        assertFailsWithMessage("same turn must preserve runtimeGeneration") {
            SymbolicProjectionContract.validateWrite(
                cancelled,
                projection(
                    generation = 3,
                    runtimeGeneration = 12,
                    turnId = "turn-old",
                    outcome = "cancelled",
                ),
                expectedGeneration = 2,
            )
        }

        SymbolicProjectionContract.validateWrite(
            cancelled,
            projection(
                generation = 3,
                runtimeGeneration = 12,
                turnId = "turn-new",
                outcome = "pending",
            ),
            expectedGeneration = 2,
        )
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
        val current = projection(generation = 1, maxModelCalls = 1, providerCalls = 1, modelCalls = 1)

        assertFailsWithMessage("provider-call ledger rewind") {
            SymbolicProjectionContract.validateWrite(
                current,
                projection(
                    generation = 2,
                    maxModelCalls = 1,
                    providerCalls = 0,
                    modelCalls = 1,
                ),
                expectedGeneration = 1,
            )
        }
        assertFailsWithMessage("model-call ledger rewind") {
            SymbolicProjectionContract.validateWrite(
                current,
                projection(
                    generation = 2,
                    maxModelCalls = 1,
                    providerCalls = 1,
                    modelCalls = 0,
                ),
                expectedGeneration = 1,
            )
        }
    }

    @Test
    fun `pure symbolic assertion rejects provider model or renderer use`() {
        assertFailsWithMessage("providerCalls=1") {
            projection(providerCalls = 1).assertPureSymbolic()
        }
        assertFailsWithMessage("modelCalls must not exceed maxModelCalls") {
            projection(modelCalls = 1).assertPureSymbolic()
        }
        assertFailsWithMessage("rendererProvenance") {
            projection(rendererProvenance = "model-fallback/v1").assertPureSymbolic()
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
    fun `normalized dialogue act verified outcome refs and renderer fail closed`() {
        val proposed = projection(
            dialogueAct = "dispatch_required",
            verifiedOutcomeRefs = listOf(
                "zara.verified-outcome/v1:effect:tool-run-7",
                "zara.verified-outcome/v1:outcome:postcondition/process-firefox",
            ),
        )
        SymbolicProjectionContract.validatePayload(proposed)

        assertFailsWithMessage("dialogueAct") {
            SymbolicProjectionContract.validatePayload(
                proposed.copy(dialogueAct = "Clarify Slot")
            )
        }
        assertFailsWithMessage("invalid verified outcome reference") {
            SymbolicProjectionContract.validatePayload(
                proposed.copy(verifiedOutcomeRefs = listOf("effect:unversioned"))
            )
        }
        assertFailsWithMessage("must be unique") {
            SymbolicProjectionContract.validatePayload(
                proposed.copy(verifiedOutcomeRefs = listOf(VERIFIED_EFFECT_REF, VERIFIED_EFFECT_REF))
            )
        }
        assertFailsWithMessage("rendererProvenance") {
            SymbolicProjectionContract.validatePayload(
                proposed.copy(rendererProvenance = "model-fallback/v1")
            )
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
        assertTrue(schema.contains("dialogue_act TEXT NOT NULL DEFAULT 'unknown'"))
        assertTrue(schema.contains("verified_outcome_refs TEXT NOT NULL DEFAULT ''"))
        assertTrue(schema.contains("provider_calls INTEGER NOT NULL DEFAULT 0"))
        assertTrue(schema.contains("model_calls INTEGER NOT NULL DEFAULT 0"))
        assertTrue(schema.contains("FOREIGN KEY(conversation_id)"))
        assertTrue(source.contains("fun PortableConversationStore.saveSymbolicProjection"))
        assertTrue(source.contains("projection_generation = ?"))
        assertTrue(source.contains("terminal turn projection is immutable"))
        assertTrue(source.contains("provider-call ledger rewind rejected"))
        assertTrue(source.contains("model-call ledger rewind rejected"))
        assertTrue(source.contains("SYMBOLIC_RENDERER_ID = \"symbolic-dcg/v1\""))
        assertTrue(source.contains("verifiedOutcomeRefPattern"))
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

    private companion object {
        const val VERIFIED_EFFECT_REF = "zara.verified-outcome/v1:effect:fact-1"
        const val SYMBOLIC_RENDERER = "symbolic-dcg/v1"
    }
}
