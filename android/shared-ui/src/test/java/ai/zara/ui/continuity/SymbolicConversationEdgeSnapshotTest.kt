package ai.zara.ui.continuity

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class SymbolicConversationEdgeSnapshotTest {
    @Test
    fun roundTripsPureSymbolicProjectionAndEvidenceReferences() {
        val snapshot = fixture()
        val encoded = SymbolicConversationEdgeCodec.encode(snapshot)
        val decoded = SymbolicConversationEdgeCodec.decode(encoded)
        assertEquals(snapshot, decoded)
        decoded.assertPureSymbolic()
        assertTrue(encoded.size <= SymbolicConversationEdgeCodec.MAX_WIRE_BYTES)
    }

    @Test
    fun pureSymbolicRequiresProvidersDisabledAndZeroModelBudget() {
        assertFails("providers enabled") { fixture().copy(providersEnabled = true).assertPureSymbolic() }
        assertFails("max model calls must be 0") { fixture().copy(maxModelCalls = 1).assertPureSymbolic() }
        assertFails("model calls") { fixture().copy(maxModelCalls = 1, modelCalls = 1).assertPureSymbolic() }
        assertFails("provider calls") { fixture().copy(providerCalls = 1).assertPureSymbolic() }
    }

    @Test
    fun pureSymbolicRequiresCanonicalDialogueActAndRenderer() {
        assertFails("dialogueAct is not a ZARA-SYMBOLIC-DIALOGUE/1 act") {
            fixture().copy(dialogueAct = "explain").assertPureSymbolic()
        }
        assertFails("rendererProvenance must be symbolic-dcg/v1") {
            fixture().copy(rendererProvenance = "model-fallback/v1").assertPureSymbolic()
        }
        assertFails("rendererProvenance must be symbolic-dcg/v1") {
            fixture().copy(rendererProvenance = "").assertPureSymbolic()
        }
    }

    @Test
    fun principalScopeIsRequired() {
        assertFails("principalId must not be blank") { fixture().copy(principalId = "").validate() }
    }

    @Test
    fun modelUsageCannotExceedDeclaredBudget() {
        assertFails("exceed declared max model calls") { fixture().copy(maxModelCalls = 1, modelCalls = 2).validate() }
    }

    @Test
    fun codecRejectsTrailingBytesAndUnboundedEvidence() {
        val encoded = SymbolicConversationEdgeCodec.encode(fixture())
        assertFails("trailing bytes") { SymbolicConversationEdgeCodec.decode(encoded + byteArrayOf(0x01)) }
        assertFails("expertEvidenceRefs") {
            SymbolicConversationEdgeCodec.encode(
                fixture().copy(expertEvidenceRefs = List(SymbolicConversationEdgeSnapshot.MAX_REFS + 1) { "e:$it" }),
            )
        }
    }

    private fun fixture() = SymbolicConversationEdgeSnapshot(
        principalId = "principal:alice",
        conversationId = "chat-7",
        projectionGeneration = 4,
        runtimeGeneration = 9,
        projectId = "dotfiles",
        projectGeneration = 3,
        dialogueAct = "expert_answer",
        discourseEntityRefs = listOf("entity:dotfiles", "entity:emacs"),
        unresolvedQuestionRefs = listOf("question:q1"),
        expertEvidenceRefs = listOf("expert:dotfiles:invoke:42", "evidence:sha256:abc"),
        verifiedOutcomeRefs = listOf("outcome:postcondition:42"),
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        modelCalls = 0,
        providerCalls = 0,
    )

    private fun assertFails(expectedMessage: String, block: () -> Unit) {
        try {
            block()
            fail("expected failure containing: $expectedMessage")
        } catch (error: IllegalArgumentException) {
            assertTrue(error.message.orEmpty().contains(expectedMessage))
        } catch (error: IllegalStateException) {
            assertTrue(error.message.orEmpty().contains(expectedMessage))
        }
    }
}
