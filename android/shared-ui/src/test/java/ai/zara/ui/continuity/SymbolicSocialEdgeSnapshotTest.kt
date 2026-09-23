package ai.zara.ui.continuity

import org.junit.Test

class SymbolicSocialEdgeSnapshotTest {
    @Test
    fun helpAndAcknowledgementRemainCanonicalPureSymbolicActs() {
        listOf("help", "acknowledgement").forEach { act ->
            SymbolicConversationEdgeSnapshot(
                principalId = "principal:test",
                conversationId = "conversation:test",
                projectionGeneration = 1,
                runtimeGeneration = 1,
                dialogueAct = act,
                rendererProvenance = "symbolic-dcg/v1",
                providersEnabled = false,
                maxModelCalls = 0,
                modelCalls = 0,
                providerCalls = 0,
            ).assertPureSymbolic()
        }
    }
}
