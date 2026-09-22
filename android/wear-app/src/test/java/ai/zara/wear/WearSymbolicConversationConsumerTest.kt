package ai.zara.wear

import ai.zara.ui.continuity.SymbolicConversationEdgeCodec
import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

class WearSymbolicConversationConsumerTest {
    @Test
    fun acceptsCanonicalScopedTruthAndExposesKnowledgeRefs() {
        val consumer = consumer()
        val snapshot = fixture(
            projectionGeneration = 7,
            runtimeGeneration = 11,
            dialogueAct = "verified",
            discourseEntityRefs = listOf("entity:repo:zara", "entity:person:alice"),
            unresolvedQuestionRefs = listOf("question:target-project"),
            expertEvidenceRefs = listOf("expert:dotfiles:11"),
            verifiedOutcomeRefs = listOf(
                "zara.verified-outcome/v2:11:outcome:postcondition:timer-42",
            ),
        )

        val accepted = consumer.accept(SymbolicConversationEdgeCodec.encode(snapshot))

        assertEquals(snapshot, accepted)
        assertEquals("verified", consumer.currentSnapshot()?.dialogueAct)
        assertEquals(snapshot.discourseEntityRefs, consumer.currentSnapshot()?.discourseEntityRefs)
        assertEquals(snapshot.unresolvedQuestionRefs, consumer.currentSnapshot()?.unresolvedQuestionRefs)
        assertEquals(snapshot.expertEvidenceRefs, consumer.currentSnapshot()?.expertEvidenceRefs)
        assertEquals(snapshot.verifiedOutcomeRefs, consumer.currentSnapshot()?.verifiedOutcomeRefs)
        assertEquals(0L, consumer.currentSnapshot()?.maxModelCalls)
        assertEquals(0L, consumer.currentSnapshot()?.modelCalls)
        assertEquals(0L, consumer.currentSnapshot()?.providerCalls)
    }

    @Test
    fun rejectsWrongScopeStaleGenerationAndProviderAuthority() {
        val consumer = consumer()
        val accepted = fixture(projectionGeneration = 5, runtimeGeneration = 8)
        assertEquals(accepted, consumer.accept(SymbolicConversationEdgeCodec.encode(accepted)))

        assertNull(
            consumer.accept(
                SymbolicConversationEdgeCodec.encode(
                    fixture(
                        projectId = "other",
                        projectGeneration = 5,
                        projectionGeneration = 6,
                        runtimeGeneration = 9,
                    ),
                ),
            ),
        )
        assertNull(
            consumer.accept(
                SymbolicConversationEdgeCodec.encode(
                    fixture(projectionGeneration = 5, runtimeGeneration = 9),
                ),
            ),
        )
        assertNull(
            consumer.accept(
                SymbolicConversationEdgeCodec.encode(
                    fixture(
                        projectionGeneration = 6,
                        runtimeGeneration = 9,
                        providersEnabled = true,
                    ),
                ),
            ),
        )
        assertEquals(accepted, consumer.currentSnapshot())
    }

    @Test
    fun scopeSwitchDropsOldViewAndRequiresNewProjectFloor() {
        val consumer = consumer()
        assertEquals(
            4L,
            consumer.accept(
                SymbolicConversationEdgeCodec.encode(fixture(projectionGeneration = 4)),
            )?.projectionGeneration,
        )

        consumer.selectScope(
            WearSymbolicConversationConsumer.Scope(
                principalId = PRINCIPAL,
                conversationId = CONVERSATION,
                projectId = "project:two",
                projectGeneration = 9,
            ),
        )
        assertNull(consumer.currentSnapshot())

        assertNull(
            consumer.accept(
                SymbolicConversationEdgeCodec.encode(
                    fixture(
                        projectId = "project:two",
                        projectGeneration = 8,
                        projectionGeneration = 5,
                    ),
                ),
            ),
        )
        val fresh = fixture(
            projectId = "project:two",
            projectGeneration = 9,
            projectionGeneration = 5,
            runtimeGeneration = 2,
        )
        assertEquals(fresh, consumer.accept(SymbolicConversationEdgeCodec.encode(fresh)))
    }

    private fun consumer() = WearSymbolicConversationConsumer(
        WearSymbolicConversationConsumer.Scope(
            principalId = PRINCIPAL,
            conversationId = CONVERSATION,
            projectId = PROJECT,
            projectGeneration = PROJECT_GENERATION,
        ),
    )

    private fun fixture(
        principalId: String = PRINCIPAL,
        conversationId: String = CONVERSATION,
        projectId: String? = PROJECT,
        projectGeneration: Long = PROJECT_GENERATION,
        projectionGeneration: Long = 1,
        runtimeGeneration: Long = 1,
        dialogueAct: String = "expert_answer",
        discourseEntityRefs: List<String> = listOf("entity:repo:zara"),
        unresolvedQuestionRefs: List<String> = emptyList(),
        expertEvidenceRefs: List<String> = listOf("expert:dotfiles:1"),
        verifiedOutcomeRefs: List<String> = emptyList(),
        providersEnabled: Boolean = false,
    ) = SymbolicConversationEdgeSnapshot(
        principalId = principalId,
        conversationId = conversationId,
        projectionGeneration = projectionGeneration,
        runtimeGeneration = runtimeGeneration,
        projectId = projectId,
        projectGeneration = projectGeneration,
        dialogueAct = dialogueAct,
        discourseEntityRefs = discourseEntityRefs,
        unresolvedQuestionRefs = unresolvedQuestionRefs,
        expertEvidenceRefs = expertEvidenceRefs,
        verifiedOutcomeRefs = verifiedOutcomeRefs,
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = providersEnabled,
        maxModelCalls = 0,
        modelCalls = 0,
        providerCalls = 0,
    )

    private companion object {
        const val PRINCIPAL = "principal:local"
        const val CONVERSATION = "conversation:42"
        const val PROJECT = "project:one"
        const val PROJECT_GENERATION = 4L
    }
}
