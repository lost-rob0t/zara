package ai.zara.wear

import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class WearClientPresentationTest {
    @Test
    fun searchingStateIsTruthfulAboutTheMissingPhoneLink() {
        val presentation = WearClientPresentation.from(WearCompanionClient.initial())

        assertEquals("SEARCHING FOR PHONE", presentation.statusLabel)
        assertNull(presentation.phoneName)
        assertNull(presentation.conversationAct)
        assertFalse(presentation.linkLive)
    }

    @Test
    fun requestingStateNeverFakesPairedTruth() {
        val presentation = WearClientPresentation.from(
            WearCompanionLinkState.RequestingProvision(PHONE_NODE),
        )

        assertEquals("PAIRING VIA PHONE", presentation.statusLabel)
        assertNull(presentation.conversationAct)
        assertFalse(presentation.linkLive)
    }

    @Test
    fun pairedWithoutTruthShowsThePhoneButNoConversation() {
        val presentation = WearClientPresentation.from(
            WearCompanionLinkState.Paired(
                phoneNodeId = PHONE_NODE,
                phoneName = "Pixel 9 Pro",
                phoneReachable = true,
                conversation = null,
            ),
        )

        assertEquals("PAIRED", presentation.statusLabel)
        assertEquals("Pixel 9 Pro", presentation.phoneName)
        assertTrue(presentation.linkLive)
        assertNull(presentation.conversationAct)
    }

    @Test
    fun pairedWithTruthProjectsTheCanonicalConversation() {
        val presentation = WearClientPresentation.from(pairedWithTruth())

        assertEquals("PAIRED · LINK LIVE", presentation.statusLabel)
        assertEquals("Pixel 9 Pro", presentation.phoneName)
        assertTrue(presentation.linkLive)
        assertEquals("expert answer", presentation.conversationAct)
        assertEquals(listOf("entity:dotfiles", "entity:emacs"), presentation.discourseEntities)
        assertEquals(listOf("question:q1"), presentation.unresolvedQuestions)
        assertEquals(1, presentation.verifiedOutcomeCount)
        assertEquals("gen 4 · runtime 9", presentation.generationLabel)
    }

    @Test
    fun offlineLinkKeepsAcceptedTruthAndSaysSo() {
        val presentation = WearClientPresentation.from(
            WearCompanionLinkState.Paired(
                phoneNodeId = PHONE_NODE,
                phoneName = "Pixel 9 Pro",
                phoneReachable = false,
                conversation = snapshotFixture(),
            ),
        )

        assertEquals("LINK OFFLINE", presentation.statusLabel)
        assertFalse(presentation.linkLive)
        assertEquals("expert answer", presentation.conversationAct)
    }

    @Test
    fun rejectionsAreVisibleForDiagnosticsButNeverReplaceTruth() {
        val presentation = WearClientPresentation.from(
            pairedWithTruth().copy(rejections = 3),
        )

        assertEquals("3 rejected updates", presentation.rejectionNotice)
        assertEquals("expert answer", presentation.conversationAct)
    }

    private fun pairedWithTruth(): WearCompanionLinkState.Paired =
        WearCompanionLinkState.Paired(
            phoneNodeId = PHONE_NODE,
            phoneName = "Pixel 9 Pro",
            phoneReachable = true,
            conversation = snapshotFixture(),
        )

    private fun snapshotFixture() = SymbolicConversationEdgeSnapshot(
        principalId = "local:owner",
        conversationId = "chat-7",
        projectionGeneration = 4,
        runtimeGeneration = 9,
        projectId = "dotfiles",
        projectGeneration = 3,
        dialogueAct = "expert_answer",
        discourseEntityRefs = listOf("entity:dotfiles", "entity:emacs"),
        unresolvedQuestionRefs = listOf("question:q1"),
        expertEvidenceRefs = listOf("expert:dotfiles:invoke:42", "evidence:sha256:abc"),
        verifiedOutcomeRefs = listOf("zara.verified-outcome/v2:9:outcome:postcondition:42"),
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        modelCalls = 0,
        providerCalls = 0,
    )

    private companion object {
        const val PHONE_NODE = "node:primary-phone"
    }
}
