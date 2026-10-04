package ai.zara.wear

import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot
import ai.zara.ui.continuity.WearCompanionContract
import ai.zara.ui.continuity.WearPhoneProvision
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class WearCompanionClientTest {
    @Test
    fun reachablePhoneBeforePairingTriggersProvisionRequest() {
        val state = WearCompanionClient.onPhoneReachable(WearCompanionClient.initial(), PHONE_NODE)

        assertTrue(state is WearCompanionLinkState.RequestingProvision)
        assertEquals(PHONE_NODE, (state as WearCompanionLinkState.RequestingProvision).phoneNodeId)
    }

    @Test
    fun validProvisionCompletesAutoPairingWithCanonicalTruth() {
        val snapshot = snapshotFixture()

        val state = WearCompanionClient.onProvision(
            WearCompanionClient.onPhoneReachable(WearCompanionClient.initial(), PHONE_NODE),
            PHONE_NODE,
            encodeProvision(PHONE_NAME, snapshot),
        )

        val paired = state as WearCompanionLinkState.Paired
        assertEquals(PHONE_NODE, paired.phoneNodeId)
        assertEquals(PHONE_NAME, paired.phoneName)
        assertTrue(paired.phoneReachable)
        assertEquals(snapshot, paired.conversation)
        assertEquals(0, paired.rejections)
    }

    @Test
    fun provisionWithoutTruthPairsButShowsNoConversation() {
        val state = WearCompanionClient.onProvision(
            WearCompanionClient.initial(),
            PHONE_NODE,
            encodeProvision(PHONE_NAME, null),
        )

        val paired = state as WearCompanionLinkState.Paired
        assertEquals(PHONE_NAME, paired.phoneName)
        assertNull(paired.conversation)
    }

    @Test
    fun laterTruthFromTheSamePhoneAdvancesThroughTheContinuityGate() {
        val paired = pairedWithTruth()

        val advanced = WearCompanionClient.onProvision(
            paired,
            PHONE_NODE,
            encodeProvision(PHONE_NAME, snapshotFixture(projectionGeneration = 5, runtimeGeneration = 10)),
        ) as WearCompanionLinkState.Paired

        assertEquals(5L, advanced.conversation?.projectionGeneration)
    }

    @Test
    fun staleGenerationsAreRejectedWithoutClearingAcceptedTruth() {
        val paired = pairedWithTruth()

        val state = WearCompanionClient.onProvision(
            paired,
            PHONE_NODE,
            encodeProvision(PHONE_NAME, snapshotFixture(projectionGeneration = 4, runtimeGeneration = 8)),
        ) as WearCompanionLinkState.Paired

        assertEquals(4L, state.conversation?.projectionGeneration)
        assertEquals(1, state.rejections)
    }

    @Test
    fun malformedProvisionIsRejectedWithoutClearingAcceptedTruth() {
        val paired = pairedWithTruth()
        val payload = encodeProvision(PHONE_NAME, snapshotFixture())

        val state = WearCompanionClient.onProvision(
            paired,
            PHONE_NODE,
            payload + byteArrayOf(0x01),
        ) as WearCompanionLinkState.Paired

        assertEquals(4L, state.conversation?.projectionGeneration)
        assertEquals(1, state.rejections)
    }

    @Test
    fun secondPhoneCannotSilentlyTakeOverThePairing() {
        val paired = pairedWithTruth()

        val state = WearCompanionClient.onProvision(
            paired,
            "node:other-phone",
            encodeProvision("Rival", snapshotFixture(projectionGeneration = 9, runtimeGeneration = 20)),
        ) as WearCompanionLinkState.Paired

        assertEquals(PHONE_NODE, state.phoneNodeId)
        assertEquals(PHONE_NAME, state.phoneName)
        assertEquals(4L, state.conversation?.projectionGeneration)
        assertEquals(1, state.rejections)
    }

    @Test
    fun pinnedScopeCannotBeSwitchedByASamePhoneProjection() {
        val paired = pairedWithTruth()

        val state = WearCompanionClient.onProvision(
            paired,
            PHONE_NODE,
            encodeProvision(
                PHONE_NAME,
                snapshotFixture(
                    principalId = "principal:mallory",
                    conversationId = "chat-evil",
                    projectionGeneration = 9,
                    runtimeGeneration = 20,
                ),
            ),
        ) as WearCompanionLinkState.Paired

        assertEquals("local:owner", state.conversation?.principalId)
        assertEquals("chat-7", state.conversation?.conversationId)
        assertEquals(1, state.rejections)
    }

    @Test
    fun requestTimeoutFallsBackToSearchingForAnotherPhone() {
        val state = WearCompanionClient.onRequestTimeout(
            WearCompanionClient.onPhoneReachable(WearCompanionClient.initial(), PHONE_NODE),
        )

        assertTrue(state is WearCompanionLinkState.SearchingForPhone)
    }

    @Test
    fun timeoutAfterPairingKeepsTruthAndLinkState() {
        val paired = pairedWithTruth()

        val state = WearCompanionClient.onRequestTimeout(paired)

        assertEquals(paired, state)
    }

    @Test
    fun reachabilityLossMarksThePairedPhoneOfflineWithoutDroppingTruth() {
        val offline = WearCompanionClient.onPhoneReachabilityLost(pairedWithTruth(), PHONE_NODE)

        val paired = offline as WearCompanionLinkState.Paired
        assertFalse(paired.phoneReachable)
        assertEquals(4L, paired.conversation?.projectionGeneration)
    }

    @Test
    fun reachabilityRestoreMarksThePairedPhoneOnlineAgain() {
        val offline = WearCompanionClient.onPhoneReachabilityLost(pairedWithTruth(), PHONE_NODE)

        val restored = WearCompanionClient.onPhoneReachabilityRestored(offline, PHONE_NODE)

        assertTrue((restored as WearCompanionLinkState.Paired).phoneReachable)
    }

    @Test
    fun reachabilityEventsForOtherNodesDoNotTouchAnExistingPairing() {
        val paired = pairedWithTruth()

        assertEquals(paired, WearCompanionClient.onPhoneReachabilityLost(paired, "node:other"))
        assertEquals(paired, WearCompanionClient.onPhoneReachabilityRestored(paired, "node:other"))
    }

    private fun pairedWithTruth(): WearCompanionLinkState.Paired =
        WearCompanionClient.onProvision(
            WearCompanionClient.onPhoneReachable(WearCompanionClient.initial(), PHONE_NODE),
            PHONE_NODE,
            encodeProvision(PHONE_NAME, snapshotFixture()),
        ) as WearCompanionLinkState.Paired

    private fun encodeProvision(phoneName: String, snapshot: SymbolicConversationEdgeSnapshot?): ByteArray =
        WearCompanionContract.encodeProvision(WearPhoneProvision(phoneName = phoneName, snapshot = snapshot))

    private fun snapshotFixture(
        principalId: String = "local:owner",
        conversationId: String = "chat-7",
        projectionGeneration: Long = 4,
        runtimeGeneration: Long = 9,
    ) = SymbolicConversationEdgeSnapshot(
        principalId = principalId,
        conversationId = conversationId,
        projectionGeneration = projectionGeneration,
        runtimeGeneration = runtimeGeneration,
        projectId = "dotfiles",
        projectGeneration = 3,
        dialogueAct = "expert_answer",
        discourseEntityRefs = listOf("entity:dotfiles", "entity:emacs"),
        unresolvedQuestionRefs = listOf("question:q1"),
        expertEvidenceRefs = listOf("expert:dotfiles:invoke:42", "evidence:sha256:abc"),
        verifiedOutcomeRefs = listOf("zara.verified-outcome/v2:$runtimeGeneration:outcome:postcondition:42"),
        rendererProvenance = "symbolic-dcg/v1",
        providersEnabled = false,
        maxModelCalls = 0,
        modelCalls = 0,
        providerCalls = 0,
    )

    private companion object {
        const val PHONE_NODE = "node:primary-phone"
        const val PHONE_NAME = "Pixel 9 Pro"
    }
}
