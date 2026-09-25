package ai.zara.app.watch

import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot
import ai.zara.ui.continuity.WearCompanionContract
import ai.zara.ui.continuity.WearPhoneProvision
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class WearCompanionProvisionSourceTest {
    @Test
    fun latestTruthIsEncodedAsCanonicalProvisionForTheWatch() {
        val source = WearCompanionProvisionSource(
            phoneName = { "Pixel 9 Pro" },
            latestSnapshot = { snapshotFixture() },
        )

        val payload = source.encodeProvision()

        assertNotNull(payload)
        assertEquals(
            WearPhoneProvision(phoneName = "Pixel 9 Pro", snapshot = snapshotFixture()),
            WearCompanionContract.decodeProvision(payload!!),
        )
    }

    @Test
    fun phoneWithoutCanonicalTruthStillAnnouncesReachability() {
        val source = WearCompanionProvisionSource(
            phoneName = { "Pixel 9 Pro" },
            latestSnapshot = { null },
        )

        val payload = source.encodeProvision()

        assertEquals(
            WearPhoneProvision(phoneName = "Pixel 9 Pro", snapshot = null),
            WearCompanionContract.decodeProvision(payload!!),
        )
    }

    @Test
    fun failsClosedWhenTheCanonicalStoreCannotBeRead() {
        val source = WearCompanionProvisionSource(
            phoneName = { "Pixel 9 Pro" },
            latestSnapshot = { throw IllegalStateException("conversation store is closed") },
        )

        assertNull(source.encodeProvision())
    }

    @Test
    fun failsClosedOnBlankPhoneName() {
        val source = WearCompanionProvisionSource(
            phoneName = { " " },
            latestSnapshot = { snapshotFixture() },
        )

        assertNull(source.encodeProvision())
    }

    @Test
    fun payloadsStayBoundedForTheWearDataLayer() {
        val source = WearCompanionProvisionSource(
            phoneName = { "Pixel 9 Pro" },
            latestSnapshot = { snapshotFixture() },
        )

        val payload = source.encodeProvision()!!

        assertTrue(payload.size <= WearCompanionContract.MAX_PROVISION_WIRE_BYTES)
    }

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
}
