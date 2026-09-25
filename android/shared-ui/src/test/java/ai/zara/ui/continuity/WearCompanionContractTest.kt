package ai.zara.ui.continuity

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class WearCompanionContractTest {
    @Test
    fun roundTripsPhoneProvisionWithCanonicalSnapshot() {
        val provision = WearPhoneProvision(
            phoneName = "Pixel 9 Pro",
            snapshot = fixture(),
        )

        val encoded = WearCompanionContract.encodeProvision(provision)
        val decoded = WearCompanionContract.decodeProvision(encoded)

        assertEquals(provision, decoded)
        assertTrue(encoded.size <= WearCompanionContract.MAX_PROVISION_WIRE_BYTES)
    }

    @Test
    fun roundTripsPhoneReachableWithoutCanonicalTruth() {
        val provision = WearPhoneProvision(
            phoneName = "Pixel 9 Pro",
            snapshot = null,
        )

        val encoded = WearCompanionContract.encodeProvision(provision)
        val decoded = WearCompanionContract.decodeProvision(encoded)

        assertEquals(provision, decoded)
        assertTrue(encoded.size <= WearCompanionContract.MAX_PROVISION_WIRE_BYTES)
    }

    @Test
    fun provisionPathsAndCapabilityAreCanonical() {
        assertEquals("zara_watch", WearCompanionContract.CAPABILITY_WATCH)
        assertEquals("zara_phone", WearCompanionContract.CAPABILITY_PHONE)
        assertEquals("/zara/watch/hello", WearCompanionContract.PATH_WATCH_HELLO)
        assertEquals("/zara/phone/provision", WearCompanionContract.PATH_PHONE_PROVISION)
    }

    @Test
    fun encodeFailsClosedOnNonPureSymbolicSnapshot() {
        assertFails("providers enabled") {
            WearCompanionContract.encodeProvision(
                WearPhoneProvision(
                    phoneName = "Pixel 9 Pro",
                    snapshot = fixture().copy(providersEnabled = true, maxModelCalls = 1),
                ),
            )
        }
    }

    @Test
    fun decodeRejectsTrailingBytesBadMagicAndTruncation() {
        val encoded = WearCompanionContract.encodeProvision(
            WearPhoneProvision(phoneName = "Pixel 9 Pro", snapshot = fixture()),
        )
        assertFails("trailing bytes") {
            WearCompanionContract.decodeProvision(encoded + byteArrayOf(0x01))
        }
        assertFails("magic is invalid") {
            WearCompanionContract.decodeProvision(encoded.copyOf().also { it[3] = 'X'.code.toByte() })
        }
        assertFails("truncated or malformed") {
            WearCompanionContract.decodeProvision(encoded.copyOf(encoded.size - 4))
        }
    }

    @Test
    fun decodeRejectsBlankOversizedAndControlCharacterPhoneNames() {
        assertFails("phoneName must not be blank") {
            WearCompanionContract.encodeProvision(WearPhoneProvision(phoneName = " ", snapshot = fixture()))
        }
        assertFails("phoneName exceeds") {
            WearCompanionContract.encodeProvision(
                WearPhoneProvision(phoneName = "p".repeat(WearCompanionContract.MAX_PHONE_NAME_CHARS + 1), snapshot = fixture()),
            )
        }
        assertFails("control characters") {
            WearCompanionContract.encodeProvision(
                WearPhoneProvision(phoneName = "Pixel\u00079", snapshot = fixture()),
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
        verifiedOutcomeRefs = listOf("zara.verified-outcome/v2:9:outcome:postcondition:42"),
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
