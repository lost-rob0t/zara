package ai.zara.app.expert

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PureSymbolicExpertConversationProjectionTest {
    @Test
    fun readOnlyResultProjectsCanonicalSummaryAndSingleEvidence() {
        val projected = PureSymbolicExpertConversationProjection.from(
            result(
                data = mapOf("summary" to "diagnosis(alex,flu)"),
                evidenceRefs = listOf("evidence:diagnosis:42"),
            ),
        )

        assertEquals("diagnosis(alex,flu)", projected.summary)
        assertEquals("evidence:diagnosis:42", projected.evidenceRef)
    }

    @Test
    fun readOnlyResultWithAmbiguousEvidenceFailsClosed() {
        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertConversationProjection.from(
                result(
                    data = mapOf("summary" to "diagnosis(alex,flu)"),
                    evidenceRefs = listOf("evidence:diagnosis:42", "evidence:rule:viral"),
                ),
            )
        }
    }

    @Test
    fun effectfulResultProjectsVerifiedOutcomeReference() {
        val verifiedOutcomeRef = "zara.verified-outcome/v2:11:outcome:calendar-created"
        val projected = PureSymbolicExpertConversationProjection.from(
            result(
                data = mapOf(
                    "summary" to "Created the calendar event.",
                    "verified" to true,
                    "verified_outcome_ref" to verifiedOutcomeRef,
                    "postcondition_evidence" to mapOf(
                        "receipt_ref" to verifiedOutcomeRef,
                        "source_generation" to 11L,
                    ),
                ),
                evidenceRefs = listOf("evidence:request:42", verifiedOutcomeRef),
                effectReceipts = listOf(mapOf("receipt_id" to "effect:calendar:42")),
            ),
        )

        assertEquals("Created the calendar event.", projected.summary)
        assertEquals(verifiedOutcomeRef, projected.evidenceRef)
    }

    @Test
    fun effectfulResultCannotProjectEvidenceOutsideCanonicalRefs() {
        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertConversationProjection.from(
                result(
                    data = mapOf(
                        "summary" to "Created the calendar event.",
                        "verified_outcome_ref" to "zara.verified-outcome/v2:11:outcome:calendar-created",
                    ),
                    evidenceRefs = listOf("evidence:request:42"),
                    effectReceipts = listOf(mapOf("receipt_id" to "effect:calendar:42")),
                ),
            )
        }
    }

    @Test
    fun missingConversationSummaryFailsClosed() {
        assertThrows(IllegalArgumentException::class.java) {
            PureSymbolicExpertConversationProjection.from(
                result(
                    data = emptyMap(),
                    evidenceRefs = listOf("evidence:diagnosis:42"),
                ),
            )
        }
    }

    private fun result(
        data: Map<String, Any?>,
        evidenceRefs: List<String>,
        effectReceipts: List<Map<String, Any?>> = emptyList(),
    ): ExpertResult = ExpertResult(
        protocol = ZARA_EXPERT_PROTOCOL,
        requestId = "turn:42",
        invocationId = "invocation:diagnosis:42",
        activationId = "act:0123456789abcdef0123456789abcdef",
        expertId = "zara:expert/diagnosis",
        expertVersion = "1.0.0",
        manifestDigest = "sha256:diagnosis",
        expertOperation = "diagnose",
        resolvedRegistryGeneration = 7L,
        resolvedRuntimeGeneration = 11L,
        verdict = ExpertVerdict.SUCCEEDED,
        data = data,
        evidenceRefs = evidenceRefs,
        usage = mapOf(
            "provider_calls" to 0,
            "model_calls" to 0,
        ),
        effectReceipts = effectReceipts,
    )
}
