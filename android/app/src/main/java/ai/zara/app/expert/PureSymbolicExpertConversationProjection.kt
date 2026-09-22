package ai.zara.app.expert

/**
 * Conversation-facing projection of an already-admitted canonical expert result.
 *
 * This helper owns no expert authority and performs no invocation. Callers must obtain [result]
 * from [PureSymbolicExpertInvocationAdapter] first so ZARA-EXPERT/1 identity, generation, budget,
 * zero-model usage, capability/effect, and postcondition evidence checks have already passed.
 *
 * The symbolic dialogue contract carries one stable evidence reference with an expert answer. For
 * effectful results that reference is the canonical verified outcome. For read-only results Zara
 * currently requires one unambiguous canonical evidence reference and otherwise fails closed.
 */
internal data class PureSymbolicExpertConversationResult(
    val summary: String,
    val evidenceRef: String,
)

internal object PureSymbolicExpertConversationProjection {
    fun from(result: ExpertResult): PureSymbolicExpertConversationResult {
        require(result.verdict == ExpertVerdict.SUCCEEDED) {
            "Only an admitted successful expert result may enter symbolic conversation projection"
        }
        require(result.errorCode == null && result.errorMessage.isEmpty()) {
            "Successful expert conversation projection cannot carry an error"
        }

        val summary = result.data["summary"] as? String
            ?: throw IllegalArgumentException(
                "Canonical expert result is missing conversation summary data",
            )
        require(summary.isNotBlank()) {
            "Canonical expert conversation summary must not be blank"
        }
        require(summary.length <= MAX_EXPERT_SUMMARY_CHARS) {
            "Canonical expert conversation summary exceeds the symbolic renderer bound"
        }
        require(summary.none { character ->
            character.isISOControl() && character !in charArrayOf('\n', '\r', '\t')
        }) {
            "Canonical expert conversation summary contains unsupported control characters"
        }

        val evidenceRef = if (result.effectReceipts.isNotEmpty()) {
            val verifiedOutcomeRef = result.data["verified_outcome_ref"] as? String
                ?: throw IllegalArgumentException(
                    "Effectful expert conversation result is missing verified outcome evidence",
                )
            require(verifiedOutcomeRef in result.evidenceRefs) {
                "Verified outcome evidence is not present in canonical expert evidence"
            }
            verifiedOutcomeRef
        } else {
            require(result.evidenceRefs.size == 1) {
                "Read-only expert conversation result requires exactly one canonical evidence reference"
            }
            result.evidenceRefs.single()
        }

        require(evidenceRef.isNotBlank()) {
            "Canonical expert conversation evidence must not be blank"
        }
        require(evidenceRef.length <= MAX_EXPERT_EVIDENCE_CHARS) {
            "Canonical expert conversation evidence exceeds the symbolic renderer bound"
        }
        require(evidenceRef.none(Char::isISOControl)) {
            "Canonical expert conversation evidence contains control characters"
        }

        return PureSymbolicExpertConversationResult(
            summary = summary,
            evidenceRef = evidenceRef,
        )
    }

    private const val MAX_EXPERT_SUMMARY_CHARS = 1_024
    private const val MAX_EXPERT_EVIDENCE_CHARS = 256
}
