package ai.zara.wear

/**
 * Pure watch-surface projection of the companion link state.
 *
 * Every state renders a truthful label; conversation fields appear only when
 * canonical truth has been accepted, and rejected updates surface as a muted
 * notice instead of replacing accepted truth.
 */
data class WearClientPresentation(
    val statusLabel: String,
    val phoneName: String?,
    val linkLive: Boolean,
    val conversationAct: String?,
    val discourseEntities: List<String> = emptyList(),
    val unresolvedQuestions: List<String> = emptyList(),
    val verifiedOutcomeCount: Int = 0,
    val generationLabel: String? = null,
    val rejectionNotice: String? = null,
) {
    companion object {
        fun from(state: WearCompanionLinkState): WearClientPresentation = when (state) {
            WearCompanionLinkState.SearchingForPhone ->
                WearClientPresentation(
                    statusLabel = "SEARCHING FOR PHONE",
                    phoneName = null,
                    linkLive = false,
                    conversationAct = null,
                )
            is WearCompanionLinkState.RequestingProvision ->
                WearClientPresentation(
                    statusLabel = "PAIRING VIA PHONE",
                    phoneName = null,
                    linkLive = false,
                    conversationAct = null,
                )
            is WearCompanionLinkState.Paired -> {
                val conversation = state.conversation
                WearClientPresentation(
                    statusLabel = when {
                        state.phoneReachable && conversation != null -> "PAIRED · LINK LIVE"
                        state.phoneReachable -> "PAIRED"
                        else -> "LINK OFFLINE"
                    },
                    phoneName = state.phoneName,
                    linkLive = state.phoneReachable,
                    conversationAct = conversation?.dialogueAct?.replace('_', ' '),
                    discourseEntities = conversation?.discourseEntityRefs.orEmpty(),
                    unresolvedQuestions = conversation?.unresolvedQuestionRefs.orEmpty(),
                    verifiedOutcomeCount = conversation?.verifiedOutcomeRefs?.size ?: 0,
                    generationLabel = conversation?.let { "gen ${it.projectionGeneration} · runtime ${it.runtimeGeneration}" },
                    rejectionNotice = state.rejections.takeIf { it > 0 }?.let { "$it rejected updates" },
                )
            }
        }
    }
}
