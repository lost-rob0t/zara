package ai.zara.wear

import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot

/**
 * Read-only Wear consumer for canonical phone conversation projections.
 *
 * This owns no durable conversation state and grants no runtime/tool authority.
 * Callers must supply the currently selected scope; encoded projections are
 * accepted only through [SymbolicConversationContinuityGate].
 */
class WearSymbolicConversationConsumer(
    initialScope: Scope,
) {
    data class Scope(
        val principalId: String,
        val conversationId: String,
        val projectId: String?,
        val projectGeneration: Long,
    ) {
        init {
            require(principalId.isNotBlank()) { "principalId must not be blank" }
            require(conversationId.isNotBlank()) { "conversationId must not be blank" }
            require(projectId?.isNotBlank() != false) { "projectId must be null or non-blank" }
            require(projectGeneration >= 0L) { "projectGeneration must be >= 0" }
        }
    }

    private var scope: Scope = initialScope
    private var current: SymbolicConversationEdgeSnapshot? = null

    @Synchronized
    fun currentSnapshot(): SymbolicConversationEdgeSnapshot? = current?.detachedCopy()

    @Synchronized
    fun accept(encoded: ByteArray): SymbolicConversationEdgeSnapshot? {
        val accepted = SymbolicConversationContinuityGate.decodeAccepted(
            expectedPrincipalId = scope.principalId,
            expectedConversationId = scope.conversationId,
            expectedProjectId = scope.projectId,
            expectedProjectGeneration = scope.projectGeneration,
            current = current,
            encoded = encoded,
        ) ?: return null

        if (accepted.projectGeneration > scope.projectGeneration) {
            scope = scope.copy(projectGeneration = accepted.projectGeneration)
        }
        current = accepted.detachedCopy()
        return current?.detachedCopy()
    }

    @Synchronized
    fun selectScope(next: Scope) {
        if (next == scope) return

        val sameLogicalScope = next.principalId == scope.principalId &&
            next.conversationId == scope.conversationId &&
            next.projectId == scope.projectId
        require(!sameLogicalScope || next.projectGeneration >= scope.projectGeneration) {
            "projectGeneration must not regress within selected scope"
        }

        scope = next
        current = null
    }

    private fun SymbolicConversationEdgeSnapshot.detachedCopy(): SymbolicConversationEdgeSnapshot =
        copy(
            discourseEntityRefs = discourseEntityRefs.toList(),
            unresolvedQuestionRefs = unresolvedQuestionRefs.toList(),
            expertEvidenceRefs = expertEvidenceRefs.toList(),
            verifiedOutcomeRefs = verifiedOutcomeRefs.toList(),
        )
}
