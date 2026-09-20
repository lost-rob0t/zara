package ai.zara.wear

import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot

/**
 * Fail-closed freshness gate for a Wear/edge view of one selected conversation.
 *
 * The caller owns the current UI projection. This gate never persists chat state
 * and never grants runtime/tool authority; it only decides whether a newer
 * canonical projection is safe to display over the current one.
 */
object SymbolicConversationContinuityGate {
    fun accepts(
        current: SymbolicConversationEdgeSnapshot?,
        incoming: SymbolicConversationEdgeSnapshot,
    ): Boolean {
        if (!isPureSymbolic(incoming)) return false
        if (current == null) return true
        if (!isPureSymbolic(current)) return false
        if (incoming.conversationId != current.conversationId) return false
        if (incoming.projectionGeneration <= current.projectionGeneration) return false
        if (incoming.runtimeGeneration < current.runtimeGeneration) return false

        return if (incoming.projectId == current.projectId) {
            incoming.projectGeneration >= current.projectGeneration
        } else {
            incoming.projectGeneration > current.projectGeneration
        }
    }

    private fun isPureSymbolic(snapshot: SymbolicConversationEdgeSnapshot): Boolean =
        try {
            snapshot.assertPureSymbolic()
            true
        } catch (_: IllegalArgumentException) {
            false
        } catch (_: IllegalStateException) {
            false
        }
}
