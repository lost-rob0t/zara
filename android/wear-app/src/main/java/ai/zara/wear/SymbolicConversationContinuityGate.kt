package ai.zara.wear

import ai.zara.ui.continuity.SymbolicConversationEdgeCodec
import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot

/**
 * Fail-closed freshness gate for a Wear/edge view of one selected conversation.
 *
 * The caller supplies the selected principal/conversation/project scope explicitly. This
 * matters when [current] is null during initial load or process recreation: a
 * valid snapshot from a different principal/conversation/project or an older project
 * generation must not become the first accepted edge truth merely because no previous
 * in-memory projection is available yet.
 *
 * The caller owns the current UI projection. This gate never persists chat state
 * and never grants runtime/tool authority; it only decides whether a newer
 * canonical projection is safe to display over the current one.
 */
object SymbolicConversationContinuityGate {
    fun decodeAccepted(
        expectedPrincipalId: String,
        expectedConversationId: String,
        expectedProjectId: String?,
        expectedProjectGeneration: Long,
        current: SymbolicConversationEdgeSnapshot?,
        encoded: ByteArray,
    ): SymbolicConversationEdgeSnapshot? {
        val incoming = try {
            SymbolicConversationEdgeCodec.decode(encoded)
        } catch (_: IllegalArgumentException) {
            return null
        } catch (_: IllegalStateException) {
            return null
        }
        return incoming.takeIf {
            accepts(
                expectedPrincipalId = expectedPrincipalId,
                expectedConversationId = expectedConversationId,
                expectedProjectId = expectedProjectId,
                expectedProjectGeneration = expectedProjectGeneration,
                current = current,
                incoming = it,
            )
        }
    }

    fun accepts(
        expectedPrincipalId: String,
        expectedConversationId: String,
        expectedProjectId: String?,
        expectedProjectGeneration: Long,
        current: SymbolicConversationEdgeSnapshot?,
        incoming: SymbolicConversationEdgeSnapshot,
    ): Boolean {
        if (expectedPrincipalId.isBlank()) return false
        if (expectedConversationId.isBlank()) return false
        if (expectedProjectId?.isBlank() == true) return false
        if (expectedProjectGeneration < 0L) return false
        if (!isPureSymbolic(incoming)) return false
        if (incoming.principalId != expectedPrincipalId) return false
        if (incoming.conversationId != expectedConversationId) return false
        if (incoming.projectId != expectedProjectId) return false
        if (incoming.projectGeneration < expectedProjectGeneration) return false
        if (current == null) return true
        if (!isPureSymbolic(current)) return false
        if (current.principalId != expectedPrincipalId) return false
        if (current.conversationId != expectedConversationId) return false
        if (incoming.principalId != current.principalId) return false
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
