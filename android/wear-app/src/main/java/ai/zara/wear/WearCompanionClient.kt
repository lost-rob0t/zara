package ai.zara.wear

import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot
import ai.zara.ui.continuity.WearCompanionContract
import ai.zara.ui.continuity.WearPhoneProvision

/**
 * Truthful link state for the watch's phone-assisted pairing.
 *
 * [SearchingForPhone] and [RequestingProvision] never render conversation
 * truth; truth exists only after a phone provision is accepted. Reachability
 * loss keeps accepted truth but marks the link offline.
 */
sealed interface WearCompanionLinkState {
    data object SearchingForPhone : WearCompanionLinkState

    data class RequestingProvision(
        val phoneNodeId: String,
    ) : WearCompanionLinkState

    data class Paired(
        val phoneNodeId: String,
        val phoneName: String,
        val phoneReachable: Boolean,
        val conversation: SymbolicConversationEdgeSnapshot?,
        val rejections: Int = 0,
    ) : WearCompanionLinkState
}

/**
 * Pure state machine for Wear auto pairing over the phone companion link.
 *
 * The first accepted provision pins the pairing to that phone node and to the
 * delivered principal/conversation scope. Every later projection must pass
 * [SymbolicConversationContinuityGate]; malformed, stale, wrong-scope, or
 * foreign-phone payloads are rejected without clearing accepted truth.
 */
object WearCompanionClient {
    fun initial(): WearCompanionLinkState = WearCompanionLinkState.SearchingForPhone

    fun onPhoneReachable(
        state: WearCompanionLinkState,
        phoneNodeId: String,
    ): WearCompanionLinkState =
        when (state) {
            is WearCompanionLinkState.Paired ->
                if (state.phoneNodeId == phoneNodeId) {
                    state.copy(phoneReachable = true)
                } else {
                    state
                }
            else -> WearCompanionLinkState.RequestingProvision(phoneNodeId)
        }

    fun onProvision(
        state: WearCompanionLinkState,
        phoneNodeId: String,
        payload: ByteArray,
    ): WearCompanionLinkState {
        if (state is WearCompanionLinkState.Paired && state.phoneNodeId != phoneNodeId) {
            return rejectForeignPhone(state)
        }
        val provision = decodeProvision(payload) ?: return reject(state)
        val incoming = provision.snapshot
        return when (state) {
            is WearCompanionLinkState.Paired -> acceptAgainstPinnedScope(state, provision, incoming)
            else -> acceptFirstProvision(phoneNodeId, provision, incoming)
        }
    }

    fun onRequestTimeout(state: WearCompanionLinkState): WearCompanionLinkState =
        when (state) {
            is WearCompanionLinkState.RequestingProvision -> WearCompanionLinkState.SearchingForPhone
            else -> state
        }

    fun onPhoneReachabilityLost(
        state: WearCompanionLinkState,
        phoneNodeId: String,
    ): WearCompanionLinkState =
        (state as? WearCompanionLinkState.Paired)
            ?.takeIf { it.phoneNodeId == phoneNodeId }
            ?.copy(phoneReachable = false)
            ?: state

    fun onPhoneReachabilityRestored(
        state: WearCompanionLinkState,
        phoneNodeId: String,
    ): WearCompanionLinkState = onPhoneReachable(state, phoneNodeId)

    private fun acceptFirstProvision(
        phoneNodeId: String,
        provision: WearPhoneProvision,
        incoming: SymbolicConversationEdgeSnapshot?,
    ): WearCompanionLinkState.Paired {
        val accepted = incoming?.takeIf { adoptsInitialScope(it) }
        return WearCompanionLinkState.Paired(
            phoneNodeId = phoneNodeId,
            phoneName = provision.phoneName,
            phoneReachable = true,
            conversation = accepted,
        )
    }

    private fun acceptAgainstPinnedScope(
        state: WearCompanionLinkState.Paired,
        provision: WearPhoneProvision,
        incoming: SymbolicConversationEdgeSnapshot?,
    ): WearCompanionLinkState.Paired {
        val current = state.conversation ?: return adoptScopeAfterEmptyPairing(state, provision, incoming)
        if (incoming == null) return state
        val accepted = SymbolicConversationContinuityGate.accepts(
            expectedPrincipalId = current.principalId,
            expectedConversationId = current.conversationId,
            current = current,
            incoming = incoming,
        )
        return if (accepted) {
            state.copy(conversation = incoming)
        } else {
            state.copy(rejections = state.rejections + 1)
        }
    }

    private fun adoptScopeAfterEmptyPairing(
        state: WearCompanionLinkState.Paired,
        provision: WearPhoneProvision,
        incoming: SymbolicConversationEdgeSnapshot?,
    ): WearCompanionLinkState.Paired =
        if (incoming != null && adoptsInitialScope(incoming)) {
            state.copy(phoneName = provision.phoneName, conversation = incoming)
        } else {
            state
        }

    private fun adoptsInitialScope(incoming: SymbolicConversationEdgeSnapshot): Boolean =
        SymbolicConversationContinuityGate.accepts(
            expectedPrincipalId = incoming.principalId,
            expectedConversationId = incoming.conversationId,
            current = null,
            incoming = incoming,
        )

    private fun rejectForeignPhone(state: WearCompanionLinkState.Paired): WearCompanionLinkState.Paired =
        state.copy(rejections = state.rejections + 1)

    private fun reject(state: WearCompanionLinkState): WearCompanionLinkState =
        when (state) {
            is WearCompanionLinkState.Paired -> state.copy(rejections = state.rejections + 1)
            else -> state
        }

    private fun decodeProvision(payload: ByteArray): WearPhoneProvision? =
        try {
            WearCompanionContract.decodeProvision(payload)
        } catch (_: IllegalArgumentException) {
            null
        } catch (_: IllegalStateException) {
            null
        }
}
