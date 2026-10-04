package ai.zara.app.watch

import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot
import ai.zara.ui.continuity.WearCompanionContract
import ai.zara.ui.continuity.WearPhoneProvision
import ai.zara.ui.health.HealthGoalTarget

/**
 * Builds the bounded phone -> Wear auto-provisioning payload from canonical truth.
 *
 * Fails closed: any unreadable store, blank identity, or non-pure-symbolic
 * projection suppresses the provision instead of shipping partial or unsafe
 * state to the watch.
 */
class WearCompanionProvisionSource(
    private val phoneName: () -> String,
    private val latestSnapshot: () -> SymbolicConversationEdgeSnapshot?,
    private val healthGoals: () -> List<HealthGoalTarget> = { emptyList() },
) {
    fun encodeProvision(): ByteArray? =
        try {
            WearCompanionContract.encodeProvision(
                WearPhoneProvision(
                    phoneName = phoneName(),
                    snapshot = latestSnapshot(),
                    healthGoals = healthGoals(),
                ),
            )
        } catch (_: IllegalArgumentException) {
            null
        } catch (_: IllegalStateException) {
            null
        }
}
