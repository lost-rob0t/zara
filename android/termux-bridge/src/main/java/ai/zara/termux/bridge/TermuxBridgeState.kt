package ai.zara.termux.bridge

enum class TermuxBridgePhase {
    TERMUX_MISSING,
    RUN_COMMAND_PERMISSION_REQUIRED,
    EXTERNAL_APPS_CHECK_REQUIRED,
    EXTERNAL_APPS_OR_TERMUX_SETUP_REQUIRED,
    READY,
}

data class TermuxBridgeState(
    val phase: TermuxBridgePhase,
    val termuxInstalled: Boolean,
    val runCommandPermissionGranted: Boolean,
    val probeAttempted: Boolean,
    val probeSucceeded: Boolean,
)

object TermuxBridgeStateMachine {
    fun resolve(
        termuxInstalled: Boolean,
        runCommandPermissionGranted: Boolean,
        probeAttempted: Boolean,
        probeSucceeded: Boolean,
    ): TermuxBridgeState {
        val phase = when {
            !termuxInstalled -> TermuxBridgePhase.TERMUX_MISSING
            !runCommandPermissionGranted -> TermuxBridgePhase.RUN_COMMAND_PERMISSION_REQUIRED
            !probeAttempted -> TermuxBridgePhase.EXTERNAL_APPS_CHECK_REQUIRED
            !probeSucceeded -> TermuxBridgePhase.EXTERNAL_APPS_OR_TERMUX_SETUP_REQUIRED
            else -> TermuxBridgePhase.READY
        }
        return TermuxBridgeState(
            phase = phase,
            termuxInstalled = termuxInstalled,
            runCommandPermissionGranted = runCommandPermissionGranted,
            probeAttempted = probeAttempted,
            probeSucceeded = probeSucceeded,
        )
    }
}
