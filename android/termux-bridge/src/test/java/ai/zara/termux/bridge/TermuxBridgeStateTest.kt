package ai.zara.termux.bridge

import org.junit.Assert.assertEquals
import org.junit.Test

class TermuxBridgeStateTest {
    @Test
    fun missingTermuxIsExplicit() {
        assertEquals(
            TermuxBridgePhase.TERMUX_MISSING,
            TermuxBridgeStateMachine.resolve(
                termuxInstalled = false,
                runCommandPermissionGranted = false,
                probeAttempted = false,
                probeSucceeded = false,
            ).phase,
        )
    }

    @Test
    fun installedTermuxStillRequiresRunCommandPermission() {
        assertEquals(
            TermuxBridgePhase.RUN_COMMAND_PERMISSION_REQUIRED,
            TermuxBridgeStateMachine.resolve(
                termuxInstalled = true,
                runCommandPermissionGranted = false,
                probeAttempted = false,
                probeSucceeded = false,
            ).phase,
        )
    }

    @Test
    fun grantedPermissionDoesNotPretendExternalAppsAreConfigured() {
        assertEquals(
            TermuxBridgePhase.EXTERNAL_APPS_CHECK_REQUIRED,
            TermuxBridgeStateMachine.resolve(
                termuxInstalled = true,
                runCommandPermissionGranted = true,
                probeAttempted = false,
                probeSucceeded = false,
            ).phase,
        )
    }

    @Test
    fun failedProbeReportsSetupRequired() {
        assertEquals(
            TermuxBridgePhase.EXTERNAL_APPS_OR_TERMUX_SETUP_REQUIRED,
            TermuxBridgeStateMachine.resolve(
                termuxInstalled = true,
                runCommandPermissionGranted = true,
                probeAttempted = true,
                probeSucceeded = false,
            ).phase,
        )
    }

    @Test
    fun successfulProbeIsTheOnlyReadyState() {
        assertEquals(
            TermuxBridgePhase.READY,
            TermuxBridgeStateMachine.resolve(
                termuxInstalled = true,
                runCommandPermissionGranted = true,
                probeAttempted = true,
                probeSucceeded = true,
            ).phase,
        )
    }
}
