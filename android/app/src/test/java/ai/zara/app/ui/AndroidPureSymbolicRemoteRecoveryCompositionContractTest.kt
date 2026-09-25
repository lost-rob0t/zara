package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * P0 reconciliation contract: pure-symbolic Android chat and the landed remote-recovery/telemetry
 * slice must coexist in one MainActivity and one emulator gate.
 *
 * This is intentionally a source-composition gate. It prevents resolving the current master/PR
 * collision by choosing either side wholesale and silently dropping the other side's behavior.
 */
class AndroidPureSymbolicRemoteRecoveryCompositionContractTest {
    @Test
    fun `main activity preserves canonical symbolic owner and remote failure recovery`() {
        val mainActivity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertTrue(
            "symbolic chat must keep the canonical zara.db-backed conversation owner",
            mainActivity.contains("CanonicalConversationStore("),
        )
        assertTrue(
            "symbolic chat must pass one PortableConversationStore into the Android symbolic factory",
            mainActivity.contains("PortableConversationStore(this)"),
        )
        assertTrue(
            "remote recovery telemetry must keep the chat-turn breadcrumb on the same UI path",
            mainActivity.contains("recordChatBreadcrumb(\"chat.turn.begin\""),
        )
        assertTrue(
            "remote failure classification must survive symbolic integration",
            mainActivity.contains("TurnFailures.from("),
        )
        assertTrue(
            "the composed UI must retain retry without routing pure-symbolic turns through providers",
            mainActivity.contains("onRetryTurn ="),
        )
        assertTrue(
            "the composed UI must retain explicit remote reconnect action",
            mainActivity.contains("onReconnectRemote ="),
        )
        assertTrue(
            "diagnostics entrypoint from the landed remote slice must remain wired",
            mainActivity.contains("onOpenDiagnostics ="),
        )
    }

    @Test
    fun `emulator gate proves both installed pure symbolic continuity and remote recovery`() {
        val gate = File("../../scripts/test-android-emulator-install.sh").readText()

        assertTrue(
            "installed APK gate must execute the pure-symbolic restart continuity acceptance",
            gate.contains("device_pure_symbolic_acceptance.py"),
        )
        assertTrue(
            "installed APK gate must keep the landed remote-recovery acceptance",
            gate.contains("device_remote_recovery_acceptance.py"),
        )
    }
}
