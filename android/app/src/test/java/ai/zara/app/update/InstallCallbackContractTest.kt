package ai.zara.app.update

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class InstallCallbackContractTest {
    @Test
    fun `pending installer confirmation is fenced before any activity launch`() {
        val receiver = File(
            "src/main/java/ai/zara/app/update/UpdateInstallReceiver.kt"
        ).readText()

        assertTrue(receiver.contains("InstallCallbackIdentity"))
        assertTrue(receiver.contains("PackageInstaller.EXTRA_SESSION_ID"))
        assertTrue(receiver.contains("handlePendingUserAction(identity, confirmation)"))
        assertFalse(receiver.contains("context.startActivity(confirmation)"))
    }

    @Test
    fun `terminal installer callback carries exact receipt identity`() {
        val receiver = File(
            "src/main/java/ai/zara/app/update/UpdateInstallReceiver.kt"
        ).readText()

        assertTrue(receiver.contains("InstallReceipt.EXTRA_NONCE"))
        assertTrue(receiver.contains("InstallReceipt.EXTRA_SOURCE_SHA"))
        assertTrue(receiver.contains("InstallReceipt.EXTRA_VERSION_NAME"))
        assertTrue(receiver.contains("InstallReceipt.EXTRA_VERSION_CODE"))
        assertTrue(receiver.contains("InstallReceipt.EXTRA_APK_SHA256"))
        assertTrue(receiver.contains("recordInstallStatus(identity, status, message)"))
    }

    @Test
    fun `refresh invalidates old package installer receipt`() {
        val manager = File(
            "src/main/java/ai/zara/app/update/AndroidUpdateManager.kt"
        ).readText()
        val check = manager.substringAfter("fun check()")
            .substringBefore("fun select(")

        assertTrue(check.contains("invalidateActiveInstallForRefresh()"))
        assertTrue(manager.contains("installReceiptStore"))
        assertTrue(manager.contains("abandonSession"))
    }
}
