package ai.zara.app.update

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class UpdateManagerContractTest {
    @Test
    fun `updater exposes master fast lane and selectable version catalog`() {
        val source = File(
            "src/main/java/ai/zara/app/update/AndroidUpdateManager.kt"
        ).readText()

        assertTrue(source.contains("zara-latest.manifest.txt"))
        assertTrue(source.contains("UpdateChannel.Master"))
        assertTrue(source.contains("fun select("))
        assertTrue(source.contains("choices ="))
        assertTrue(source.contains("Master (fastest green)"))
    }

    @Test
    fun `install request queues package copy on updater worker`() {
        val source = File(
            "src/main/java/ai/zara/app/update/AndroidUpdateManager.kt"
        ).readText()
        val request = source.substringAfter("fun requestInstall()").substringBefore("fun recordInstallStatus")
        val worker = source.substringAfter("private fun installVerifiedUpdate")
            .substringBefore("private fun releaseCandidate")

        assertTrue(request.contains("submit"))
        assertFalse(request.contains("input.copyTo"))
        assertTrue(worker.contains("input.copyTo"))
        assertTrue(worker.contains("UpdatePhase.INSTALLING"))
    }

    @Test
    fun `release transport follows only explicitly validated redirects`() {
        val source = File(
            "src/main/java/ai/zara/app/update/AndroidUpdateManager.kt"
        ).readText()

        assertTrue(source.contains("instanceFollowRedirects = false"))
        assertTrue(source.contains("UpdateSecurity.requireTrustedTransport(next)"))
        assertTrue(source.contains("MAX_REDIRECTS"))
    }

    @Test
    fun `install receiver reports terminal package installer status`() {
        val receiver = File(
            "src/main/java/ai/zara/app/update/UpdateInstallReceiver.kt"
        ).readText()

        assertTrue(receiver.contains("PackageInstaller.STATUS_PENDING_USER_ACTION"))
        assertTrue(receiver.contains("PackageInstaller.EXTRA_STATUS_MESSAGE"))
        assertTrue(receiver.contains("recordInstallStatus(status, message)"))
    }
}
