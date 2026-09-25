package ai.zara.app.automation

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import java.io.File

class AndroidAdbVisionProductionWiringContractTest {
    @Test
    fun `automation surface uses canonical vision conversation control with explicit approval`() {
        val activity = File("src/main/java/ai/zara/app/automation/AutomationActivity.kt").readText()

        assertTrue(activity.contains("AndroidAdbVisionConversationControl("))
        assertTrue(activity.contains("requestApproval = ::requestVisionApproval"))
        assertTrue(activity.contains("visionControl.run(goal)"))
        assertTrue(activity.contains("pendingVisionApproval"))
        assertTrue(activity.contains("AlertDialog("))
        assertTrue(activity.contains("visionControl.cancel()"))
        assertTrue(activity.contains("visionControl.close()"))

        assertFalse(activity.contains("LocalAiServiceClient"))
        assertFalse(activity.contains("AndroidCanonicalMultimodalVisionPort("))
    }

    @Test
    fun `blocking screenshot and admitted ADB effects never resume on the Compose UI thread`() {
        val activity = File("src/main/java/ai/zara/app/automation/AutomationActivity.kt").readText()
        val runVision = activity.substringAfter("private fun runVision(goal: String)")
            .substringBefore("private fun cancelVision()")
        val resolveApproval = activity.substringAfter("private fun resolveVisionApproval(approved: Boolean)")
            .substringBefore("private fun requestAccess(")

        // screenshotPng() is synchronous at loop entry, so production starts the loop on the
        // existing AutomationActivity I/O executor rather than blocking Compose's main thread.
        assertTrue(runVision.contains("io.execute {"))
        assertTrue(runVision.contains("visionControl.run(goal)"))

        // CompletableFuture continuations execute on the completing thread. Approval therefore
        // completes on the same I/O executor so an admitted tap/swipe/text/key cannot run on UI.
        assertTrue(resolveApproval.contains("io.execute {"))
        assertTrue(resolveApproval.contains("pending.future.complete(approved)"))
    }
}
