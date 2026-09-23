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
}
