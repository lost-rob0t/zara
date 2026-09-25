package ai.zara.wear

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class WearCompanionWiringContractTest {
    @Test
    fun wearAppRegistersThePhoneCompanionListenerAndCapability() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        val capabilities = File("src/main/res/values/wear.xml").readText()

        assertTrue(manifest.contains("ai.zara.wear.WearCompanionLinkService"))
        assertTrue(manifest.contains("/zara/phone/provision"))
        assertTrue(capabilities.contains("zara_watch"))
    }

    @Test
    fun wearClientConsumesTheSharedCompanionContractNotADuplicateProtocol() {
        val client = File("src/main/java/ai/zara/wear/WearCompanionRuntime.kt").readText()
        val service = File("src/main/java/ai/zara/wear/WearCompanionLinkService.kt").readText()
        val gate = File("src/main/java/ai/zara/wear/WearCompanionClient.kt").readText()

        assertTrue(client.contains("WearCompanionContract.CAPABILITY_PHONE"))
        assertTrue(client.contains("WearCompanionContract.PATH_WATCH_HELLO"))
        assertTrue(service.contains("WearCompanionContract.PATH_PHONE_PROVISION"))
        assertTrue(gate.contains("SymbolicConversationContinuityGate.accepts"))
    }

    @Test
    fun wearUiLaunchesTheDedicatedVoiceAppThroughItsCanonicalAction() {
        val activity = File("src/main/java/ai/zara/wear/WearMainActivity.kt").readText()
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(activity.contains("ai.zara.action.WEAR_VOICE"))
        assertTrue(manifest.contains("ai.zara.action.WEAR_VOICE"))
    }

    @Test
    fun watchClientNeverGainsCredentialOrSessionAuthority() {
        val wearSources = File("src/main/java/ai/zara/wear").listFiles()
            ?.filter { it.extension == "kt" }
            ?.map { it.readText() }
            .orEmpty()

        assertTrue(wearSources.isNotEmpty())
        wearSources.forEach { source ->
            assertTrue(
                "wear sources must not import phone enrollment material",
                !source.contains("EnrollmentRepository") && !source.contains("AndroidCredentialCipher"),
            )
        }
    }
}
