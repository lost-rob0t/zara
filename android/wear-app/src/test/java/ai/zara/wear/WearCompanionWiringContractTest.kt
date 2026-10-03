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

    @Test
    fun wearHealthUsesOptionalVendorSdkCapabilityDiscoveryAndNoPhoneProvisionPayload() {
        val build = File("build.gradle.kts").readText()
        val activity = File("src/main/java/ai/zara/wear/WearMainActivity.kt").readText()
        val health = File("src/main/java/ai/zara/wear/health/WearHealth.kt").readText()
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(build.contains("samsung-health-sensor-api-*.aar"))
        assertTrue(build.contains("HAS_SAMSUNG_HEALTH_SENSOR_SDK"))
        assertTrue(activity.contains("ZaraWearHealthSurface"))
        assertTrue(health.contains("supportedTrackers"))
        assertTrue(manifest.contains("android.permission.health.READ_HEART_RATE"))
        assertTrue(manifest.contains("READ_ADDITIONAL_HEALTH_DATA"))
        assertTrue(!manifest.contains("FOREGROUND_SERVICE_HEALTH"))
        assertTrue(!health.contains("WearCompanionContract.PATH_PHONE_PROVISION"))
        assertTrue(!health.contains("sendMessage("))
    }
}
