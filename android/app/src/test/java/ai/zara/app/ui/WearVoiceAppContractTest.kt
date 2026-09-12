package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class WearVoiceAppContractTest {
    @Test
    fun dedicatedVoiceAppIsASeparateStandaloneWatchApplication() {
        val settings = File("../settings.gradle.kts").readText()
        assertTrue(settings.contains("include(\":wear-voice\")"))

        val manifest = File("../wear-voice/src/main/AndroidManifest.xml")
        assertTrue("dedicated Wear Voice manifest must exist", manifest.isFile)
        val source = manifest.readText()
        assertTrue(source.contains("android.hardware.type.watch"))
        assertTrue(source.contains("android.permission.RECORD_AUDIO"))
        assertTrue(source.contains("android.permission.INTERNET"))
        assertTrue(source.contains("com.google.android.wearable.standalone"))
        assertTrue(source.contains("ai.zara.action.WEAR_VOICE"))
        assertTrue(source.contains("android:exported=\"true\""))
    }

    @Test
    fun voiceShellReusesThemeAndDoesNotForkPhoneVoiceRuntime() {
        val sourceFile = File("../wear-voice/src/main/java/ai/zara/wear/voice/WearVoiceActivity.kt")
        assertTrue("Wear Voice activity must exist", sourceFile.isFile)
        val source = sourceFile.readText()
        assertTrue(source.contains("import ai.zara.ui.theme.ZaraTheme"))
        assertTrue(source.contains("import ai.zara.ui.theme.themeTokens"))
        assertFalse(source.contains("Color(0x"))

        val wearVoiceRoot = File("../wear-voice/src/main/java")
        if (wearVoiceRoot.exists()) {
            val forbiddenNames = setOf(
                "AndroidAppSession.kt",
                "AndroidPcmRecorder.kt",
                "AndroidPcmOutput.kt",
                "ZaraTextClientActor.kt",
                "AndroidEnrollmentRepository.kt",
            )
            val copied = wearVoiceRoot.walkTopDown()
                .filter { it.isFile && it.name in forbiddenNames }
                .toList()
            assertTrue("Wear Voice must consume shared runtime instead of copying phone classes: $copied", copied.isEmpty())
        }
    }
}
