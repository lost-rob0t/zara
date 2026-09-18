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
        assertFalse(
            "focused Wear Voice shell must not have a direct network fallback surface",
            source.contains("android.permission.INTERNET"),
        )
        assertTrue(source.contains("com.google.android.wearable.standalone"))
        assertTrue(source.contains("ai.zara.action.WEAR_VOICE"))
        assertTrue(source.contains("android:exported=\"true\""))
    }

    @Test
    fun voiceAppUsesCanonicalVersionContext() {
        val source = File("../wear-voice/build.gradle.kts").readText()
        assertTrue(source.contains("loadZaraVersionProperties"))
        assertTrue(source.contains("versionCode = zaraAndroidVersionCode"))
        assertTrue(source.contains("versionName = zaraVersionName"))
        assertFalse(source.contains("versionCode = 1"))
        assertFalse(source.contains("versionName = \"0.1.0-alpha\""))
    }

    @Test
    fun packagedVoiceApkIsCheckedForMergedInternetPermission() {
        val gate = File("../../scripts/test-android.sh")
        assertTrue("Android/Wear gate must exist", gate.isFile)
        val source = gate.readText()
        assertTrue(source.contains("voice_apk=\"wear-voice/build/outputs/apk/debug/wear-voice-debug.apk\""))
        assertTrue(source.contains("aapt2 dump permissions \"\$voice_apk\""))
        assertTrue(source.contains("android.permission.INTERNET"))
        assertTrue(source.contains("focused strict-local APK requests INTERNET"))
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
