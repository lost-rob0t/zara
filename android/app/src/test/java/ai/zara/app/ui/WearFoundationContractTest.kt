package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class WearFoundationContractTest {
    @Test
    fun sharedThemeSourceIsCanonicalAcrossPhoneAndWear() {
        val settings = File("../settings.gradle.kts").readText()
        assertTrue(settings.contains("include(\":shared-ui\")"))
        assertTrue(settings.contains("include(\":wear-app\")"))

        val sharedTheme = File(
            "../shared-ui/src/main/java/ai/zara/ui/theme/ZaraTheme.kt"
        )
        assertTrue("shared semantic theme source must exist", sharedTheme.isFile)
        val sharedSource = sharedTheme.readText()
        listOf(
            "background", "surface", "surfaceElevated", "surfaceInput", "border",
            "borderActive", "primary", "secondary", "accentMagenta", "accentCyan",
            "text", "textMuted", "success", "warning", "error", "focus", "ambientGlow",
        ).forEach { role ->
            assertTrue("missing semantic role $role", sharedSource.contains("val $role:"))
        }
        listOf("Outrun", "StarIntel", "Midnight", "Terminal", "Light", "System").forEach { theme ->
            assertTrue("missing built-in theme $theme", sharedSource.contains(theme))
        }
        assertTrue(sharedSource.contains("0xFF02040B"))
        assertTrue(sharedSource.contains("0xFFE21CF2"))
        assertTrue(sharedSource.contains("0xFF16D9FF"))

        val phoneShell = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        assertTrue(phoneShell.contains("import ai.zara.ui.theme.ZaraSemanticTokens"))
        assertTrue(phoneShell.contains("import ai.zara.ui.theme.ZaraTheme"))
        assertTrue(phoneShell.contains("import ai.zara.ui.theme.themeTokens"))
        assertFalse(phoneShell.contains("data class ZaraSemanticTokens"))
        assertFalse(phoneShell.contains("private val OutrunTokens"))

        val wearShell = File("../wear-app/src/main/java/ai/zara/wear/WearMainActivity.kt")
        assertTrue("Wear shell must exist", wearShell.isFile)
        val wearSource = wearShell.readText()
        assertTrue(wearSource.contains("import ai.zara.ui.theme.ZaraTheme"))
        assertTrue(wearSource.contains("import ai.zara.ui.theme.themeTokens"))
        assertTrue(wearSource.contains("themeTokens(ZaraTheme.Outrun"))
        assertFalse("Wear shell must not duplicate theme colors", wearSource.contains("Color(0x"))
    }

    @Test
    fun wearApplicationsKeepApi30CompatibilityFloor() {
        val wearAppGradle = File("../wear-app/build.gradle.kts").readText()
        val wearVoiceGradle = File("../wear-voice/build.gradle.kts").readText()
        val sharedUiGradle = File("../shared-ui/build.gradle.kts").readText()

        assertTrue(
            "Zara Wear must remain installable on the Galaxy Watch5 Pro / API 30 baseline",
            wearAppGradle.contains("minSdk = 30"),
        )
        assertTrue(
            "Zara Wear Voice must remain installable on the same API 30 watch baseline",
            wearVoiceGradle.contains("minSdk = 30"),
        )
        assertTrue(
            "Shared UI must not raise the effective Wear minimum above API 30",
            Regex("""minSdk\\s*=\\s*(2[0-9]|30)""").containsMatchIn(sharedUiGradle),
        )
    }

    @Test
    fun wearClientDeclaresStandaloneWatchContract() {
        val manifest = File("../wear-app/src/main/AndroidManifest.xml")
        assertTrue("Wear manifest must exist", manifest.isFile)
        val source = manifest.readText()

        assertTrue(source.contains("android.hardware.type.watch"))
        assertTrue(source.contains("android.permission.INTERNET"))
        assertTrue(source.contains("com.google.android.wearable.standalone"))
        assertTrue(source.contains("android:value=\"true\""))
        assertTrue(source.contains("ai.zara.wear.WearMainActivity"))
    }
}
