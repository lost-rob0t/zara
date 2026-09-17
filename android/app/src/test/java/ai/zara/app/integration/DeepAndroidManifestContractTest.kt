package ai.zara.app.integration

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class DeepAndroidManifestContractTest {
    @Test
    fun `deep integration services share the voice runtime`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        listOf(
            ".integration.accessibility.ZaraAccessibilityService",
            ".integration.notification.ZaraNotificationListenerService",
            ".integration.ime.ZaraInputMethodService",
            ".integration.appfunctions.ZaraAppFunctionService",
            ".integration.surface.ZaraShareActivity",
        ).forEach { component ->
            val start = manifest.indexOf("android:name=\"$component\"")
            assertTrue("missing component $component", start >= 0)
            val end = manifest.indexOf('>', start)
            val declaration = manifest.substring(start, end)
            assertTrue("$component must run in :voice", declaration.contains("android:process=\":voice\""))
        }
    }

    @Test
    fun `system integration surfaces use platform binding permissions`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(manifest.contains("android.permission.BIND_ACCESSIBILITY_SERVICE"))
        assertTrue(manifest.contains("android.permission.BIND_NOTIFICATION_LISTENER_SERVICE"))
        assertTrue(manifest.contains("android.permission.BIND_INPUT_METHOD"))
        assertTrue(manifest.contains("android.permission.BIND_QUICK_SETTINGS_TILE"))
        assertTrue(manifest.contains("android.permission.BIND_DEVICE_ADMIN"))
        assertTrue(manifest.contains("android.permission.BIND_APP_FUNCTION_SERVICE"))
    }

    @Test
    fun `appfunctions expose Zara Prolog and policy gated Android execution`() {
        val source = File(
            "src/main/java/ai/zara/app/integration/appfunctions/BaseZaraAppFunctionService.kt",
        ).readText()
        val build = File("build.gradle.kts").readText()

        assertTrue(source.contains("@AppFunctionServiceEntryPoint"))
        assertTrue(source.contains("suspend fun askZara"))
        assertTrue(source.contains("suspend fun queryLocalProlog"))
        assertTrue(source.contains("suspend fun executeAndroid"))
        assertTrue(source.contains("zara.androidIntegration.execute"))
        assertTrue(build.contains("ksp(libs.appfunctions.compiler)"))
    }

    @Test
    fun `owner authority seed is explicitly unrestricted and live editable`() {
        val runtime = File(
            "src/main/java/ai/zara/app/integration/AndroidIntegrationRuntime.kt",
        ).readText()
        val policy = File(
            "src/main/java/ai/zara/app/integration/AndroidAuthorityPolicy.kt",
        ).readText()

        assertTrue(runtime.contains("android_authority(unrestricted)."))
        assertTrue(runtime.contains("android_confirmation(unrestricted, none)."))
        assertTrue(policy.contains("AndroidAuthorityPolicy.fromWorkspace"))
        assertTrue(policy.contains("fun snapshot(): AndroidAuthoritySnapshot = AndroidAuthorityParser.parse(sources())"))
    }
}
