package ai.zara.app.ui

import java.io.File
import org.junit.Test

class AssistantRuntimeSettingsWiringTest {
    private fun runtimeSettings(): String =
        File("src/main/java/ai/zara/app/ui/ZaraApp.kt")
            .readText()
            .substringAfter("AppRoute.Runtime -> {")
            .substringBefore("AppRoute.Permissions -> {")

    @Test
    fun runtimeRowsAreRenderedFromDiscoveredDescriptorsOnly() {
        val runtime = runtimeSettings()
        val rows = runtime
            .substringAfter("installedAssistantRuntimes.forEach { runtime ->")
            .substringBefore("TextButton(")

        check(rows.contains("runtime.displayName"))
        check(rows.contains("append(runtime.id)"))
        check(rows.contains("append(runtime.runtimeVersion)"))
        check(rows.contains("append(runtime.health)"))
        check(rows.contains("runtime.profiles.isNotEmpty()"))
        check(rows.contains("runtime.profiles.joinToString"))
        check(!rows.contains("PROLOG_RLM_RUNTIME_ID"))
        check(!rows.contains("Prolog-RLM"))
    }

    @Test
    fun selectionUsesStableRuntimeIdAndCurrentSelectability() {
        val runtime = runtimeSettings()
        val rows = runtime
            .substringAfter("installedAssistantRuntimes.forEach { runtime ->")
            .substringBefore("TextButton(")

        check(rows.contains("selectedRuntime = runtime.id == selectedAssistantRuntimeId"))
        check(rows.contains("enabled = runtime.selectable"))
        check(rows.contains("onSelectAssistantRuntime(runtime.id)"))
    }

    @Test
    fun routingPolicyRemainsSeparateFromRuntimeIdentity() {
        val runtime = runtimeSettings()
        val installedIndex = runtime.indexOf("INSTALLED ASSISTANT RUNTIMES")
        val routingIndex = runtime.indexOf("ROUTING POLICY")

        check(installedIndex >= 0)
        check(routingIndex > installedIndex)
        check(runtime.contains("RuntimeMode.entries.forEach { mode ->"))
        check(runtime.contains("onSelectRuntimeMode(mode)"))
    }

    @Test
    fun canonicalAndroidEvidenceCapturesRuntimeSettings() {
        val acceptance = File("../integration/device_acceptance.py").readText()

        check(acceptance.contains("\"Runtime\","))
        check(acceptance.contains("device.capture(f\"settings-{tab.lower()}\")"))
    }
}
