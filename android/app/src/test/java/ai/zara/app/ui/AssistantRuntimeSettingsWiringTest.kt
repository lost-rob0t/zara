package ai.zara.app.ui

import java.io.File
import org.junit.Test

class AssistantRuntimeSettingsWiringTest {
    private fun runtimeSettings(): String =
        File("src/main/java/ai/zara/app/ui/ZaraApp.kt")
            .readText()
            .substringAfter("AppRoute.Runtime -> {")
            .substringBefore("AppRoute.Permissions -> {")

    private fun runtimeRows(): String = runtimeSettings()
        .substringAfter("installedAssistantRuntimes.forEach { runtime ->")
        .substringBefore("TextButton(")

    @Test
    fun runtimeRowsAreRenderedFromDiscoveredDescriptorsOnly() {
        val rows = runtimeRows()

        check(rows.contains("runtime.displayName"))
        check(rows.contains("append(runtime.id)"))
        check(rows.contains("append(runtime.runtimeVersion)"))
        check(rows.contains("append(runtime.health)"))
        check(rows.contains("runtime.profiles.isNotEmpty()"))
        check(rows.contains("runtime.profiles.joinToString"))
        check(!rows.contains("PROLOG_RLM_RUNTIME_ID"))
        check(!rows.contains("EMBEDDED_LOCAL_RUNTIME_ID"))
        check(!rows.contains("Prolog-RLM"))
        check(!rows.contains("Embedded Local"))
    }

    @Test
    fun selectionUsesStableRuntimeIdAndCurrentSelectability() {
        val rows = runtimeRows()

        check(rows.contains("selectedRuntime = runtime.id == selectedAssistantRuntimeId"))
        check(rows.contains("enabled = runtime.selectable"))
        check(rows.contains("onSelectAssistantRuntime(runtime.id)"))
        check(!rows.contains("enabled = selectedRuntime"))
        check(!rows.contains("onSelectAssistantRuntime(\""))
    }

    @Test
    fun runtimeMetadataAndProfilesAreNeverManufacturedByTheUi() {
        val rows = runtimeRows()

        check(rows.contains("append(runtime.runtimeVersion)"))
        check(rows.contains("append(runtime.health)"))
        check(rows.contains("runtime.profiles.joinToString"))
        check(!rows.contains("append(\"ready\")"))
        check(!rows.contains("append(\"agentprolog\")"))
        check(!rows.contains("append(\"prolog-rlm\")"))
    }

    @Test
    fun routingPolicyRemainsSeparateFromRuntimeIdentity() {
        val runtime = runtimeSettings()
        val rows = runtimeRows()
        val installedIndex = runtime.indexOf("INSTALLED ASSISTANT RUNTIMES")
        val routingIndex = runtime.indexOf("ROUTING POLICY")

        check(installedIndex >= 0)
        check(routingIndex > installedIndex)
        check(runtime.contains("RuntimeMode.entries.forEach { mode ->"))
        check(runtime.contains("onSelectRuntimeMode(mode)"))
        check(!rows.contains("RuntimeMode.entries"))
        check(!rows.contains("onSelectRuntimeMode"))
    }

    @Test
    fun refreshActsOnDiscoveryInsteadOfRoutingPolicy() {
        val runtime = runtimeSettings()
        val refreshIndex = runtime.indexOf("onClick = onRefreshAssistantRuntimes")
        val routingIndex = runtime.indexOf("ROUTING POLICY")

        check(refreshIndex >= 0)
        check(routingIndex > refreshIndex)
        check(runtime.contains("Text(\"Refresh installed runtimes\")"))
    }

    @Test
    fun canonicalAndroidEvidenceCapturesRuntimeSettings() {
        val acceptance = File("../integration/device_acceptance.py").readText()

        check(acceptance.contains("\"Runtime\","))
        check(acceptance.contains("device.capture(f\"settings-{tab.lower()}\")"))
    }
}
