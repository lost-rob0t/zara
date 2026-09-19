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
        check(rows.contains("append(runtime.locality)"))
        check(rows.contains("runtime.supportsStreaming"))
        check(rows.contains("runtime.supportsCancel"))
        check(rows.contains("runtime.profiles.isNotEmpty()"))
        check(rows.contains("runtime.profiles.joinToString"))
        check(!rows.contains("PROLOG_RLM_RUNTIME_ID"))
        check(!rows.contains("EMBEDDED_LOCAL_RUNTIME_ID"))
        check(!rows.contains("Prolog-RLM"))
        check(!rows.contains("Embedded Local"))
    }

    @Test
    fun runtimeRowsNeverSynthesizeFallbackRuntimeEntries() {
        val installed = runtimeSettings()
            .substringAfter("INSTALLED ASSISTANT RUNTIMES")
            .substringBefore("ROUTING POLICY")

        check(installed.contains("installedAssistantRuntimes.forEach { runtime ->"))
        check(!installed.contains("installedAssistantRuntimes.ifEmpty"))
        check(!installed.contains("AssistantRuntimeDescriptor("))
        check(!installed.contains("embeddedLocalRuntimeDescriptor("))
        check(!installed.contains("PROLOG_RLM_RUNTIME_ID"))
        check(!installed.contains("EMBEDDED_LOCAL_RUNTIME_ID"))
    }

    @Test
    fun discoveredButUnselectableRuntimesRemainVisibleForDiagnostics() {
        val runtime = runtimeSettings()
        val installed = runtime
            .substringAfter("INSTALLED ASSISTANT RUNTIMES")
            .substringBefore("ROUTING POLICY")
        val rows = runtimeRows()

        check(installed.contains("installedAssistantRuntimes.forEach { runtime ->"))
        check(!installed.contains("installedAssistantRuntimes.filter"))
        check(!installed.contains("filter { runtime -> runtime.selectable"))
        check(rows.contains("enabled = runtime.selectable"))
    }

    @Test
    fun runtimeRowsPreserveCanonicalDiscoveryOrder() {
        val installed = runtimeSettings()
            .substringAfter("INSTALLED ASSISTANT RUNTIMES")
            .substringBefore("ROUTING POLICY")

        check(installed.contains("installedAssistantRuntimes.forEach { runtime ->"))
        check(!installed.contains("installedAssistantRuntimes.sorted"))
        check(!installed.contains("installedAssistantRuntimes.reversed"))
        check(!installed.contains("installedAssistantRuntimes.distinct"))
        check(!installed.contains("installedAssistantRuntimes.groupBy"))
    }

    @Test
    fun selectionUsesStableRuntimeIdAndCurrentSelectability() {
        val rows = runtimeRows()

        check(rows.contains("selectedRuntime = runtime.id == selectedAssistantRuntimeId"))
        check(rows.contains("enabled = runtime.selectable"))
        check(rows.contains("onSelectAssistantRuntime(runtime.id)"))
        check(!rows.contains("selectedRuntime = runtime.displayName"))
        check(!rows.contains("onSelectAssistantRuntime(runtime.displayName)"))
        check(!rows.contains("enabled = selectedRuntime"))
        check(!rows.contains("onSelectAssistantRuntime(\""))
    }

    @Test
    fun runtimeHealthDrivesStatusAccentInsteadOfSelection() {
        val rows = runtimeRows()

        check(rows.contains("when (runtime.health)"))
        check(rows.contains("\"ready\" -> tokens.success"))
        check(rows.contains("\"starting\", \"busy\", \"degraded\" -> tokens.warning"))
        check(rows.contains("\"failed\" -> tokens.error"))
        check(rows.contains("\"stopped\" -> tokens.textMuted"))
        check(rows.contains("else -> tokens.warning"))
        check(!rows.contains("selectedRuntime -> tokens.success"))
        check(!rows.contains("runtime.selectable -> tokens.success"))
        check(!rows.contains("else -> tokens.success"))
    }

    @Test
    fun runtimeMetadataAndProfilesAreNeverManufacturedByTheUi() {
        val rows = runtimeRows()

        check(rows.contains("append(runtime.runtimeVersion)"))
        check(rows.contains("append(runtime.health)"))
        check(rows.contains("append(runtime.locality)"))
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
    fun routingPolicyCannotSelectOrSynthesizeConcreteRuntimes() {
        val routing = runtimeSettings()
            .substringAfter("ROUTING POLICY")
            .substringBefore("LOCAL EMBEDDINGS")

        check(routing.contains("RuntimeMode.entries.forEach { mode ->"))
        check(routing.contains("onSelectRuntimeMode(mode)"))
        check(!routing.contains("onSelectAssistantRuntime"))
        check(!routing.contains("AssistantRuntimeDescriptor("))
        check(!routing.contains("PROLOG_RLM_RUNTIME_ID"))
        check(!routing.contains("EMBEDDED_LOCAL_RUNTIME_ID"))
        check(!routing.contains("prolog-rlm"))
        check(!routing.contains("embedded-local"))
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

    @Test
    fun runtimeScreenshotEvidenceIsExactHeadAndFailClosed() {
        val acceptance = File("../integration/device_acceptance.py").readText()
        val exerciseIndex = acceptance.indexOf("exercise_three_menu_ui(device)")
        val passIndex = acceptance.indexOf("result[\"passed\"] = True")

        check(acceptance.contains("source_sha = verified_source_sha(args.source_sha)"))
        check(acceptance.contains("\"source_sha\": source_sha"))
        check(acceptance.contains("\"passed\": False"))
        check(exerciseIndex >= 0)
        check(passIndex > exerciseIndex)
        check(acceptance.contains("(args.output / \"manifest.json\").write_text"))
    }

    @Test
    fun runtimeScreenshotFlowNeverAssumesAConcreteInstalledRuntime() {
        val acceptance = File("../integration/device_acceptance.py").readText()
        val settingsFlow = acceptance
            .substringAfter("open_menu(device, \"Settings\")")
            .substringBefore("device.tap_tab(\"Appearance\")")

        check(settingsFlow.contains("\"Runtime\","))
        check(settingsFlow.contains("device.capture(f\"settings-{tab.lower()}\")"))
        check(!settingsFlow.contains("Prolog-RLM"))
        check(!settingsFlow.contains("Embedded Local"))
        check(!settingsFlow.contains("prolog-rlm"))
        check(!settingsFlow.contains("embedded-local"))
    }
}
