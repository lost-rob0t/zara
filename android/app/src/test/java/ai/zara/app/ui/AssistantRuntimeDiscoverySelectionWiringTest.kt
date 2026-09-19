package ai.zara.app.ui

import java.io.File
import org.junit.Test

class AssistantRuntimeDiscoverySelectionWiringTest {
    private fun discoverySelection(): String =
        File("src/main/java/ai/zara/app/MainActivity.kt")
            .readText()
            .substringAfter("fun applyAssistantRuntimeDiscovery(")
            .substringBefore("fun refreshAssistantRuntimes(")

    @Test
    fun persistedSelectionIsRestoredOnlyWhenRediscoveredAndSelectable() {
        val wiring = discoverySelection()

        check(wiring.contains("runtimes.any { it.id == candidate && it.selectable }"))
        check(wiring.contains("runtimes.firstOrNull { it.selectable }?.id"))
        check(wiring.contains("appSession.selectAssistantRuntime(target)"))
        check(!wiring.contains("EMBEDDED_LOCAL_RUNTIME_ID"))
        check(!wiring.contains("PROLOG_RLM_RUNTIME_ID"))
        check(!wiring.contains("embedded-local"))
        check(!wiring.contains("prolog-rlm"))
    }

    @Test
    fun noSelectableRuntimeFailsClosedInsteadOfInventingAConcreteRuntime() {
        val wiring = discoverySelection()
        val noTarget = wiring.substringAfter("if (target == null) {")
            .substringBefore("appSession.selectAssistantRuntime(target)")

        check(noTarget.contains("selectedAssistantRuntimeId = \"\""))
        check(noTarget.contains("No discovered assistant runtime is currently selectable"))
        check(noTarget.contains("return"))
    }
}
