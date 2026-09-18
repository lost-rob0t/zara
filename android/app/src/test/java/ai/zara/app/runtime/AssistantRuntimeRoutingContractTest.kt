package ai.zara.app.runtime

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class AssistantRuntimeRoutingContractTest {
    @Test
    fun selectedPrologRlmOwnsNormalLocalReasoningBeforeLegacyModelFallback() {
        val source = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val submitLocal = source
            .substringAfter("private fun submitLocalText(")
            .substringBefore("private fun generatePrologRlmTurn(")

        val selection = submitLocal.indexOf(
            "assistantRuntimes.selectedRuntimeId() == PROLOG_RLM_RUNTIME_ID"
        )
        val legacyExpert = submitLocal.indexOf("LocalNaturalLanguageExpertRouter.query")
        val legacyModel = submitLocal.indexOf("recoverLocalNaturalLanguageTurn(")

        assertTrue(selection >= 0)
        assertTrue(legacyExpert > selection)
        assertTrue(legacyModel > selection)
        assertTrue(submitLocal.contains("if (!explicitSymbolic"))
        assertTrue(submitLocal.contains("return generatePrologRlmTurn(query, conversationId)"))
    }

    @Test
    fun androidRuntimeMenuIsDiscoveryDrivenNotStaticPrologRow() {
        val source = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val runtimeSettings = source
            .substringAfter("INSTALLED ASSISTANT RUNTIMES")
            .substringBefore("ROUTING POLICY")

        assertTrue(runtimeSettings.contains("installedAssistantRuntimes.forEach"))
        assertTrue(runtimeSettings.contains("runtime.displayName"))
        assertTrue(runtimeSettings.contains("runtime.runtimeVersion"))
        assertTrue(runtimeSettings.contains("runtime.profiles"))
    }
}
