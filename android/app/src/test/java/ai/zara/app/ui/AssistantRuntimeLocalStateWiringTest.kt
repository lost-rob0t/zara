package ai.zara.app.ui

import java.io.File
import org.junit.Test

class AssistantRuntimeLocalStateWiringTest {
    private val activitySource: String
        get() = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

    @Test
    fun initialRuntimeUiFailsClosedBeforeCanonicalLocalAiObservation() {
        val initialization = activitySource
            .substringAfter("val desiredAssistantRuntimeId = assistantRuntimeStore.load()")
            .substringBefore("val embeddingPreferenceStore")

        check(
            initialization.contains(
                "failClosedEmbeddedLocalRuntime(appSession.installedAssistantRuntimes())",
            ),
        )
        check(initialization.contains("selectedAssistantRuntimeId by mutableStateOf(\"\")"))
        check(!initialization.contains("mutableStateOf(appSession.selectedAssistantRuntimeId())"))
    }

    @Test
    fun refreshJoinsRuntimeDiscoveryWithCanonicalLocalAiState() {
        val refresh = activitySource
            .substringAfter("fun refreshAssistantRuntimes(")
            .substringBefore("refreshAssistantRuntimes(restorePersistedSelection = true)")

        check(refresh.contains("appSession.discoverAssistantRuntimes()"))
        check(refresh.contains("thenCombine(appSession.localAiState())"))
        check(refresh.contains("projectEmbeddedLocalRuntime(runtimes, localAiState)"))
    }

    @Test
    fun failedLocalAiObservationCannotRestoreSyntheticReadyRow() {
        val refresh = activitySource
            .substringAfter("fun refreshAssistantRuntimes(")
            .substringBefore("refreshAssistantRuntimes(restorePersistedSelection = true)")

        check(refresh.contains("if (error != null)"))
        check(refresh.contains("failClosedEmbeddedLocalRuntime("))
        check(refresh.contains("it.id == candidate && it.selectable"))
        check(refresh.contains("}.orEmpty()"))
    }
}
