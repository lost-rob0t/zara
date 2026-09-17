package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Test

class ProjectIdentityTest {
    @Test
    fun projectNameRenamesDefaultWakeWords() {
        val identity = PrologProjectIdentityResolver.resolve(
            listOf(PrologSource("identity.pl", "project_name(\"Mara\").\n"))
        )

        assertEquals("Mara", identity.effectiveProjectName())
        assertEquals("Mara", identity.effectiveLlmAppName())
        assertEquals(listOf("hey mara", "mara"), identity.defaultWakeWords())
    }

    @Test
    fun llmAppNameCanDifferFromProjectName() {
        val identity = PrologProjectIdentityResolver.resolve(
            listOf(
                PrologSource(
                    "identity.pl",
                    "project_name(\"Mara\").\nllm_app_name(\"Mara Android Coding\").\n",
                )
            )
        )

        assertEquals("Mara", identity.effectiveProjectName())
        assertEquals("Mara Android Coding", identity.effectiveLlmAppName("Provider Default"))
    }

    @Test
    fun lastPrologFactWinsAcrossWorkspaceSources() {
        val identity = PrologProjectIdentityResolver.resolve(
            listOf(
                PrologSource("a.pl", "project_name(\"Zara\").\n"),
                PrologSource("z-local.pl", "project_name(\"Nova\").\n"),
            )
        )

        assertEquals("Nova", identity.effectiveProjectName())
        assertEquals("Nova", identity.effectiveLlmAppName("Configured Name"))
    }

    @Test
    fun zaraIdentityKeepsLegacyWakeAliases() {
        val identity = PrologProjectIdentityResolver.resolve(emptyList())

        assertEquals(PrologProjectIdentity.LEGACY_ZARA_WAKE_WORDS, identity.defaultWakeWords())
    }
}
