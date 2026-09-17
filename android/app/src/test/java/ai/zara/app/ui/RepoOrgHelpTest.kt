package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class RepoOrgHelpTest {
    @Test
    fun preferredCanonicalRepoSourcesComeFirst() {
        val paths = selectRepoOrgHelpPaths(
            """
            wiki/agent-mode.org
            docs/README.org
            README.org
            wiki/android.org
            docs/server.org
            """.trimIndent(),
        )

        assertEquals(
            listOf(
                "README.org",
                "docs/README.org",
                "wiki/android.org",
                "wiki/agent-mode.org",
                "docs/server.org",
            ),
            paths,
        )
    }

    @Test
    fun androidBuildPackagesRepoOrgSourcesInsteadOfDuplicatedHelpCopy() {
        val build = File("build.gradle.kts").readText()
        val panel = File("src/main/java/ai/zara/app/ui/RepoOrgHelp.kt").readText()

        assertTrue(build.contains("GenerateRepoOrgHelpAssets"))
        assertTrue(build.contains("README.org"))
        assertTrue(build.contains("docs"))
        assertTrue(build.contains("wiki"))
        assertTrue(panel.contains("assets.open(\"help/index.txt\")"))
        assertTrue(panel.contains("OrgTextRenderer.renderSource"))
    }
}
