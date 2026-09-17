package ai.zara.app.org

import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class LiterateOrgConfigTest {
    @Test
    fun tanglesPythonAndPrologIntoRuntimeDirectory() {
        val root = Files.createTempDirectory("zara-org-config").toFile()
        val source = """
            #+title: Zara config
            #+property: header-args:python :tangle hooks/config.py
            #+property: header-args:prolog :tangle logic/config.pl

            #+begin_src python
            def configure(zara):
                zara.project = "StarIntel"
            #+end_src

            #+begin_src prolog
            zara_project(starintel).
            #+end_src
        """.trimIndent()

        val outputs = LiterateOrgConfig(root).tangle(source, "zara.org")

        assertEquals(listOf("config.py", "config.pl"), outputs.map { it.name })
        assertTrue(root.resolve("hooks/config.py").readText().contains("def configure"))
        assertTrue(root.resolve("logic/config.pl").readText().contains("zara_project(starintel)."))
    }
}
