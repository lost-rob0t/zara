package ai.zara.app.prolog

import java.nio.file.Files
import java.nio.file.Path
import org.junit.Assert.assertTrue
import org.junit.Test

class PortableSemanticAssetsTest {

    private fun assetPath(resource: String): Path =
        Path.of("src", "main", "assets", resource)

    @Test fun `declared portable semantic assets are packaged and nonempty`() {
        PortableSemanticCore.resources.forEach { resource ->
            val path = assetPath(resource)
            assertTrue("missing asset: $resource", Files.isRegularFile(path))
            assertTrue("empty asset: $resource", Files.size(path) > 0L)
        }
    }

    @Test fun `packaged assets identify the frozen semantic contract and corpus`() {
        val core = Files.readString(assetPath("prolog/portable/semantic_core.pl"))
        val fixtures = Files.readString(assetPath("prolog/portable/semantic_fixtures.json"))

        assertTrue(core.contains("semantic_contract_version('ZARA-SEMANTIC/1')"))
        assertTrue(fixtures.contains("\"contract\": \"ZARA-SEMANTIC/1\""))
        assertTrue(fixtures.contains("\"source\": \"kb/semantic_corpus.pl#156\""))
        assertTrue(fixtures.contains("\"id\": \"timer_complete\""))
        assertTrue(fixtures.contains("\"id\": \"empty_input\""))
    }
}
