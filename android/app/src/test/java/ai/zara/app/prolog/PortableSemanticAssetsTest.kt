package ai.zara.app.prolog

import java.nio.file.Files
import java.nio.file.Path
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PortableSemanticAssetsTest {

    private fun sourcePath(resource: String): Path = when (resource) {
        PortableSemanticCore.coreAssetPath,
        PortableSemanticCore.fixtureAssetPath -> Path.of("src", "main", "assets", resource)
        "prolog/shared/modules/intent_frames.pl" -> Path.of("..", "..", "modules", "intent_frames.pl")
        "prolog/shared/modules/normalizer.pl" -> Path.of("..", "..", "modules", "normalizer.pl")
        "prolog/shared/kb/intents.pl" -> Path.of("..", "..", "kb", "intents.pl")
        else -> throw AssertionError("unmapped portable semantic resource: $resource")
    }

    @Test fun `declared portable semantic sources are present and nonempty`() {
        PortableSemanticCore.resources.forEach { resource ->
            val path = sourcePath(resource)
            assertTrue("missing portable source: $resource", Files.isRegularFile(path))
            assertTrue("empty portable source: $resource", Files.size(path) > 0L)
        }
    }

    @Test fun `generated Android assets come from canonical repository resolver sources`() {
        val build = Files.readString(Path.of("build.gradle.kts"))

        assertTrue(build.contains("GeneratePortableSemanticAssets"))
        assertTrue(build.contains("modules/intent_frames.pl"))
        assertTrue(build.contains("modules/normalizer.pl"))
        assertTrue(build.contains("kb/intents.pl"))
        assertTrue(build.contains("addGeneratedSourceDirectory"))
    }

    @Test fun `generated asset task fingerprints only canonical resolver inputs`() {
        val build = Files.readString(Path.of("build.gradle.kts"))

        assertTrue(build.contains("@get:InputFiles"))
        assertTrue(build.contains("PathSensitivity.RELATIVE"))
        assertTrue(build.contains("sourceFiles.from("))
        assertFalse(build.contains("@get:InputDirectory"))
        assertFalse(build.contains("repositoryRoot.set("))
    }

    @Test fun `packaged assets identify the frozen semantic contract and corpus`() {
        val core = Files.readString(sourcePath(PortableSemanticCore.coreAssetPath))
        val fixtures = Files.readString(sourcePath(PortableSemanticCore.fixtureAssetPath))

        assertTrue(core.contains("semantic_contract_version('ZARA-SEMANTIC/1')"))
        assertTrue(fixtures.contains("\"contract\": \"ZARA-SEMANTIC/1\""))
        assertTrue(fixtures.contains("\"source\": \"kb/semantic_corpus.pl#156\""))
        assertTrue(fixtures.contains("\"id\": \"timer_complete\""))
        assertTrue(fixtures.contains("\"id\": \"empty_input\""))
    }
}
