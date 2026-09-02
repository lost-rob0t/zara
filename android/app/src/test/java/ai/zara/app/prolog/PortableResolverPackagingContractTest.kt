package ai.zara.app.prolog

import java.io.ByteArrayInputStream
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class PortableResolverPackagingContractTest {
    @Test
    fun canonicalResolverDependenciesAreDeclaredAsPortableResources() {
        assertEquals(
            listOf(
                "prolog/shared/modules/intent_frames.pl",
                "prolog/shared/modules/normalizer.pl",
                "prolog/shared/kb/intents.pl"
            ),
            PortableSemanticCore.resolverDependencies
        )
        assertTrue(PortableSemanticCore.resources.containsAll(PortableSemanticCore.resolverDependencies))
    }

    @Test
    fun stagerPreservesDeclaredResourceTreeBeforeReturningCore() {
        val root = Files.createTempDirectory("zara-portable-resolver").toFile()
        val payloads = PortableSemanticCore.resources.associateWith { "asset:$it".encodeToByteArray() }
        val source = PortableSemanticAssetSource { path ->
            ByteArrayInputStream(payloads.getValue(path))
        }

        val staged = PortableSemanticAssetStager(root).stageAll(source)

        assertEquals(
            PortableSemanticCore.resources.toSet(),
            staged.resources.keys.toSet()
        )
        PortableSemanticCore.resources.forEach { resource ->
            val file = staged.resources.getValue(resource)
            assertTrue(file.isFile)
            assertEquals("asset:$resource", file.readText())
            assertTrue(file.canonicalPath.startsWith(root.canonicalPath))
        }
        assertEquals(
            staged.resources.getValue(PortableSemanticCore.coreAssetPath).canonicalFile,
            staged.coreFile.canonicalFile
        )
    }

    @Test
    fun portableEntrypointDelegatesResolutionToCanonicalIntentFrames() {
        val core = Files.readString(
            java.nio.file.Path.of("src", "main", "assets", PortableSemanticCore.coreAssetPath)
        )

        assertTrue(core.contains("resolve_frames/4"))
        assertTrue(core.contains("intent_frames:resolve_frames"))
    }
}
