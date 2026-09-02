package ai.zara.app.prolog

import java.io.ByteArrayInputStream
import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PortableSemanticAssetStagerTest {

    @Test
    fun stagesCanonicalCoreIntoPrivateFilesystemPath() {
        val root = Files.createTempDirectory("zara-semantic-assets").toFile()
        val expected = "semantic_contract_version('ZARA-SEMANTIC/1').\n".toByteArray()
        val source = RecordingAssetSource(expected)
        val stager = PortableSemanticAssetStager(root)

        val staged = stager.stage(source)

        assertEquals(PortableSemanticCore.coreAssetPath, source.openedPath)
        assertTrue(staged.isFile)
        assertTrue(staged.canonicalPath.startsWith(root.canonicalPath + File.separator))
        assertFalse(staged.path.contains("prolog/portable/semantic_core.pl"))
        assertArrayEquals(expected, staged.readBytes())
    }

    @Test
    fun staleStagedCoreIsReplacedWithCurrentPackagedBytes() {
        val root = Files.createTempDirectory("zara-semantic-assets").toFile()
        val stager = PortableSemanticAssetStager(root)
        val first = stager.stage(RecordingAssetSource("old".toByteArray()))
        first.writeText("stale-on-disk")

        val second = stager.stage(RecordingAssetSource("current".toByteArray()))

        assertEquals(first.canonicalPath, second.canonicalPath)
        assertEquals("current", second.readText())
    }

    @Test
    fun stagingFailureDoesNotReturnFakeReadyPathOrDestroyPreviousGoodCore() {
        val root = Files.createTempDirectory("zara-semantic-assets").toFile()
        val stager = PortableSemanticAssetStager(root)
        val existing = stager.stage(RecordingAssetSource("known-good".toByteArray()))

        try {
            stager.stage(ThrowingAssetSource())
            throw AssertionError("asset read failure must fail closed")
        } catch (_: IllegalStateException) {
        }

        assertTrue(existing.isFile)
        assertEquals("known-good", existing.readText())
    }

    @Test
    fun sourceRejectsTraversalInsteadOfOpeningArbitraryAssetNames() {
        val root = Files.createTempDirectory("zara-semantic-assets").toFile()
        val stager = PortableSemanticAssetStager(root, coreAssetPath = "../secret")
        val source = RecordingAssetSource("secret".toByteArray())

        try {
            stager.stage(source)
            throw AssertionError("traversal path must be rejected")
        } catch (_: IllegalArgumentException) {
        }

        assertEquals(null, source.openedPath)
    }

    @Test
    fun completeSetBuildsFilesystemIndependentDependencyLoader() {
        val root = Files.createTempDirectory("zara-semantic-assets").toFile()
        val payloads = PortableSemanticCore.resources.associateWith { path ->
            "resource:$path\n".toByteArray()
        }
        val staged = PortableSemanticAssetStager(root).stageAll(MapAssetSource(payloads))

        assertTrue(staged.entryFile.isFile)
        val loader = staged.entryFile.readText()
        val dependencyPaths = PortableSemanticCore.resolverDependencies.map { path ->
            staged.resources.getValue(path).canonicalPath
        }
        dependencyPaths.forEach { path ->
            assertTrue("loader must use absolute staged dependency path", loader.contains(path))
        }
        assertTrue(loader.contains(staged.coreFile.canonicalPath))
        assertFalse(
            "loader must not depend on Trealla process working directory",
            loader.contains("../shared/")
        )
        assertTrue(
            dependencyPaths.map(loader::indexOf).zipWithNext().all { (first, second) -> first < second }
        )
        assertTrue(loader.indexOf(dependencyPaths.last()) < loader.indexOf(staged.coreFile.canonicalPath))
    }

    @Test
    fun runtimeAssetInitializationPassesGeneratedLoaderToBridge() {
        val root = Files.createTempDirectory("zara-semantic-assets").toFile()
        val bridge = RecordingBridge()
        val runtime = TreallaSemanticRuntime(bridge)
        val payloads = PortableSemanticCore.resources.associateWith { path ->
            "resource:$path\n".toByteArray()
        }
        val stager = PortableSemanticAssetStager(root)

        runtime.initializeFromAssets(stager, MapAssetSource(payloads))

        val initializedPath = bridge.initializedPath ?: throw AssertionError("bridge was not initialized")
        assertTrue(File(initializedPath).isFile)
        assertTrue(File(initializedPath).canonicalPath.startsWith(root.canonicalPath + File.separator))
        assertTrue(File(initializedPath).name == "portable_loader.pl")
        assertFalse(initializedPath == PortableSemanticCore.coreAssetPath)
    }

    private class RecordingAssetSource(private val bytes: ByteArray) : PortableSemanticAssetSource {
        var openedPath: String? = null

        override fun open(path: String) = ByteArrayInputStream(bytes).also {
            openedPath = path
        }
    }

    private class MapAssetSource(
        private val payloads: Map<String, ByteArray>
    ) : PortableSemanticAssetSource {
        override fun open(path: String) = ByteArrayInputStream(
            payloads[path] ?: error("missing packaged asset: $path")
        )
    }

    private class ThrowingAssetSource : PortableSemanticAssetSource {
        override fun open(path: String) = error("missing packaged asset")
    }

    private class RecordingBridge : TreallaBridge {
        var initializedPath: String? = null

        override fun initialize(coreAssetPath: String) {
            initializedPath = coreAssetPath
        }

        override fun evaluate(query: String): List<String> = emptyList()
        override fun shutdown() = Unit
    }
}
