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
    fun runtimeAssetInitializationPassesRealStagedFilesystemPathToBridge() {
        val root = Files.createTempDirectory("zara-semantic-assets").toFile()
        val bridge = RecordingBridge()
        val runtime = TreallaSemanticRuntime(bridge)
        val stager = PortableSemanticAssetStager(root)

        runtime.initializeFromAssets(stager, RecordingAssetSource("semantic_core".toByteArray()))

        val initializedPath = bridge.initializedPath ?: throw AssertionError("bridge was not initialized")
        assertTrue(File(initializedPath).isFile)
        assertTrue(File(initializedPath).canonicalPath.startsWith(root.canonicalPath + File.separator))
        assertFalse(initializedPath == PortableSemanticCore.coreAssetPath)
    }

    private class RecordingAssetSource(private val bytes: ByteArray) : PortableSemanticAssetSource {
        var openedPath: String? = null

        override fun open(path: String) = ByteArrayInputStream(bytes).also {
            openedPath = path
        }
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
