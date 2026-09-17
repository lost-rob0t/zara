package ai.zara.app.localai

import java.nio.file.Files
import java.security.MessageDigest
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalModelStoreTest {
    @Test
    fun installPersistsExplicitMetadataAndVerifiesShaBeforeActivation() {
        val root = Files.createTempDirectory("zara-model-store").toFile()
        val source = Files.createTempFile("zara-model", ".litertlm").toFile().apply {
            writeText("verified model bytes")
        }
        val expectedSha = sha256(source.readBytes())
        val store = LocalModelStore(root)
        val metadata = LocalModelMetadata(
            id = "tiny-fixture",
            version = "2026.09",
            quantization = LocalModelQuantization.INT4,
            sha256 = expectedSha,
            maxContextTokens = 2048,
            backend = LocalModelBackend.CPU,
        )

        val installed = source.inputStream().use { store.install(it, metadata) }
        val active = store.activeModel()

        assertEquals(installed, active)
        assertEquals(expectedSha, active?.sha256)
        assertEquals(LocalModelQuantization.INT4, active?.quantization)
        assertTrue(active!!.path.startsWith(root.canonicalPath))
    }

    @Test
    fun installRejectsChecksumMismatchWithoutActivatingModel() {
        val root = Files.createTempDirectory("zara-model-store").toFile()
        val store = LocalModelStore(root)
        val metadata = LocalModelMetadata(
            id = "bad-fixture",
            version = "1",
            quantization = LocalModelQuantization.DYNAMIC_INT4,
            sha256 = "0".repeat(64),
            maxContextTokens = 1024,
            backend = LocalModelBackend.CPU,
        )

        assertThrows(IllegalArgumentException::class.java) {
            "wrong bytes".byteInputStream().use { store.install(it, metadata) }
        }
        assertEquals(null, store.activeModel())
        assertFalse(root.walkTopDown().any { it.extension == "litertlm" })
    }

    @Test
    fun tamperingAfterInstallIsRejectedOnNextLoad() {
        val root = Files.createTempDirectory("zara-model-store").toFile()
        val bytes = "good model".toByteArray()
        val metadata = LocalModelMetadata(
            id = "tamper-fixture",
            version = "1",
            quantization = LocalModelQuantization.INT8,
            sha256 = sha256(bytes),
            maxContextTokens = 1024,
            backend = LocalModelBackend.CPU,
        )
        val store = LocalModelStore(root)
        val installed = bytes.inputStream().use { store.install(it, metadata) }
        java.io.File(installed.path).writeText("tampered")

        assertThrows(IllegalStateException::class.java) {
            store.activeModel()
        }
    }

    private fun sha256(bytes: ByteArray): String =
        MessageDigest.getInstance("SHA-256")
            .digest(bytes)
            .joinToString("") { "%02x".format(it) }
}
