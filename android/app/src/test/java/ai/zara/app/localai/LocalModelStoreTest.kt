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
    fun catalogKeepsMultipleVerifiedModelsAndCanReactivateEitherOne() {
        val root = Files.createTempDirectory("zara-model-store").toFile()
        val store = LocalModelStore(root)
        val firstBytes = "first model".toByteArray()
        val secondBytes = "second model".toByteArray()
        val first = LocalModelMetadata(
            id = "gemma-fixture",
            version = "1",
            quantization = LocalModelQuantization.INT4,
            sha256 = sha256(firstBytes),
            maxContextTokens = 2048,
            backend = LocalModelBackend.CPU,
        )
        val second = LocalModelMetadata(
            id = "qwen-fixture",
            version = "2",
            quantization = LocalModelQuantization.INT8,
            sha256 = sha256(secondBytes),
            maxContextTokens = 4096,
            backend = LocalModelBackend.GPU,
        )

        firstBytes.inputStream().use { store.install(it, first) }
        secondBytes.inputStream().use { store.install(it, second) }

        assertEquals(
            setOf("gemma-fixture@1", "qwen-fixture@2"),
            store.installedModels().map { "${it.id}@${it.version}" }.toSet(),
        )
        assertEquals("qwen-fixture", store.activeModel()?.id)

        val selected = requireNotNull(store.model("gemma-fixture", "1"))
        store.activate(selected)
        assertEquals("gemma-fixture", store.activeModel()?.id)
        assertEquals(2, store.installedModels().size)
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
        assertThrows(IllegalStateException::class.java) {
            store.installedModels()
        }
    }

    private fun sha256(bytes: ByteArray): String =
        MessageDigest.getInstance("SHA-256")
            .digest(bytes)
            .joinToString("") { "%02x".format(it) }
}
