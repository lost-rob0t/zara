package ai.zara.app.localai

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalAiProviderContractTest {
    @Test
    fun serviceClientIsTheEmbeddedProviderAndExposesModelCatalogOperations() {
        val client = File("src/main/java/ai/zara/app/localai/LocalAiServiceClient.kt").readText()
        val provider = File("src/main/java/ai/zara/app/localai/LocalAiProvider.kt").readText()
        val abi = File("src/main/java/ai/zara/app/localai/LocalAiAbi.kt").readText()

        assertTrue(client.contains(") : LocalAiProvider"))
        assertTrue(client.contains("displayName = \"Embedded Local\""))
        assertTrue(client.contains("override fun models()"))
        assertTrue(client.contains("override fun selectModel("))
        assertTrue(provider.contains("const val ID = \"embedded\""))
        assertTrue(provider.contains("class LocalAiProviderRegistry"))
        assertTrue(abi.contains("OP_LIST_MODELS"))
        assertTrue(abi.contains("OP_SELECT_MODEL"))
    }

    @Test
    fun sharedModelContractCanRepresentMultipleProviderFormats() {
        val gguf = LocalModelSpec(
            id = "qwen-fixture",
            version = "1",
            quantization = LocalModelQuantization.Q4_K_M,
            sha256 = "a".repeat(64),
            path = "/private/qwen-fixture.gguf",
            maxContextTokens = 4096,
            backend = LocalModelBackend.CPU,
            format = LocalModelFormat.GGUF,
        )
        val onnx = gguf.copy(
            id = "onnx-fixture",
            path = "/private/onnx-fixture.onnx",
            quantization = LocalModelQuantization.INT8,
            format = LocalModelFormat.ONNX,
        )

        assertEquals(LocalModelFormat.GGUF, gguf.format)
        assertEquals(LocalModelQuantization.Q4_K_M, gguf.quantization)
        assertEquals(LocalModelFormat.ONNX, onnx.format)
    }

    @Test
    fun embeddedProviderRemainsOfflineAndAdvertisesOnlyWhatItActuallyExecutes() {
        val client = File("src/main/java/ai/zara/app/localai/LocalAiServiceClient.kt").readText()
        val service = File("src/main/java/ai/zara/app/localai/LocalAiService.kt").readText()

        assertTrue(client.contains("offlineOnly = true"))
        assertTrue(client.contains("modelContainerExtensions = setOf(\".litertlm\")"))
        assertTrue(client.contains("accelerators = LocalModelBackend.entries.toSet()"))
        assertTrue(service.contains("metadata.format == LocalModelFormat.LITERT_LM"))
    }
}
