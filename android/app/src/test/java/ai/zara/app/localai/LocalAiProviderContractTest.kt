package ai.zara.app.localai

import java.io.File
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
    fun providerRemainsOfflineAndDoesNotClaimUnsupportedContainers() {
        val client = File("src/main/java/ai/zara/app/localai/LocalAiServiceClient.kt").readText()

        assertTrue(client.contains("offlineOnly = true"))
        assertTrue(client.contains("modelContainerExtensions = setOf(\".litertlm\")"))
        assertTrue(client.contains("accelerators = LocalModelBackend.entries.toSet()"))
    }
}
