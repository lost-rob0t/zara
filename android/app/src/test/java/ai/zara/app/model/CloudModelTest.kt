package ai.zara.app.model

import ai.zara.app.auth.CredentialCipher
import ai.zara.app.auth.SealedCredential
import java.io.File
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class CloudModelTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun providerEndpointsAreExplicitAndHttpsOnly() {
        assertTrue(
            CloudModelConfig(
                enabled = true,
                endpoint = CloudModelConfig.DEFAULT_STARINTEL_ENDPOINT,
                model = "star-model",
            ).runCatchingValidated().isSuccess
        )
        assertTrue(
            CloudModelConfig(
                enabled = true,
                endpoint = CloudModelConfig.STATINTEL_ENDPOINT,
                model = "stat-model",
            ).runCatchingValidated().isSuccess
        )
        assertTrue(
            CloudModelConfig(
                enabled = true,
                provider = CloudModelProvider.OPENROUTER,
                endpoint = CloudModelConfig.OPENROUTER_ENDPOINT,
                model = "openai/gpt-5",
            ).runCatchingValidated().isSuccess
        )
        assertTrue(
            CloudModelConfig(
                enabled = true,
                provider = CloudModelProvider.ZAI_CODING_PLAN,
                endpoint = CloudModelConfig.ZAI_CODING_ENDPOINT,
                model = "glm-coding",
            ).runCatchingValidated().isSuccess
        )
        assertTrue(
            CloudModelConfig(
                enabled = true,
                endpoint = "http://llm.example.test/v1",
                model = "model",
            ).runCatchingValidated().isFailure
        )
        assertTrue(
            CloudModelConfig(
                enabled = true,
                provider = CloudModelProvider.ZAI_CODING_PLAN,
                endpoint = "https://api.z.ai/api/paas/v4",
                model = "model",
            ).runCatchingValidated().isFailure
        )
    }

    @Test
    fun zaiCodingPlanCannotBecomeGeneralAssistantFallback() {
        val root = temporary.newFolder("zai")
        val configStore = CloudModelConfigStore(File(root, "cloud.properties"))
        configStore.save(
            CloudModelConfig(
                enabled = true,
                provider = CloudModelProvider.ZAI_CODING_PLAN,
                endpoint = CloudModelConfig.ZAI_CODING_ENDPOINT,
                model = "glm-coding",
            )
        )
        val keyStore = CloudApiKeyStore(File(root, "key.bin"), PassThroughCipher())
        keyStore.save("zai-test-key")
        val backend = RecordingBackend()
        val coordinator = CloudModelCoordinator(configStore, keyStore, backend)

        val generalFailure = runCatching {
            coordinator.generate("hello", CloudModelPurpose.GENERAL).get(2, TimeUnit.SECONDS)
        }.exceptionOrNull()
        assertTrue(generalFailure != null)
        assertTrue(backend.purposes.isEmpty())

        val coding = coordinator.generate("write a parser", CloudModelPurpose.CODING)
            .get(2, TimeUnit.SECONDS)
        assertEquals("coding-ok", coding.text)
        assertEquals(listOf(CloudModelPurpose.CODING), backend.purposes)
        coordinator.close()
    }

    @Test
    fun apiKeyNeverLandsInProviderProperties() {
        val root = temporary.newFolder("secrets")
        val configFile = File(root, "cloud.properties")
        val keyFile = File(root, "key.bin")
        val configStore = CloudModelConfigStore(configFile)
        val keyStore = CloudApiKeyStore(keyFile, PassThroughCipher())
        configStore.save(
            CloudModelConfig(
                enabled = true,
                provider = CloudModelProvider.OPENROUTER,
                endpoint = CloudModelConfig.OPENROUTER_ENDPOINT,
                model = "openai/gpt-5",
            )
        )
        keyStore.save("super-secret-token")

        assertFalse(configFile.readText().contains("super-secret-token"))
        assertEquals("super-secret-token", keyStore.load())
    }

    private fun CloudModelConfig.runCatchingValidated(): Result<CloudModelConfig> =
        runCatching { validated() }

    private class PassThroughCipher : CredentialCipher {
        override fun seal(plaintext: ByteArray): SealedCredential =
            SealedCredential(byteArrayOf(1), plaintext.copyOf())

        override fun open(sealed: SealedCredential): ByteArray = sealed.ciphertext.copyOf()
    }

    private class RecordingBackend : CloudModelBackend {
        val purposes = mutableListOf<CloudModelPurpose>()

        override fun generate(
            config: CloudModelConfig,
            apiKey: String,
            request: CloudModelRequest,
            cancelled: () -> Boolean,
            onText: (String) -> Unit,
        ): CloudModelResult {
            purposes += request.purpose
            onText("coding-ok")
            return CloudModelResult(
                identity = CloudModelIdentity(
                    provider = config.provider,
                    endpoint = config.endpoint,
                    model = config.model,
                    appName = request.appName,
                ),
                text = "coding-ok",
                elapsedMs = 1,
            )
        }
    }
}
