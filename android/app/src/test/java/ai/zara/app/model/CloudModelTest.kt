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
    fun openRouterPolicyDefaultsAreExplicitAndExactModelOnly() {
        val safe = CloudModelConfig(
            enabled = true,
            provider = CloudModelProvider.OPENROUTER,
            endpoint = CloudModelConfig.OPENROUTER_ENDPOINT,
            model = "openai/gpt-5",
        ).validated()

        assertEquals(OpenRouterProviderSort.PRICE, safe.openRouterPolicy.sort)
        assertTrue(safe.openRouterPolicy.allowFallbacks)
        assertEquals(listOf("fp16", "bf16", "fp8"), safe.openRouterPolicy.quantizations)
        assertEquals(OpenRouterDataCollection.DENY, safe.openRouterPolicy.dataCollection)
        assertTrue(safe.openRouterPolicy.requireParameters)

        val wire = safe.openRouterPolicy.toWireMap()
        assertEquals("price", wire["sort"])
        assertEquals(listOf("fp16", "bf16", "fp8"), wire["quantizations"])
        assertEquals("deny", wire["data_collection"])
        assertFalse(wire.containsKey("models"))
    }

    @Test
    fun openRouterPolicyRoundTripsActorStyleRoutingAndBudgetCaps() {
        val root = temporary.newFolder("openrouter-policy")
        val configFile = File(root, "cloud.properties")
        val store = CloudModelConfigStore(configFile)
        val expected = CloudModelConfig(
            enabled = true,
            provider = CloudModelProvider.OPENROUTER,
            endpoint = CloudModelConfig.OPENROUTER_ENDPOINT,
            model = "anthropic/claude-sonnet-4.5",
            openRouterPolicy = OpenRouterProviderPolicy(
                sort = OpenRouterProviderSort.THROUGHPUT,
                allowFallbacks = false,
                quantizations = listOf("fp16", "bf16"),
                dataCollection = OpenRouterDataCollection.DENY,
                zeroDataRetention = true,
                requireParameters = true,
                order = listOf("anthropic", "google-vertex"),
                only = listOf("anthropic", "google-vertex"),
                ignore = listOf("deepinfra"),
                maxPromptUsdPerMillion = 4.0,
                maxCompletionUsdPerMillion = 20.0,
            ),
        ).validated()

        store.save(expected)
        assertEquals(expected, store.load())
        assertTrue(configFile.readText().contains("schema_version=1"))
        assertTrue(configFile.readText().contains("openrouter.max_prompt_usd_per_m=4.0"))
        assertFalse(configFile.readText().contains("models="))
    }

    @Test
    fun openRouterQuantizationsNormalizeCaseAndRejectUnknownOrDuplicates() {
        val normalized = OpenRouterProviderPolicy(
            quantizations = listOf("FP16", "BF16", "FP8"),
        ).validated()
        assertEquals(listOf("fp16", "bf16", "fp8"), normalized.quantizations)

        assertTrue(
            runCatching {
                OpenRouterProviderPolicy(quantizations = listOf("UNKNOWN")).validated()
            }.isFailure
        )
        assertTrue(
            runCatching {
                OpenRouterProviderPolicy(quantizations = listOf("fp16", "FP16")).validated()
            }.isFailure
        )
    }

    @Test
    fun openRouterProviderSlugsNormalizeCaseAndRejectCaseFoldedDuplicates() {
        val normalized = OpenRouterProviderPolicy(
            order = listOf("Anthropic", "Google-Vertex"),
            only = listOf("ANTHROPIC", "google-vertex"),
            ignore = listOf("DeepInfra"),
        ).validated()

        assertEquals(listOf("anthropic", "google-vertex"), normalized.order)
        assertEquals(listOf("anthropic", "google-vertex"), normalized.only)
        assertEquals(listOf("deepinfra"), normalized.ignore)
        assertTrue(
            runCatching {
                OpenRouterProviderPolicy(order = listOf("Anthropic", "anthropic")).validated()
            }.isFailure
        )
    }

    @Test
    fun openRouterPolicyFailsClosedOnUnknownQuantizationAndConflictingProviders() {
        assertTrue(
            runCatching {
                OpenRouterProviderPolicy(quantizations = listOf("unknown")).validated()
            }.isFailure
        )
        assertTrue(
            runCatching {
                OpenRouterProviderPolicy(
                    only = listOf("OPENAI"),
                    ignore = listOf("openai"),
                ).validated()
            }.isFailure
        )
        assertTrue(
            runCatching {
                OpenRouterProviderPolicy(maxPromptUsdPerMillion = Double.NaN).validated()
            }.isFailure
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
