package ai.zara.app.model

import ai.zara.app.BuildConfig
import ai.zara.app.conversations.ConversationStore
import ai.zara.app.conversations.cloudModelHistory
import androidx.test.platform.app.InstrumentationRegistry
import java.io.File
import java.math.BigDecimal
import java.util.UUID
import java.util.concurrent.TimeUnit
import org.json.JSONObject
import org.junit.Assert.*
import org.junit.Test

class OpenRouterLiveAcceptanceTest {
    @Test fun twoRealProviderTurnsPreservePersistedContext() {
        val context = InstrumentationRegistry.getInstrumentation().targetContext
        val fixture = File(context.cacheDir, "openrouter-live.json")
        check(fixture.isFile) { "Manual credential and capped-budget preflight required" }
        val input = try { JSONObject(fixture.readText()) } finally { fixture.delete() }
        val budget = BigDecimal(input.getString("budget_usd"))
        require(budget > BigDecimal.ZERO && budget <= BigDecimal("0.05"))
        assertEquals(input.getString("source_sha"), BuildConfig.SOURCE_SHA)
        val receipt = File(context.cacheDir, "openrouter-live-receipt.json")
        receipt.delete()
        val root = File(context.cacheDir, "openrouter-probe-${UUID.randomUUID()}").apply { mkdirs() }
        val configStore = AndroidCloudModelStorage.configStore(root)
        val keyStore = AndroidCloudModelStorage.apiKeyStore(root)
        var coordinator = CloudModelCoordinator(configStore, keyStore)
        try {
            coordinator.configure(CloudModelConfig(enabled = true, provider = CloudModelProvider.OPENROUTER,
                endpoint = CloudModelConfig.OPENROUTER_ENDPOINT, model = input.getString("model"),
                maxOutputTokens = 64, deadlineMs = 30_000,
                openRouterPolicy = OpenRouterProviderPolicy(allowFallbacks = false,
                    maxPromptUsdPerMillion = 1.0, maxCompletionUsdPerMillion = 1.0)))
            coordinator.setApiKey(input.getString("key"))
            val storeFile = File(root, "conversations.bin")
            val store = ConversationStore(storeFile)
            val chat = store.create()
            val nonce = "cobalt-" + UUID.randomUUID().toString().take(8)
            val prompt = "Remember this verification code for my next question: $nonce. Reply only OK."
            store.beginTurn(chat.id, prompt)
            val first = coordinator.generate(prompt, CloudModelPurpose.GENERAL).get(40, TimeUnit.SECONDS)
            store.completeTurn(chat.id, first.text, true)
            coordinator.close()
            coordinator = CloudModelCoordinator(configStore, keyStore)
            val restored = ConversationStore(storeFile)
            val second = coordinator.generate("What verification code did I give you? Reply with that code only.",
                CloudModelPurpose.GENERAL,
                history = restored.state().conversation(chat.id)!!.cloudModelHistory().forConversation(chat.localConversationId)
            ).get(40, TimeUnit.SECONDS)
            assertTrue("Provider did not recall prior turn", second.text.contains(nonce))
            receipt.writeText(JSONObject().put("source_sha", BuildConfig.SOURCE_SHA)
                .put("passed", true).put("paid_requests", 2).put("budget_usd", budget.toPlainString())
                .put("model", input.getString("model")).put("persisted_history_recall", true)
                .put("scope", "installed coordinator and HTTP backend; not composer UI acceptance").toString())
        } finally {
            coordinator.close()
            keyStore.clear()
            root.deleteRecursively()
        }
    }
}
