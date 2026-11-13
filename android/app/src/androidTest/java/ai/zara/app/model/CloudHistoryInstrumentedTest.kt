package ai.zara.app.model

import ai.zara.app.conversations.ConversationStore
import ai.zara.app.conversations.cloudModelHistory
import androidx.test.platform.app.InstrumentationRegistry
import java.io.ByteArrayOutputStream
import java.io.File
import java.net.HttpURLConnection
import java.net.URL
import java.util.concurrent.TimeUnit
import org.json.JSONObject
import org.junit.Assert.*
import org.junit.Test

class CloudHistoryInstrumentedTest {
    @Test fun persistedChatsReachHttpRequestsInIsolationAfterRecreation() {
        val context = InstrumentationRegistry.getInstrumentation().targetContext
        val root = File(context.cacheDir, "cloud-history-${System.nanoTime()}").apply { mkdirs() }
        val payloads = mutableListOf<JSONObject>()
        val backend = OpenAiCompatibleCloudModelBackend { url -> RecordingConnection(url, payloads) }
        val coordinator = CloudModelCoordinator(
            AndroidCloudModelStorage.configStore(root),
            AndroidCloudModelStorage.apiKeyStore(root),
            backend,
        )
        try {
            coordinator.configure(CloudModelConfig(enabled = true, provider = CloudModelProvider.OPENROUTER,
                endpoint = CloudModelConfig.OPENROUTER_ENDPOINT, model = "fixture/model"))
            coordinator.setApiKey("synthetic-instrumentation-key")
            val file = File(root, "conversations.bin")
            val store = ConversationStore(file)
            val a = store.create()
            store.beginTurn(a.id, "Remember cobalt")
            val first = coordinator.generate("Remember cobalt", CloudModelPurpose.GENERAL,
                history = store.state().conversation(a.id)!!.cloudModelHistory().forConversation(a.localConversationId)).get(5, TimeUnit.SECONDS)
            store.completeTurn(a.id, first.text, true)
            val b = store.create()
            val reopened = ConversationStore(file)
            coordinator.generate("Recall it", CloudModelPurpose.GENERAL,
                history = reopened.state().conversation(a.id)!!.cloudModelHistory().forConversation(a.localConversationId)).get(5, TimeUnit.SECONDS)
            coordinator.generate("New chat", CloudModelPurpose.GENERAL,
                history = reopened.state().conversation(b.id)!!.cloudModelHistory().forConversation(b.localConversationId)).get(5, TimeUnit.SECONDS)
            assertEquals(2, payloads[0].getJSONArray("messages").length())
            val second = payloads[1].getJSONArray("messages")
            assertEquals(4, second.length())
            assertEquals("Remember cobalt", second.getJSONObject(1).getString("content"))
            assertEquals("Cobalt noted", second.getJSONObject(2).getString("content"))
            assertEquals("Recall it", second.getJSONObject(3).getString("content"))
            assertEquals(2, payloads[2].getJSONArray("messages").length())
            assertFalse(payloads[2].toString().contains("cobalt", ignoreCase = true))
        } finally {
            coordinator.close()
            root.deleteRecursively()
        }
    }

    private class RecordingConnection(url: URL, private val payloads: MutableList<JSONObject>) : HttpURLConnection(url) {
        private val output = ByteArrayOutputStream()
        override fun connect() {}
        override fun disconnect() {}
        override fun usingProxy() = false
        override fun getOutputStream() = output
        override fun getResponseCode(): Int {
            payloads.add(JSONObject(output.toString("UTF-8")))
            return 200
        }
        override fun getContentType() = "application/json"
        override fun getInputStream() = """{"choices":[{"message":{"content":"Cobalt noted"}}]}""".byteInputStream()
    }
}
