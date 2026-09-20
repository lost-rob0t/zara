package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidSymbolicPersistenceCompositionContractTest {
    @Test
    fun `pure symbolic chat uses one canonical durable conversation owner`() {
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

        assertFalse(
            "pure-symbolic chat must not keep the legacy conversations.bin owner beside zara.db",
            activity.contains("ConversationStore(File(filesDir, \"conversations.bin\"))"),
        )
        assertTrue(
            "Android chat must be wired to the canonical portable conversation owner",
            activity.contains("PortableConversationStore"),
        )
    }

    @Test
    fun `natural symbolic turns load and generation fence canonical projection context`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()

        assertTrue(
            "natural turns must load Context0 from the canonical symbolic projection",
            factory.contains("loadSymbolicProjection("),
        )
        assertTrue(
            "natural turns must persist Context1 through the canonical projection CAS",
            factory.contains("saveSymbolicProjection("),
        )
        assertTrue(
            "projection persistence must carry the expected generation fence",
            factory.contains("expectedGeneration"),
        )
        assertFalse(
            "natural turns must not reset dialogue context on every request",
            factory.contains("conversation, []"),
        )
        assertFalse(
            "natural turns must not discard the returned dialogue context",
            factory.contains("_Context"),
        )
    }

    @Test
    fun `pure symbolic composition never introduces provider fallback`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()
        val controller = File(
            "src/main/java/ai/zara/app/prolog/PureSymbolicConversationController.kt"
        ).readText()

        assertFalse(factory.contains("providerClient"))
        assertFalse(factory.contains("modelClient"))
        assertTrue(controller.contains("maxModelCalls: Int = 0"))
        assertTrue(controller.contains("maxProviderCalls: Int = 0"))
        assertTrue(controller.contains("modelCalls: Int = 0"))
        assertTrue(controller.contains("providerCalls: Int = 0"))
    }
}
