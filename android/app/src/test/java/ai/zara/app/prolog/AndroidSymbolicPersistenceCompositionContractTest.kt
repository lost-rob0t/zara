package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertEquals
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
    fun `symbolic factory cannot be created without canonical durable projection store`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()

        assertFalse(
            "a stateless factory overload would reintroduce per-turn Context0 reset",
            factory.contains("fun create(session: AndroidAppSession): PureSymbolicConversationController"),
        )
        assertTrue(factory.contains("projectionStore: PortableConversationStore"))
        assertTrue(factory.contains("resolvePersistedTurn("))
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
    fun `natural symbolic turns derive project scope from the canonical conversation binding`() {
        val activity = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()

        assertTrue(
            "MainActivity must pass the selected conversation project binding into pure-symbolic execution",
            activity.contains("projectIdForConversation"),
        )
        assertTrue(
            "the symbolic factory must consume the existing conversation project binding, not invent a project store",
            factory.contains("projectIdForConversation"),
        )
        assertTrue(
            "each pending symbolic turn must derive project id/generation through the canonical project-scope contract",
            factory.contains("SymbolicProjectScopeContract.next("),
        )
        assertTrue(
            "the pending canonical projection must carry the derived project id",
            factory.contains("projectId = projectScope.projectId"),
        )
        assertTrue(
            "the pending canonical projection must carry the derived stale-project generation fence",
            factory.contains("projectGeneration = projectScope.projectGeneration"),
        )
    }

    @Test
    fun `one natural turn executes canonical dialogue exactly once`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()
        val persistedTurn = factory
            .substringAfter("private fun resolvePersistedTurn(")
            .substringBefore("private fun splitDialogueEnvelope(")

        assertTrue(persistedTurn.contains("dialogueTurnEnvelopeQuery(utterance, context0)"))
        assertEquals(
            "response rendering and Context1 capture must share one local Prolog evaluation",
            1,
            Regex("queryLocalProlog\\(").findAll(persistedTurn).count(),
        )
        assertFalse(
            "Context1 must not be recovered by replaying the dialogue turn",
            persistedTurn.contains("dialogueContextQuery("),
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
