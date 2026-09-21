package ai.zara.app.prolog

import ai.zara.app.runtime.LocalQueryResult
import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PureSymbolicConversationControllerTest {
    private val emptyCatalog = PrologWorkspaceCatalog(
        facts = emptyList(),
        rules = emptyList(),
        schemas = emptyList(),
        experts = emptyList(),
        activations = emptyMap(),
    )

    @Test
    fun `natural text uses canonical frame resolver with hard zero usage budget`() {
        val queries = mutableListOf<String>()
        val resolves = mutableListOf<Pair<String, String>>()
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { query ->
                queries += query
                CompletableFuture.completedFuture(LocalQueryResult(query, listOf("unexpected"), 4))
            },
            resolve = { text, conversationId ->
                resolves += text to conversationId
                resolution(
                    turnId = "turn-natural",
                    result = LocalQueryResult("symbolic_dialogue_turn", listOf("Hello from symbols."), 4),
                )
            },
            turnIds = listOf("synthetic-turn-must-not-be-used").iterator(),
        )

        val result = controller.submit("help me", "chat-a").get()

        assertTrue(queries.isEmpty())
        assertEquals(listOf("help me" to "chat-a"), resolves)
        assertEquals(PureSymbolicRoute.FRAME_RESOLVER, result.route)
        assertEquals(0, result.maxModelCalls)
        assertEquals(0, result.maxProviderCalls)
        assertEquals(0, result.providerCalls)
        assertEquals(0, result.modelCalls)
        assertEquals("symbolic-dcg/v1", result.renderer)
        assertEquals("Hello from symbols.", result.turn.text)
        assertEquals("turn-natural", result.turn.turnId)
        assertTrue(result.turn.success)
    }

    @Test
    fun `natural result preserves canonical durable turn identity without minting a second id`() {
        val syntheticIds = listOf("synthetic-turn-must-remain-unused").iterator()
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { error("query path must not run") },
            resolve = { _, _ ->
                resolution(
                    turnId = "canonical-persisted-turn",
                    result = LocalQueryResult("symbolic_dialogue_turn", listOf("ok"), 7),
                )
            },
            turnIds = syntheticIds,
        )

        val result = controller.submit("hello", "chat-a").get()

        assertEquals("canonical-persisted-turn", result.turn.turnId)
        assertTrue("natural route must not allocate a second turn identity", syntheticIds.hasNext())
        assertEquals(0, result.providerCalls)
        assertEquals(0, result.modelCalls)
    }

    @Test
    fun `natural resolver receives normalized canonical conversation id`() {
        var resolvedConversationId = ""
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { error("query path must not run") },
            resolve = { _, conversationId ->
                resolvedConversationId = conversationId
                resolution(
                    turnId = "turn-normalized-conversation",
                    result = LocalQueryResult("symbolic_dialogue_turn", listOf("ok"), 1),
                )
            },
            turnIds = listOf("synthetic-turn-must-not-be-used").iterator(),
        )

        val result = controller.submit("hello", "  chat-a  ").get()

        assertEquals("chat-a", resolvedConversationId)
        assertEquals("chat-a", result.turn.conversationId)
        assertEquals("turn-normalized-conversation", result.turn.turnId)
        assertEquals(0, result.providerCalls)
        assertEquals(0, result.modelCalls)
    }

    @Test
    fun `no symbolic match returns deterministic unsupported result without fallback`() {
        var calls = 0
        val syntheticIds = listOf("synthetic-turn-must-remain-unused").iterator()
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { error("query path must not run") },
            resolve = { text, _ ->
                calls += 1
                resolution(
                    turnId = "turn-unknown",
                    result = LocalQueryResult(text, emptyList(), 9),
                )
            },
            turnIds = syntheticIds,
        )

        val result = controller.submit("make up something", "chat-a").get()

        assertEquals(1, calls)
        assertFalse(result.turn.success)
        assertEquals(
            "I don't have a deterministic symbolic answer for that yet.",
            result.turn.text,
        )
        assertEquals("turn-unknown", result.turn.turnId)
        assertTrue(syntheticIds.hasNext())
        assertEquals(0, result.providerCalls)
        assertEquals(0, result.modelCalls)
    }

    @Test
    fun `symbolic runtime error remains zero-model and preserves canonical turn identity`() {
        val syntheticIds = listOf("synthetic-turn-must-remain-unused").iterator()
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { error("query path must not run") },
            resolve = { _, _ ->
                PureSymbolicResolution(
                    turnId = "turn-error",
                    future = CompletableFuture.failedFuture(
                        IllegalStateException("fixture resolver failure"),
                    ),
                )
            },
            turnIds = syntheticIds,
        )

        val result = controller.submit("hello", "chat-a").get()

        assertFalse(result.turn.success)
        assertEquals(PureSymbolicRoute.FRAME_RESOLVER, result.route)
        assertEquals(
            "The symbolic runtime could not complete this turn.",
            result.turn.text,
        )
        assertEquals("turn-error", result.turn.turnId)
        assertTrue(syntheticIds.hasNext())
        assertEquals(0, result.providerCalls)
        assertEquals(0, result.modelCalls)
    }

    @Test
    fun `cancelling submitted symbolic turn cancels resolver and fences late output`() {
        val resolver = CompletableFuture<LocalQueryResult>()
        val turnIds = listOf("synthetic-turn-must-remain-unused").iterator()
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { error("query path must not run") },
            resolve = { _, _ -> PureSymbolicResolution("turn-cancelled", resolver) },
            turnIds = turnIds,
        )

        val result = controller.submit("keep thinking", "chat-a")

        assertTrue(result.cancel(true))
        assertTrue(result.isCancelled)
        assertTrue(resolver.isCancelled)
        assertTrue(turnIds.hasNext())
        assertFalse(
            resolver.complete(
                LocalQueryResult("symbolic_dialogue_turn", listOf("late(response)"), 11),
            ),
        )
        assertTrue(result.isCancelled)
        assertTrue(turnIds.hasNext())
    }

    @Test
    fun `upstream symbolic cancellation stays cancelled instead of rendering runtime failure`() {
        val resolver = CompletableFuture<LocalQueryResult>()
        val turnIds = listOf("synthetic-turn-must-remain-unused").iterator()
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { error("query path must not run") },
            resolve = { _, _ -> PureSymbolicResolution("turn-upstream-cancelled", resolver) },
            turnIds = turnIds,
        )

        val result = controller.submit("cancel me", "chat-a")
        assertTrue(resolver.cancel(true))

        assertTrue(result.isCancelled)
        assertTrue(turnIds.hasNext())
    }

    @Test
    fun `explicit prolog stays inside symbolic query boundary`() {
        val queries = mutableListOf<String>()
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { query ->
                queries += query
                CompletableFuture.completedFuture(LocalQueryResult(query, listOf("Result = ok"), 2))
            },
            resolve = { _, _ -> error("resolver must not run") },
            turnIds = listOf("turn-query").iterator(),
        )

        val result = controller.submit("?- true.", "chat-a").get()

        assertEquals(listOf("?- true."), queries)
        assertEquals(PureSymbolicRoute.EXPLICIT_QUERY, result.route)
        assertEquals("symbolic-term/v1", result.renderer)
        assertEquals("Result = ok", result.turn.text)
        assertEquals("turn-query", result.turn.turnId)
        assertEquals(0, result.modelCalls)
    }

    @Test
    fun `blank input is rejected before symbolic execution`() {
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { error("must not run") },
            resolve = { _, _ -> error("must not run") },
        )

        try {
            controller.submit("   ", "chat-a")
            throw AssertionError("blank input should be rejected")
        } catch (error: IllegalArgumentException) {
            assertTrue(error.message.orEmpty().contains("required"))
        }
    }

    private fun resolution(
        turnId: String,
        result: LocalQueryResult,
    ): PureSymbolicResolution = PureSymbolicResolution(
        turnId = turnId,
        future = CompletableFuture.completedFuture(result),
    )
}
