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
        val resolves = mutableListOf<String>()
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { query ->
                queries += query
                CompletableFuture.completedFuture(LocalQueryResult(query, listOf("unexpected"), 4))
            },
            resolve = { text ->
                resolves += text
                CompletableFuture.completedFuture(
                    LocalQueryResult("resolve_frames", listOf("response_act(help)"), 4),
                )
            },
            turnIds = listOf("turn-natural").iterator(),
        )

        val result = controller.submit("help me", "chat-a").get()

        assertTrue(queries.isEmpty())
        assertEquals(listOf("help me"), resolves)
        assertEquals(PureSymbolicRoute.FRAME_RESOLVER, result.route)
        assertEquals(0, result.maxModelCalls)
        assertEquals(0, result.maxProviderCalls)
        assertEquals(0, result.providerCalls)
        assertEquals(0, result.modelCalls)
        assertEquals("symbolic-term/v1", result.renderer)
        assertEquals("response_act(help)", result.turn.text)
        assertTrue(result.turn.success)
    }

    @Test
    fun `no symbolic match returns deterministic unsupported result without fallback`() {
        var calls = 0
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { error("query path must not run") },
            resolve = { text ->
                calls += 1
                CompletableFuture.completedFuture(LocalQueryResult(text, emptyList(), 9))
            },
            turnIds = listOf("turn-unknown").iterator(),
        )

        val result = controller.submit("make up something", "chat-a").get()

        assertEquals(1, calls)
        assertFalse(result.turn.success)
        assertEquals(
            "I don't have a deterministic symbolic answer for that yet.",
            result.turn.text,
        )
        assertEquals(0, result.providerCalls)
        assertEquals(0, result.modelCalls)
    }

    @Test
    fun `symbolic runtime error remains zero-model and never becomes provider fallback`() {
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { error("query path must not run") },
            resolve = {
                CompletableFuture.failedFuture(IllegalStateException("fixture resolver failure"))
            },
            turnIds = listOf("turn-error").iterator(),
        )

        val result = controller.submit("hello", "chat-a").get()

        assertFalse(result.turn.success)
        assertEquals(PureSymbolicRoute.FRAME_RESOLVER, result.route)
        assertEquals(
            "The symbolic runtime could not complete this turn.",
            result.turn.text,
        )
        assertEquals(0, result.providerCalls)
        assertEquals(0, result.modelCalls)
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
            resolve = { error("resolver must not run") },
            turnIds = listOf("turn-query").iterator(),
        )

        val result = controller.submit("?- true.", "chat-a").get()

        assertEquals(listOf("?- true."), queries)
        assertEquals(PureSymbolicRoute.EXPLICIT_QUERY, result.route)
        assertEquals("Result = ok", result.turn.text)
        assertEquals(0, result.modelCalls)
    }

    @Test
    fun `blank input is rejected before symbolic execution`() {
        val controller = PureSymbolicConversationController(
            catalog = { emptyCatalog },
            query = { error("must not run") },
            resolve = { error("must not run") },
        )

        try {
            controller.submit("   ", "chat-a")
            throw AssertionError("blank input should be rejected")
        } catch (error: IllegalArgumentException) {
            assertTrue(error.message.orEmpty().contains("required"))
        }
    }
}
