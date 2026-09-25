package ai.zara.app.prolog

import ai.zara.app.expert.PureSymbolicExpertConversationResult
import ai.zara.app.runtime.LocalQueryResult
import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class CanonicalExpertSymbolicRendererAdapterTest {
    @Test
    fun admittedExpertResultCrossesCanonicalResponseActAndRenderer() {
        var capturedQuery: String? = null
        val adapter = CanonicalExpertSymbolicRendererAdapter { query ->
            capturedQuery = query
            CompletableFuture.completedFuture(
                LocalQueryResult(
                    query = query,
                    terms = listOf("triage says alex is stable"),
                    generation = 17L,
                ),
            )
        }

        val rendered = adapter.render(projected()).get()
        val query = requireNotNull(capturedQuery)

        assertTrue(
            query.contains(
                "symbolic_dialogue:response_act(" +
                    "expert_result(summary(Summary), evidence(EvidenceRef)), Act)",
            ),
        )
        assertTrue(query.contains("Act = answer(expert, Summary, evidence(EvidenceRef))"))
        assertTrue(query.contains("symbolic_dialogue:render_response(Act, Result)"))
        assertFalse(query.contains("expert.invoke"))
        assertEquals("triage says alex is stable", rendered.text)
        assertEquals("evidence:triage:42", rendered.evidenceRef)
        assertEquals(17L, rendered.runtimeGeneration)
    }

    @Test
    fun cancellationOfMappedRenderedAnswerCancelsLocalSymbolicRuntimeQuery() {
        val localQuery = CompletableFuture<LocalQueryResult>()
        val adapter = CanonicalExpertSymbolicRendererAdapter { localQuery }

        val rendered = adapter.render(projected())
        val uiEnvelope = rendered.thenApply { result -> result.text }

        assertTrue(uiEnvelope.cancel(true))
        assertTrue(uiEnvelope.isCancelled)
        assertTrue(rendered.isCancelled)
        assertTrue(localQuery.isCancelled)
    }

    @Test
    fun malformedRendererEnvelopeFailsClosed() {
        val adapter = CanonicalExpertSymbolicRendererAdapter { query ->
            CompletableFuture.completedFuture(
                LocalQueryResult(
                    query = query,
                    terms = listOf("first", "second"),
                    generation = 17L,
                ),
            )
        }

        val rendered = adapter.render(projected())

        assertTrue(rendered.isCompletedExceptionally)
    }

    @Test
    fun summaryAndEvidenceAreSerializedAsInertPrologStrings() {
        var capturedQuery: String? = null
        val adapter = CanonicalExpertSymbolicRendererAdapter { query ->
            capturedQuery = query
            CompletableFuture.completedFuture(
                LocalQueryResult(
                    query = query,
                    terms = listOf("quoted result"),
                    generation = 3L,
                ),
            )
        }
        val projected = PureSymbolicExpertConversationResult(
            summary = "quoted \"value\" at C:\\tmp\nnext",
            evidenceRef = "evidence:triage/quoted",
        )

        val rendered = adapter.render(projected).get()
        val query = requireNotNull(capturedQuery)

        assertTrue(query.contains("quoted \\\"value\\\" at C:\\\\tmp\\nnext"))
        assertTrue(query.contains("evidence:triage/quoted"))
        assertEquals("evidence:triage/quoted", rendered.evidenceRef)
    }

    private fun projected(): PureSymbolicExpertConversationResult =
        PureSymbolicExpertConversationResult(
            summary = "triage says alex is stable",
            evidenceRef = "evidence:triage:42",
        )
}
