package ai.zara.app.prolog

import ai.zara.app.expert.PureSymbolicExpertConversationResult
import ai.zara.app.runtime.LocalQueryResult
import java.util.concurrent.CompletableFuture

/**
 * Consumer adapter from an already-admitted canonical expert result into Zara's existing
 * `symbolic_dialogue` response-act and rendering contract.
 *
 * This adapter owns no expert authority, registry, provider runtime, planner, permission state,
 * effect execution, conversation history, or rendering semantics. It only serializes the admitted
 * summary/evidence pair as inert Prolog strings, asks the local symbolic runtime to construct the
 * canonical `answer(expert, ..., evidence(...))` act, and returns the deterministic rendered text.
 */
internal class CanonicalExpertSymbolicRendererAdapter(
    private val queryLocalProlog: (String) -> CompletableFuture<LocalQueryResult>,
) {
    fun render(
        projected: PureSymbolicExpertConversationResult,
    ): CompletableFuture<CanonicalRenderedExpertAnswer> {
        val query = try {
            renderQuery(projected)
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }
        val upstream = try {
            queryLocalProlog(query)
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }
        val output = CancellationPropagatingRenderFuture<CanonicalRenderedExpertAnswer>(upstream)
        upstream.whenComplete { result, error ->
            if (output.isDone) return@whenComplete
            if (error != null || result == null) {
                output.completeExceptionally(
                    error ?: IllegalStateException("Canonical symbolic expert rendering result is missing"),
                )
                return@whenComplete
            }
            try {
                require(result.query == query) {
                    "Canonical symbolic expert renderer returned a result for a different query"
                }
                require(result.generation >= 0L) {
                    "Canonical symbolic expert renderer returned an invalid runtime generation"
                }
                require(result.terms.size == 1) {
                    "Canonical symbolic expert renderer must return exactly one rendered response"
                }
                val text = result.terms.single()
                require(text.isNotBlank()) {
                    "Canonical symbolic expert renderer returned a blank response"
                }
                require(text.length <= MAX_RENDERED_RESPONSE_CHARS) {
                    "Canonical symbolic expert renderer exceeded the portable response bound"
                }
                require(text.none { character ->
                    character.isISOControl() && character !in charArrayOf('\n', '\r', '\t')
                }) {
                    "Canonical symbolic expert renderer returned unsupported control characters"
                }
                requireWellFormedUtf16(text, "rendered expert response")
                output.complete(
                    CanonicalRenderedExpertAnswer(
                        text = text,
                        evidenceRef = projected.evidenceRef,
                        runtimeGeneration = result.generation,
                    ),
                )
            } catch (validationError: Throwable) {
                output.completeExceptionally(validationError)
            }
        }
        return output
    }

    internal fun renderQuery(projected: PureSymbolicExpertConversationResult): String {
        val summary = boundedSummary(projected.summary)
        val evidenceRef = boundedEvidence(projected.evidenceRef)
        val escapedSummary = prologString(summary)
        val escapedEvidence = prologString(evidenceRef)
        return "((string_codes(\"$escapedSummary\", SummaryCodes), " +
            "string_codes(Summary, SummaryCodes), " +
            "string_codes(\"$escapedEvidence\", EvidenceCodes), " +
            "string_codes(EvidenceRef, EvidenceCodes), " +
            "symbolic_dialogue:response_act(" +
            "expert_result(summary(Summary), evidence(EvidenceRef)), Act), " +
            "Act = answer(expert, Summary, evidence(EvidenceRef)), " +
            "symbolic_dialogue:render_response(Act, Result)) -> true ; fail)"
    }

    private fun boundedSummary(value: String): String {
        require(value.isNotBlank()) { "Canonical expert summary must not be blank" }
        require(value.length <= MAX_EXPERT_SUMMARY_CHARS) {
            "Canonical expert summary exceeds the symbolic renderer bound"
        }
        require(value.none { character ->
            character.isISOControl() && character !in charArrayOf('\n', '\r', '\t')
        }) {
            "Canonical expert summary contains unsupported control characters"
        }
        requireWellFormedUtf16(value, "canonical expert summary")
        return value
    }

    private fun boundedEvidence(value: String): String {
        require(value.isNotBlank()) { "Canonical expert evidence must not be blank" }
        require(value.length <= MAX_EXPERT_EVIDENCE_CHARS) {
            "Canonical expert evidence exceeds the symbolic renderer bound"
        }
        require(value.none(Char::isISOControl)) {
            "Canonical expert evidence contains control characters"
        }
        requireWellFormedUtf16(value, "canonical expert evidence")
        return value
    }

    private fun prologString(value: String): String = buildString(value.length + 8) {
        value.forEach { character ->
            when (character) {
                '\\' -> append("\\\\")
                '"' -> append("\\\"")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> append(character)
            }
        }
    }

    private fun requireWellFormedUtf16(value: String, field: String) {
        var index = 0
        while (index < value.length) {
            val character = value[index]
            when {
                Character.isHighSurrogate(character) -> {
                    require(
                        index + 1 < value.length && Character.isLowSurrogate(value[index + 1]),
                    ) {
                        "$field contains an unpaired UTF-16 surrogate"
                    }
                    index += 2
                }
                Character.isLowSurrogate(character) -> {
                    throw IllegalArgumentException("$field contains an unpaired UTF-16 surrogate")
                }
                else -> index += 1
            }
        }
    }

    private class CancellationPropagatingRenderFuture<T>(
        private val upstream: CompletableFuture<*>,
    ) : CompletableFuture<T>() {
        override fun cancel(mayInterruptIfRunning: Boolean): Boolean {
            val cancelled = super.cancel(mayInterruptIfRunning)
            if (cancelled && !upstream.isDone) {
                upstream.cancel(mayInterruptIfRunning)
            }
            return cancelled
        }

        override fun <U> newIncompleteFuture(): CompletableFuture<U> =
            CancellationPropagatingRenderFuture(this)
    }

    private companion object {
        const val MAX_EXPERT_SUMMARY_CHARS = 1_024
        const val MAX_EXPERT_EVIDENCE_CHARS = 256
        const val MAX_RENDERED_RESPONSE_CHARS = 2_048
    }
}

internal data class CanonicalRenderedExpertAnswer(
    val text: String,
    val evidenceRef: String,
    val runtimeGeneration: Long,
)
