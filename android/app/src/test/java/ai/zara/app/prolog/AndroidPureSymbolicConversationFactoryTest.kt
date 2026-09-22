package ai.zara.app.prolog

import ai.zara.app.expert.PureSymbolicExpertConversationResult
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidPureSymbolicConversationFactoryTest {
    @Test
    fun dialogueTurnQueryUsesCanonicalDialogueTurnAndRenderer() {
        val query = AndroidPureSymbolicConversationFactory.dialogueTurnQuery(
            "  set a timer for \"five\"\\minutes\nplease  ",
        )

        assertTrue(query.contains("string_codes(\"[]\", Context0Codes)"))
        assertTrue(query.contains("atom_codes(Context0Atom, Context0Codes)"))
        assertTrue(query.contains("read_term_from_atom(Context0Atom, Context0, [])"))
        assertFalse(query.contains("atom_string("))
        assertFalse(query.contains("term_string("))
        assertTrue(query.contains("symbolic_dialogue_turn:valid_dialogue_context(Context0)"))
        assertTrue(query.contains("symbolic_dialogue_turn:dialogue_turn("))
        assertTrue(query.contains("conversation, Context0, turn(_Frames, Act, Context1)"))
        assertTrue(query.contains("symbolic_dialogue_turn:valid_dialogue_context(Context1)"))
        assertTrue(query.endsWith(", symbolic_dialogue:render_response(Act, Result)"))
        assertTrue(query.contains("set a timer for \\\"five\\\"\\\\minutes\\nplease"))
        assertFalse(query.contains("resolve_frames("))
    }

    @Test
    fun dialogueTurnQueryDoesNotResetOrDiscardConversationContext() {
        val context = "partial_frame(frame(intent(ns(device),name('timer.set')),[],missing([duration])),[duration])"
        val query = AndroidPureSymbolicConversationFactory.dialogueTurnQuery("five minutes", context)

        assertFalse(
            "natural turns must load canonical persisted Context0 instead of resetting dialogue state",
            query.contains("conversation, []"),
        )
        assertFalse(
            "natural turns must expose Context1 for generation-fenced persistence instead of discarding it",
            query.contains("_Context"),
        )
        assertTrue(query.contains("partial_frame("))
        assertTrue(query.contains("Context1"))
    }

    @Test
    fun persistedDialogueEnvelopeReturnsResponseCanonicalContextActAndExpertEvidenceFromOneDialogueTurn() {
        val context = "completed_frame(frame(intent(ns(device),name('timer.set')),[],complete))"
        val query = AndroidPureSymbolicConversationFactory.dialogueTurnEnvelopeQuery(
            "actually ten minutes",
            context,
        )

        assertFalse(
            "internal natural-turn query must not use once/1 because the canonical bounded mobile query policy forbids it",
            query.contains("once("),
        )
        assertFalse(
            "Android's pinned Trealla runtime does not provide term_string/2",
            query.contains("term_string("),
        )
        assertFalse(
            "Android's pinned Trealla runtime does not provide atom_string/2",
            query.contains("atom_string("),
        )
        assertTrue(
            "persisted Context0 must cross the SWI/Trealla read_term_from_atom/3 boundary",
            query.contains("read_term_from_atom("),
        )
        assertTrue(
            "Context1 must be rendered with the same quoted reader-safe writer exercised by pinned SWI/Trealla parity",
            query.contains("write_term_to_atom(ContextAtom, Context1, [quoted(true)])"),
        )
        assertTrue(
            "canonical Context1 must cross JNI as a real string through portable code-list conversion",
            query.contains("atom_codes(ContextTagged, ContextWireCodes)") &&
                query.contains("string_codes(ContextWire, ContextWireCodes)"),
        )
        assertTrue(
            "the dedicated wire tag must distinguish continuation state from the rendered response without a second dialogue turn",
            query.contains("__zara_context__:"),
        )
        assertTrue(
            "canonical response act must cross JNI as a bounded tagged string from the same dialogue turn",
            query.contains("functor(Act, ActName, _)") &&
                query.contains("__zara_act__:") &&
                query.contains("string_codes(ActWire, ActWireCodes)"),
        )
        assertTrue(
            "expert response acts must carry both the canonical expert_answer token and their canonical evidence reference",
            query.contains("Act = answer(expert, _, evidence(EvidenceRef))") &&
                query.contains("ActName = expert_answer") &&
                query.contains("__zara_expert_evidence__:") &&
                query.contains("string_codes(EvidenceWire, EvidenceWireCodes)"),
        )
        assertTrue(
            "ISO if-then-else must commit the router/renderer/context serialization condition before Result enumeration",
            query.startsWith("((") && query.contains(") -> true ; fail), "),
        )
        assertEquals(
            "generated natural-turn query must cross the same bounded mobile query policy as the real Android runtime",
            query,
            PrologQueryPolicy.requireSafe(query),
        )
        assertEquals(
            "one user turn must execute canonical dialogue_turn exactly once",
            1,
            Regex("symbolic_dialogue_turn:dialogue_turn\\(").findAll(query).count(),
        )
        assertTrue(query.contains("symbolic_dialogue:render_response(Act, Response)"))
        assertTrue(
            query.endsWith(
                "(Result = Response ; Result = ContextWire ; Result = ActWire ; Result = EvidenceWire)",
            ),
        )
        assertTrue(query.contains("valid_dialogue_context(Context0)"))
        assertTrue(query.contains("valid_dialogue_context(Context1)"))
    }

    @Test
    fun canonicalExpertEnvelopeProjectsOnlyAdmittedSummaryEvidenceAndContext() {
        val context = "completed_frame(frame(intent(ns(device),name('timer.set')),[],complete))"
        val result = AndroidPureSymbolicConversationFactory.canonicalExpertEnvelopeResult(
            projected = PureSymbolicExpertConversationResult(
                summary = "triage says alex is stable",
                evidenceRef = "evidence:triage:42",
            ),
            contextTerm = context,
            generation = 11L,
        )

        assertEquals("expert.invoke", result.query)
        assertEquals(11L, result.generation)
        assertEquals(
            listOf(
                "triage says alex is stable",
                "__zara_context__:$context",
                "__zara_act__:expert_answer",
                "__zara_expert_evidence__:evidence:triage:42",
            ),
            result.terms,
        )
        assertFalse(result.query.contains("triage_explain"))
    }

    @Test
    fun canonicalExpertEnvelopeRejectsNonCanonicalContextOrInvalidGeneration() {
        val projected = PureSymbolicExpertConversationResult(
            summary = "triage says alex is stable",
            evidenceRef = "evidence:triage:42",
        )

        assertThrows(IllegalArgumentException::class.java) {
            AndroidPureSymbolicConversationFactory.canonicalExpertEnvelopeResult(
                projected = projected,
                contextTerm = "future_context(foo)",
                generation = 11L,
            )
        }
        assertThrows(IllegalArgumentException::class.java) {
            AndroidPureSymbolicConversationFactory.canonicalExpertEnvelopeResult(
                projected = projected,
                contextTerm = "[]",
                generation = -1L,
            )
        }
    }

    @Test
    fun canonicalRenderedExpertEnvelopePreservesRenderedTextEvidenceAndRendererGeneration() {
        val context = "completed_frame(frame(intent(ns(device),name('timer.set')),[],complete))"
        val result = AndroidPureSymbolicConversationFactory.canonicalRenderedExpertEnvelopeResult(
            rendered = CanonicalRenderedExpertAnswer(
                text = "Triage says Alex is stable.",
                evidenceRef = "evidence:triage:42",
                runtimeGeneration = 17L,
            ),
            contextTerm = context,
        )

        assertEquals("expert.invoke", result.query)
        assertEquals(17L, result.generation)
        assertEquals(
            listOf(
                "Triage says Alex is stable.",
                "__zara_context__:$context",
                "__zara_act__:expert_answer",
                "__zara_expert_evidence__:evidence:triage:42",
            ),
            result.terms,
        )
    }

    @Test
    fun canonicalRenderedExpertEnvelopeRejectsNonCanonicalContextOrInvalidRendererGeneration() {
        val rendered = CanonicalRenderedExpertAnswer(
            text = "Triage says Alex is stable.",
            evidenceRef = "evidence:triage:42",
            runtimeGeneration = 17L,
        )

        assertThrows(IllegalArgumentException::class.java) {
            AndroidPureSymbolicConversationFactory.canonicalRenderedExpertEnvelopeResult(
                rendered = rendered,
                contextTerm = "future_context(foo)",
            )
        }
        assertThrows(IllegalArgumentException::class.java) {
            AndroidPureSymbolicConversationFactory.canonicalRenderedExpertEnvelopeResult(
                rendered = rendered.copy(runtimeGeneration = -1L),
                contextTerm = "[]",
            )
        }
    }

    @Test
    fun dialogueTurnQueryRejectsBlankInput() {
        assertThrows(IllegalArgumentException::class.java) {
            AndroidPureSymbolicConversationFactory.dialogueTurnQuery("   ")
        }
    }

    @Test
    fun dialogueTurnQueryRejectsNonCanonicalContextShape() {
        assertThrows(IllegalArgumentException::class.java) {
            AndroidPureSymbolicConversationFactory.dialogueTurnQuery(
                "hello",
                "future_context(foo)",
            )
        }
    }

    @Test
    fun dialogueTurnQueryKeepsAndroidRuntimeBound() {
        val oversized = "x".repeat(8_193)

        assertThrows(IllegalArgumentException::class.java) {
            AndroidPureSymbolicConversationFactory.dialogueTurnQuery(oversized)
        }
    }
}
