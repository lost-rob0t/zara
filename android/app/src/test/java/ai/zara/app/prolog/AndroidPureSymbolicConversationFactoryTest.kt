package ai.zara.app.prolog

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
        assertTrue(query.endsWith("symbolic_dialogue:render_response(Act, Result)"))
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
    fun persistedDialogueEnvelopeReturnsResponseAndCanonicalContextWireFromOneDialogueTurn() {
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
            "Context1 must be rendered as a canonical readable Prolog atom before JNI sees it",
            query.contains("term_to_atom(Context1, ContextAtom)"),
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
        assertTrue(query.endsWith("(Result = Response ; Result = ContextWire)"))
        assertTrue(query.contains("valid_dialogue_context(Context0)"))
        assertTrue(query.contains("valid_dialogue_context(Context1)"))
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
