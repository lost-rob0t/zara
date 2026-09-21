package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidPinnedTreallaContextInputContractTest {
    @Test
    fun persistedDialogueContextBridgesStringDataToPinnedTreallaAtomInput() {
        val context =
            "partial_frame(frame(intent(ns(device),name('timer.set')),[],missing([duration])),[duration])"
        val query = AndroidPureSymbolicConversationFactory.dialogueTurnEnvelopeQuery(
            utterance = "5 minutes",
            contextTerm = context,
        )

        assertFalse(
            "Trealla v3.9.22 read_term_from_atom/3 is typed +atom; Android must not feed its persisted context as a double-quoted Prolog string",
            Regex("read_term_from_atom\\(\\\"").containsMatchIn(query),
        )
        assertFalse(
            "pinned Trealla does not provide atom_string/2; the natural-turn envelope must not depend on it",
            query.contains("atom_string("),
        )
        assertFalse(
            "SWI-Prolog does not provide write_term_to_atom/3; the Android envelope must stay on the shared SWI/Trealla surface",
            query.contains("write_term_to_atom("),
        )
        assertTrue(
            "Context1 must be rendered as a quoted reader-safe atom through the portable output-capture surface",
            query.contains("with_output_to(atom(ContextAtom), write_term(Context1, [quoted(true)]))"),
        )
        assertTrue(
            "persisted context must be decoded to code points before crossing the portable atom bridge",
            query.contains("string_codes("),
        )
        assertTrue(
            "persisted context codes must become the atom required by pinned Trealla read_term_from_atom/3",
            query.contains("atom_codes(Context0Atom, Context0Codes)"),
        )
        assertTrue(
            "read_term_from_atom/3 must consume the bridged atom, not the durable string literal directly",
            query.contains("read_term_from_atom(Context0Atom, Context0, [])"),
        )
        assertTrue(query.contains("symbolic_dialogue_turn:valid_dialogue_context(Context0)"))
        assertTrue(query.contains("symbolic_dialogue_turn:valid_dialogue_context(Context1)"))
        assertEquals(
            "the repaired envelope must still cross Android's bounded local-query policy",
            query,
            PrologQueryPolicy.requireSafe(query),
        )
        assertEquals(
            "one natural user turn must still execute exactly one canonical dialogue turn",
            1,
            Regex("symbolic_dialogue_turn:dialogue_turn\\(").findAll(query).count(),
        )
    }
}
