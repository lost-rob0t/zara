package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidPinnedTreallaContextInputContractTest {
    @Test
    fun persistedDialogueContextDoesNotFeedAStringDirectlyToPinnedReadTermFromAtom() {
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
        assertTrue(
            "the canonical persisted-context parser must remain on read_term_from_atom/3 rather than bypassing the shape fence",
            query.contains("read_term_from_atom("),
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
