package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Test

class AndroidPureSymbolicConversationFactoryTest {
    @Test
    fun dialogueTurnQueryUsesCanonicalDialogueTurnAndRenderer() {
        val query = AndroidPureSymbolicConversationFactory.dialogueTurnQuery(
            "  set a timer for \"five\"\\minutes\nplease  ",
        )

        assertEquals(
            "symbolic_dialogue_turn:dialogue_turn(\"set a timer for \\\"five\\\"\\\\minutes\\nplease\", conversation, [], turn(_Frames, Act, _Context)), symbolic_dialogue:render_response(Act, Result)",
            query,
        )
        assertFalse(query.contains("resolve_frames("))
    }

    @Test
    fun dialogueTurnQueryDoesNotResetOrDiscardConversationContext() {
        val query = AndroidPureSymbolicConversationFactory.dialogueTurnQuery("five minutes")

        assertFalse(
            "natural turns must load canonical persisted Context0 instead of resetting dialogue state",
            query.contains("conversation, []"),
        )
        assertFalse(
            "natural turns must expose Context1 for generation-fenced persistence instead of discarding it",
            query.contains("_Context"),
        )
    }

    @Test
    fun dialogueTurnQueryRejectsBlankInput() {
        assertThrows(IllegalArgumentException::class.java) {
            AndroidPureSymbolicConversationFactory.dialogueTurnQuery("   ")
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
