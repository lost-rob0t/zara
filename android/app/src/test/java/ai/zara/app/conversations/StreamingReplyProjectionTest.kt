package ai.zara.app.conversations

import org.junit.Assert.*
import org.junit.Test

class StreamingReplyProjectionTest {
    private val running = ConversationRecord("chat", status = ConversationStatus.Running,
        turns = listOf(ConversationTurn("hi")), createdAtEpochMs = 0, updatedAtEpochMs = 0)
    private val state = ConversationState(listOf(running), "chat")

    @Test fun draftIsTransientAndDoesNotMarkTheTurnComplete() {
        val draft = state.withStreamingReply("chat", 1, "Hello")
        assertEquals("Hello", draft.conversation("chat")!!.turns.last().assistantText)
        assertNull(draft.conversation("chat")!!.turns.last().success)
        assertNull(state.conversation("chat")!!.turns.last().assistantText)
    }

    @Test fun completedReplacedAndUnknownTurnsRejectLateChunks() {
        assertSame(state, state.withStreamingReply("missing", 1, "late"))
        assertSame(state, state.withStreamingReply("chat", 2, "late"))
        val completed = state.copy(conversations = listOf(running.copy(status = ConversationStatus.Success,
            turns = listOf(ConversationTurn("hi", "done", true)))))
        assertSame(completed, completed.withStreamingReply("chat", 1, "late"))
    }

    @Test fun draftCannotExceedConversationTextLimit() {
        assertEquals(65_536, state.withStreamingReply("chat", 1, "x".repeat(70_000))
            .conversation("chat")!!.turns.last().assistantText!!.length)
    }
}
