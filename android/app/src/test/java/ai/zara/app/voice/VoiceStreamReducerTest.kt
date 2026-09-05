package ai.zara.app.voice

import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class VoiceStreamReducerTest {
    @Test fun `transcript updates require current session and monotonic stream sequence`() {
        var state = VoiceStreamState.connected("session-1")
        state = reduceVoiceStream(
            state,
            VoiceStreamEvent.Transcript("session-1", "conversation-1", "mic-1", 3, "hello wor", false),
        )
        state = reduceVoiceStream(
            state,
            VoiceStreamEvent.Transcript("session-1", "conversation-1", "mic-1", 4, "hello world", true),
        )

        assertEquals("hello world", state.transcriptText)
        assertEquals(true, state.transcriptFinal)
        assertEquals("conversation-1", state.conversationId)
        assertEquals(4L, state.lastTranscriptSequence)

        assertThrows(StaleVoiceStreamException::class.java) {
            reduceVoiceStream(
                state,
                VoiceStreamEvent.Transcript("session-1", "conversation-1", "mic-1", 4, "duplicate", false),
            )
        }
        assertThrows(StaleVoiceStreamException::class.java) {
            reduceVoiceStream(
                state,
                VoiceStreamEvent.Transcript("old-session", "conversation-1", "mic-1", 5, "stale", false),
            )
        }
    }

    @Test fun `audio lifecycle requires one current turn stream and increasing chunks`() {
        var state = VoiceStreamState.connected("session-1")
        state = reduceVoiceStream(
            state,
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1),
        )
        val first = byteArrayOf(1, 0, 2, 0)
        state = reduceVoiceStream(
            state,
            VoiceStreamEvent.AudioChunk("session-1", "turn-1", "speaker-1", 9, first),
        )
        assertEquals(9L, state.lastAudioSequence)
        assertArrayEquals(first, state.lastAudioChunk)

        assertThrows(StaleVoiceStreamException::class.java) {
            reduceVoiceStream(
                state,
                VoiceStreamEvent.AudioChunk("session-1", "turn-1", "speaker-1", 8, byteArrayOf(3, 0)),
            )
        }
        assertThrows(StaleVoiceStreamException::class.java) {
            reduceVoiceStream(
                state,
                VoiceStreamEvent.AudioChunk("session-1", "old-turn", "speaker-1", 10, byteArrayOf(3, 0)),
            )
        }

        state = reduceVoiceStream(
            state,
            VoiceStreamEvent.AudioDone("session-1", "turn-1", "speaker-1"),
        )
        assertEquals(null, state.audio)
        assertThrows(StaleVoiceStreamException::class.java) {
            reduceVoiceStream(
                state,
                VoiceStreamEvent.AudioChunk("session-1", "turn-1", "speaker-1", 10, byteArrayOf(3, 0)),
            )
        }
    }

    @Test fun `new output start supersedes prior output without accepting old chunks`() {
        var state = VoiceStreamState.connected("session-1")
        state = reduceVoiceStream(
            state,
            VoiceStreamEvent.AudioStarted("session-1", "turn-1", "speaker-1", 24_000, 1),
        )
        state = reduceVoiceStream(
            state,
            VoiceStreamEvent.AudioStarted("session-1", "turn-2", "speaker-2", 24_000, 1),
        )

        assertEquals("turn-2", state.audio?.turnId)
        assertThrows(StaleVoiceStreamException::class.java) {
            reduceVoiceStream(
                state,
                VoiceStreamEvent.AudioChunk("session-1", "turn-1", "speaker-1", 1, byteArrayOf(1, 0)),
            )
        }
    }
}
