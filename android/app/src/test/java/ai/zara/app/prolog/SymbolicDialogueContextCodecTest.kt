package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class SymbolicDialogueContextCodecTest {
    @Test
    fun emptyContextHasCanonicalVersionedWireForm() {
        val encoded = SymbolicDialogueContextCodec.encode("[]")

        assertEquals(
            "{\"version\":\"ZARA-SYMBOLIC-DIALOGUE-CONTEXT/1\",\"term\":\"[]\"}",
            encoded,
        )
        assertEquals("[]", SymbolicDialogueContextCodec.decode(encoded))
        assertEquals("[]", SymbolicDialogueContextCodec.decode("{}"))
    }

    @Test
    fun partialAndCompletedContextsRoundTripWithoutLoss() {
        val contexts = listOf(
            "partial_frame(frame(intent(ns(device),name('timer.set')),[],missing([duration])),[duration])",
            "partial_frame(frame(intent(ns(app),name(open)),[],ambiguous([firefox,chromium,emacs])),[firefox,chromium,emacs])",
            "completed_frame(frame(intent(ns(device),name('timer.set')),[slot(name(duration),value(duration(300)),origin(follow_up))],complete))",
        )

        contexts.forEach { context ->
            assertEquals(context, SymbolicDialogueContextCodec.decode(SymbolicDialogueContextCodec.encode(context)))
        }
    }

    @Test
    fun jsonEscapesCannotChangeTheContextEnvelope() {
        val context = "completed_frame(frame(intent(ns(app),name(open)),[slot(name(target),value(ref(kind(app_alias),id('quote\\\\slash'))),origin(utterance))],complete))"
        val encoded = SymbolicDialogueContextCodec.encode(context)

        assertTrue(encoded.contains("\\\\"))
        assertEquals(context, SymbolicDialogueContextCodec.decode(encoded))
    }

    @Test
    fun malformedOrFutureWireFailsClosed() {
        listOf(
            "",
            "[]",
            "{\"version\":\"ZARA-SYMBOLIC-DIALOGUE-CONTEXT/2\",\"term\":\"[]\"}",
            "{\"term\":\"[]\",\"version\":\"ZARA-SYMBOLIC-DIALOGUE-CONTEXT/1\"}",
            "{\"version\":\"ZARA-SYMBOLIC-DIALOGUE-CONTEXT/1\",\"term\":\"future_context(foo)\"}",
            "{\"version\":\"ZARA-SYMBOLIC-DIALOGUE-CONTEXT/1\",\"term\":\"[]\"} trailing",
        ).forEach { malformed ->
            assertThrows(IllegalArgumentException::class.java) {
                SymbolicDialogueContextCodec.decode(malformed)
            }
        }
    }

    @Test
    fun oversizedContextFailsBeforeItCanReachProlog() {
        val oversized = "partial_frame(" + "x".repeat(2_048) + ")"

        assertThrows(IllegalArgumentException::class.java) {
            SymbolicDialogueContextCodec.encode(oversized)
        }
    }
}
