package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFails
import org.junit.Test

class PrologReplaceTest {
    @Test
    fun replaceNextStartsAtCursorAndWrapsOnce() {
        val source = "fact(one).\nfact(two).\nFACT(three).\n"

        val fromMiddle = PrologReplace.replaceNext(source, "fact", "signal", cursor = 6)
        assertEquals(1, fromMiddle.replacements)
        assertEquals("fact(one).\nsignal(two).\nFACT(three).\n", fromMiddle.text)
        assertEquals(fromMiddle.text.indexOf("signal") + "signal".length, fromMiddle.cursor)

        val wrapped = PrologReplace.replaceNext(source, "fact", "signal", cursor = source.length)
        assertEquals(1, wrapped.replacements)
        assertEquals("signal(one).\nfact(two).\nFACT(three).\n", wrapped.text)
    }

    @Test
    fun replaceAllIsCaseInsensitiveAndBounded() {
        val source = buildString {
            repeat(501) { index ->
                append(if (index % 2 == 0) "fact" else "FACT")
                append("($index).\n")
            }
        }

        val result = PrologReplace.replaceAll(source, "fact", "signal")
        assertEquals(500, result.replacements)
        assertEquals(1, Regex("(?i)fact\\(").findAll(result.text).count())
        assertEquals(result.text.length, result.cursor)
    }

    @Test
    fun noMatchPreservesTextAndClampsCursor() {
        val source = "fact(one).\n"
        val result = PrologReplace.replaceNext(source, "missing", "signal", cursor = 9_999)

        assertEquals(0, result.replacements)
        assertEquals(source, result.text)
        assertEquals(source.length, result.cursor)
    }

    @Test
    fun invalidInputsFailClosedBeforeMutation() {
        assertFails { PrologReplace.replaceNext("fact(one).", "", "signal", 0) }
        assertFails { PrologReplace.replaceAll("fact(one).", "", "signal") }
        assertFails { PrologReplace.replaceAll("fact(one).", "fact", "signal", limit = 0) }
        assertFails { PrologReplace.replaceAll("fact(one).", "fact", "signal", limit = 501) }
    }
}
