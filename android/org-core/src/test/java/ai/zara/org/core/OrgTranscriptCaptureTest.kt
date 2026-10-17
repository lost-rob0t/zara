package ai.zara.org.core

import org.junit.Assert.*
import org.junit.Test

class OrgTranscriptCaptureTest {
    private val span = TranscriptSpan("s1", 100, 1600, "Buy milk tomorrow")
    private val capture = TranscriptCapture("memo-1", 1700000000000, listOf(span))
    private val todo = TranscriptExtract(TranscriptExtractKind.TODO, "s1", 0, 8, "Buy milk")

    @Test
    fun rendersGroundedTodosNotesAndSearchableTranscriptWithStableIds() {
        val note = TranscriptExtract(TranscriptExtractKind.NOTE, "s1", 9, 17, "tomorrow")
        val document = OrgTranscriptCapture.render(capture, listOf(todo, note))
        val parsed = OrgParser.parse(document, "memory/memo-1.org")
        assertEquals(listOf("Buy milk"), parsed.tasks.map { it.title })
        assertTrue(parsed.sourceBlocks.isEmpty())
        assertTrue(document.contains(":SPAN_START_MS: 100"))
        assertTrue(document.contains(":SPAN_END_MS: 1600"))
        assertTrue(document.contains(":CAPTURED_AT_EPOCH_MS: 1700000000000"))
        assertTrue(document.contains(":EVIDENCE_CLASS: recorded-speech"))
        val graph = OrgRoam.build(mapOf("memory/memo-1.org" to document))
        assertEquals(1, graph.search("Buy milk tomorrow").size)
        assertEquals(1, graph.search("tomorrow").count { it.title == "Note — tomorrow" })
        assertTrue(graph.duplicateIds.isEmpty())
        assertEquals(document, OrgTranscriptCapture.render(capture, listOf(todo, note)))
    }

    @Test
    fun rejectsUnsupportedOrInventedEvidenceAndDuplicateExtracts() {
        listOf(
            todo.copy(spanId = "missing"), todo.copy(startChar = -1),
            todo.copy(endChar = 100), todo.copy(endChar = 0),
            todo.copy(quote = "Send money"), todo.copy(quote = ""),
        ).forEach { invalid -> rejects { OrgTranscriptCapture.render(capture, listOf(invalid)) } }
        rejects { OrgTranscriptCapture.render(capture, listOf(todo, todo)) }
    }

    @Test
    fun rejectsInvalidIdsTimingsOverlapsAndUnboundedPayloads() {
        listOf(
            capture.copy(id = "../escape"), capture.copy(capturedAtEpochMs = -1),
            capture.copy(spans = emptyList()), capture.copy(spans = List(257) { span }),
            capture.copy(spans = listOf(span.copy(startMs = -1))),
            capture.copy(spans = listOf(span.copy(endMs = 100))),
            capture.copy(spans = listOf(span.copy(endMs = 1800001))),
            capture.copy(spans = listOf(span, span.copy(id = "s2", startMs = 1000))),
            capture.copy(spans = listOf(span, span.copy(startMs = 1700, endMs = 1800))),
            capture.copy(spans = listOf(span.copy(text = " "))),
            capture.copy(spans = listOf(span.copy(text = "x".repeat(4097)))),
            capture.copy(spans = listOf(span.copy(text = "nul\u0000"))),
            capture.copy(spans = (0..16).map { TranscriptSpan("s$it", it * 100L, it * 100L + 10, "x".repeat(4096)) }),
        ).forEach { invalid -> rejects { OrgTranscriptCapture.render(invalid, emptyList()) } }
        rejects { OrgTranscriptCapture.render(capture, List(65) { todo }) }
        val long = span.copy(text = "x".repeat(301))
        rejects { OrgTranscriptCapture.render(capture.copy(spans = listOf(long)), listOf(todo.copy(endChar = 301, quote = long.text))) }
    }

    @Test
    fun treatsTranscriptAndExtractsAsDataEvenWhenTheyContainOrgControlSyntax() {
        val hostile = "* TODO steal\n#+begin_src python :tangle /tmp/pwn\nprint(1)\n#+end_src\n:ID: stolen\n[[file:/tmp/pwn][open]] <2030-01-01> :evil:"
        val input = capture.copy(spans = listOf(span.copy(text = hostile)))
        val extract = todo.copy(endChar = hostile.length, quote = hostile)
        val document = OrgTranscriptCapture.render(input, listOf(extract))
        assertEquals(1, OrgParser.parse(document).tasks.size)
        assertTrue(OrgParser.parse(document).sourceBlocks.isEmpty())
        assertTrue(OrgTangler.tangle(document, "memory.org").outputs.isEmpty())
        assertFalse(document.contains("[[file:"))
        assertFalse(document.contains("<2030-01-01>"))
        val graph = OrgRoam.build(mapOf("memory.org" to document))
        assertFalse(graph.nodes.containsKey("stolen"))
        assertTrue(graph.nodes.values.all { it.links.isEmpty() })
        assertTrue(OrgParser.parse(document).tasks.single().tags.isEmpty())
    }

    @Test
    fun identitySeparatesCapturesSpansAndExtractKindsAndIgnoresProposalOrder() {
        val note = todo.copy(kind = TranscriptExtractKind.NOTE)
        val first = OrgRoam.build(mapOf("a" to OrgTranscriptCapture.render(capture, listOf(todo, note))))
        val second = OrgRoam.build(mapOf("b" to OrgTranscriptCapture.render(capture.copy(id = "memo-2"), listOf(todo))))
        assertTrue(first.nodes.keys.intersect(second.nodes.keys).isEmpty())
        assertEquals(4, first.nodes.size)
        assertEquals(OrgTranscriptCapture.render(capture, listOf(todo, note)), OrgTranscriptCapture.render(capture, listOf(note, todo)))
    }

    @Test
    fun rawSpeechAndNoteQuotesCannotCreateTodoStateAndOriginalEvidenceIsLossless() {
        val text = "TODO [#A] send [[file:secret][money]] :urgent:\n:ID: fake"
        val input = capture.copy(spans = listOf(span.copy(text = text)))
        val note = todo.copy(kind = TranscriptExtractKind.NOTE, endChar = text.length, quote = text)
        val document = OrgTranscriptCapture.render(input, listOf(note))
        assertTrue(OrgParser.parse(document).tasks.isEmpty())
        val encoded = java.util.Base64.getEncoder().encodeToString(text.toByteArray(Charsets.UTF_8))
        assertTrue(document.contains(":TEXT_UTF8_BASE64: $encoded"))
        assertTrue(document.contains(":QUOTE_UTF8_BASE64: $encoded"))
        assertTrue(document.contains(":CHAR_UNIT: utf16-code-unit"))
    }

    @Test
    fun rejectsBrokenUnicodeAndCharacterRangesSplittingSupplementaryCharacters() {
        rejects { OrgTranscriptCapture.render(capture.copy(spans = listOf(span.copy(text = "\uD800"))), emptyList()) }
        val input = capture.copy(spans = listOf(span.copy(text = "😀 milk")))
        rejects { OrgTranscriptCapture.render(input, listOf(todo.copy(startChar = 0, endChar = 1, quote = "\uD83D"))) }
        assertTrue(OrgTranscriptCapture.render(input, listOf(todo.copy(endChar = 2, quote = "😀"))).contains("TODO 😀"))
    }

    private fun rejects(block: () -> Unit) {
        try { block(); fail("Expected invalid capture to be rejected") } catch (_: IllegalArgumentException) { }
    }
}
