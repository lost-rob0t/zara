package ai.zara.org.core

import java.util.Base64

data class TranscriptSpan(val id: String, val startMs: Long, val endMs: Long, val text: String)

data class TranscriptCapture(
    val id: String,
    val capturedAtEpochMs: Long,
    val spans: List<TranscriptSpan>,
)

enum class TranscriptExtractKind { NOTE, TODO }

data class TranscriptExtract(
    val kind: TranscriptExtractKind,
    val spanId: String,
    val startChar: Int,
    val endChar: Int,
    val quote: String,
)

object OrgTranscriptCapture {
    private val identifier = Regex("[A-Za-z0-9][A-Za-z0-9_-]{0,63}")

    fun relativePath(captureId: String): String {
        require(identifier.matches(captureId)) { "Invalid capture ID" }
        return "memory/captures/$captureId.org"
    }

    fun render(capture: TranscriptCapture, extracts: List<TranscriptExtract>): String {
        relativePath(capture.id)
        require(capture.capturedAtEpochMs >= 0) { "Invalid capture time" }
        require(capture.spans.size in 1..256) { "Capture needs 1 to 256 spans" }
        require(extracts.size <= 64) { "Capture exceeds 64 extracts" }
        require(extracts.distinct().size == extracts.size) { "Duplicate extract" }
        require(capture.spans.map { it.id }.distinct().size == capture.spans.size) { "Duplicate span ID" }
        var previousEnd = 0L
        var totalChars = 0
        capture.spans.forEach { span ->
            require(identifier.matches(span.id)) { "Invalid span ID" }
            require(span.startMs >= previousEnd && span.endMs > span.startMs && span.endMs <= 1_800_000) {
                "Spans must be ordered, nonoverlapping and within 30 minutes"
            }
            require(span.text.isNotBlank() && span.text.length <= 4096) { "Invalid span text length" }
            require(wellFormed(span.text)) { "Invalid Unicode in transcript" }
            require(span.text.none { it.isISOControl() && it != '\n' && it != '\r' && it != '\t' }) {
                "Control characters are not allowed"
            }
            totalChars += span.text.length
            require(totalChars <= 65_536) { "Capture exceeds text budget" }
            previousEnd = span.endMs
        }
        val spans = capture.spans.associateBy { it.id }
        extracts.forEach { extract ->
            val span = requireNotNull(spans[extract.spanId]) { "Unknown evidence span" }
            require(extract.startChar >= 0 && extract.endChar > extract.startChar && extract.endChar <= span.text.length) {
                "Invalid evidence character range"
            }
            require(extract.quote.isNotBlank() && extract.quote.length <= 300) { "Invalid extract length" }
            require(wellFormed(extract.quote)) { "Evidence range splits a Unicode character" }
            require(span.text.substring(extract.startChar, extract.endChar) == extract.quote) {
                "Extract must quote its evidence exactly"
            }
        }
        return buildString {
            append("#+title: Capture ").append(capture.id).append('\n')
            append("* Capture ").append(capture.id).append(" :capture:\n")
            properties("capture:${capture.id}", capture)
            capture.spans.forEach { span ->
                append("** Transcript — ").append(literal(span.text)).append('\n')
                properties("capture:${capture.id}:span:${span.id}", capture, span,
                    listOf("TEXT_UTF8_BASE64" to encoded(span.text)))
                append(": ").append(literal(span.text)).append('\n')
            }
            extracts.sortedWith(compareBy<TranscriptExtract> { it.spanId }
                .thenBy { it.startChar }.thenBy { it.endChar }.thenBy { it.kind.name })
                .forEach { extract ->
                    val span = spans.getValue(extract.spanId)
                    append("** ")
                    append(if (extract.kind == TranscriptExtractKind.TODO) "TODO " else "Note — ")
                    append(literal(extract.quote)).append('\n')
                    properties(
                        "capture:${capture.id}:extract:${span.id}:${extract.startChar}:${extract.endChar}:${extract.kind.name.lowercase()}",
                        capture, span,
                        listOf(
                            "EXTRACT_KIND" to extract.kind.name.lowercase(),
                            "CHAR_START" to extract.startChar.toString(),
                            "CHAR_END" to extract.endChar.toString(),
                            "CHAR_UNIT" to "utf16-code-unit",
                            "QUOTE_UTF8_BASE64" to encoded(extract.quote),
                            "REVIEW_STATE" to "proposed",
                        ),
                    )
                    append(": Evidence: ").append(literal(extract.quote)).append('\n')
                }
        }
    }

    private fun StringBuilder.properties(
        id: String,
        capture: TranscriptCapture,
        span: TranscriptSpan? = null,
        extra: List<Pair<String, String>> = emptyList(),
    ) {
        append(":PROPERTIES:\n:ID: ").append(id).append('\n')
        append(":CAPTURE_ID: ").append(capture.id).append('\n')
        append(":CAPTURED_AT_EPOCH_MS: ").append(capture.capturedAtEpochMs).append('\n')
        append(":EVIDENCE_CLASS: recorded-speech\n")
        if (span != null) {
            append(":SPAN_ID: ").append(span.id).append('\n')
            append(":SPAN_START_MS: ").append(span.startMs).append('\n')
            append(":SPAN_END_MS: ").append(span.endMs).append('\n')
        }
        extra.forEach { (key, value) -> append(':').append(key).append(": ").append(value).append('\n') }
        append(":END:\n")
    }

    private fun literal(text: String): String = text
        .replace('\r', ' ').replace('\n', ' ').replace('\t', ' ')
        .replace('[', '［').replace(']', '］')
        .replace('<', '＜').replace('>', '＞')
        .replace(':', '：')

    private fun encoded(text: String): String = Base64.getEncoder().encodeToString(text.toByteArray(Charsets.UTF_8))

    private fun wellFormed(text: String): Boolean {
        var index = 0
        while (index < text.length) {
            val char = text[index]
            if (char.isLowSurrogate()) return false
            if (char.isHighSurrogate()) {
                if (index + 1 == text.length || !text[index + 1].isLowSurrogate()) return false
                index += 1
            }
            index += 1
        }
        return true
    }
}
