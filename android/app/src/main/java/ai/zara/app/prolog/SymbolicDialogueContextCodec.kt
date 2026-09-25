package ai.zara.app.prolog

/**
 * Language-neutral durable envelope for ZARA-SYMBOLIC-DIALOGUE context terms.
 *
 * The canonical conversation projection owns the JSON. This codec owns no
 * history or runtime state; it only converts the trusted Prolog continuation
 * term to/from a bounded, versioned JSON object. The decoded term must still
 * cross symbolic_dialogue_turn:valid_dialogue_context/1 before execution.
 */
internal object SymbolicDialogueContextCodec {
    const val version = "ZARA-SYMBOLIC-DIALOGUE-CONTEXT/1"
    const val emptyContextTerm = "[]"
    private const val maxContextTermChars = 2_048

    fun encode(contextTerm: String): String {
        val term = requireContextTerm(contextTerm)
        return buildString(term.length + version.length + 32) {
            append('{')
            append("\"version\":")
            appendJsonString(version)
            append(',')
            append("\"term\":")
            appendJsonString(term)
            append('}')
        }
    }

    fun decode(dialogueStateJson: String): String {
        val source = dialogueStateJson.trim()
        if (source == "{}") return emptyContextTerm
        val parser = WireParser(source)
        parser.expect('{')
        parser.skipWhitespace()
        require(parser.readString() == "version") { "dialogue context requires version" }
        parser.skipWhitespace()
        parser.expect(':')
        parser.skipWhitespace()
        val wireVersion = parser.readString()
        require(wireVersion == version) { "unsupported dialogue context version" }
        parser.skipWhitespace()
        parser.expect(',')
        parser.skipWhitespace()
        require(parser.readString() == "term") { "dialogue context requires term" }
        parser.skipWhitespace()
        parser.expect(':')
        parser.skipWhitespace()
        val term = parser.readString()
        parser.skipWhitespace()
        parser.expect('}')
        parser.skipWhitespace()
        require(parser.atEnd()) { "trailing dialogue context JSON" }
        return requireContextTerm(term)
    }

    fun requireContextTerm(raw: String): String {
        val term = raw.trim()
        require(term.isNotEmpty()) { "dialogue context term is required" }
        require(term.length <= maxContextTermChars) { "dialogue context term is too large" }
        require(term.none(Char::isISOControl)) { "dialogue context term contains control characters" }
        require(
            term == emptyContextTerm ||
                term.startsWith("partial_frame(") ||
                term.startsWith("completed_frame(")
        ) { "unsupported dialogue context term shape" }
        return term
    }

    fun prologString(raw: String): String = buildString(raw.length + 8) {
        raw.forEach { character ->
            when (character) {
                '\\' -> append("\\\\")
                '"' -> append("\\\"")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> append(character)
            }
        }
    }

    private fun StringBuilder.appendJsonString(value: String) {
        append('"')
        value.forEach { character ->
            when (character) {
                '"' -> append("\\\"")
                '\\' -> append("\\\\")
                '\b' -> append("\\b")
                '\u000C' -> append("\\f")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> {
                    if (character.code < 0x20) {
                        append("\\u")
                        append(character.code.toString(16).padStart(4, '0'))
                    } else {
                        append(character)
                    }
                }
            }
        }
        append('"')
    }

    private class WireParser(private val source: String) {
        private var index = 0

        fun atEnd(): Boolean = index == source.length

        fun skipWhitespace() {
            while (index < source.length && source[index] in charArrayOf(' ', '\t', '\n', '\r')) {
                index += 1
            }
        }

        fun expect(expected: Char) {
            require(index < source.length && source[index] == expected) {
                "expected '$expected' in dialogue context JSON"
            }
            index += 1
        }

        fun readString(): String {
            expect('"')
            val output = StringBuilder()
            while (index < source.length) {
                val character = source[index++]
                when {
                    character == '"' -> return output.toString()
                    character == '\\' -> output.append(readEscape())
                    character.code < 0x20 -> throw IllegalArgumentException(
                        "control character in dialogue context JSON"
                    )
                    else -> output.append(character)
                }
            }
            throw IllegalArgumentException("unterminated dialogue context JSON string")
        }

        private fun readEscape(): Char {
            require(index < source.length) { "unterminated dialogue context JSON escape" }
            return when (val escaped = source[index++]) {
                '"' -> '"'
                '\\' -> '\\'
                '/' -> '/'
                'b' -> '\b'
                'f' -> '\u000C'
                'n' -> '\n'
                'r' -> '\r'
                't' -> '\t'
                'u' -> readUnicodeEscape()
                else -> throw IllegalArgumentException(
                    "invalid dialogue context JSON escape: $escaped"
                )
            }
        }

        private fun readUnicodeEscape(): Char {
            require(index + 4 <= source.length) { "truncated dialogue context unicode escape" }
            val hex = source.substring(index, index + 4)
            require(hex.all { it.isDigit() || it.lowercaseChar() in 'a'..'f' }) {
                "invalid dialogue context unicode escape"
            }
            index += 4
            return hex.toInt(16).toChar()
        }
    }
}
