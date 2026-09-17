package ai.zara.app.ui.extensions

object PrologUiTermParser {
    fun parse(raw: String): UiContribution {
        val term = raw.trim().removeSuffix(".").trim()
        require(term.startsWith("ui(") && term.endsWith(")")) {
            "zara_ui/1 must return ui/7 terms"
        }
        val arguments = splitTopLevel(term.substring(3, term.length - 1))
        require(arguments.size == 7) { "zara_ui/1 must return ui/7 terms" }
        return UiContribution(
            id = parseText(arguments[0]),
            slot = UiSlot.fromWire(parseText(arguments[1])),
            kind = UiContributionKind.fromWire(parseText(arguments[2])),
            label = parseText(arguments[3]),
            action = parseText(arguments[4]),
            priority = arguments[5].trim().toIntOrNull()
                ?: throw IllegalArgumentException("zara_ui priority must be an integer"),
            platforms = parseList(arguments[6]).map(UiPlatform::fromWire).toSet(),
        )
    }

    private fun splitTopLevel(text: String): List<String> {
        val output = mutableListOf<String>()
        val current = StringBuilder()
        var quote: Char? = null
        var escaped = false
        var round = 0
        var square = 0
        text.forEach { character ->
            if (quote != null) {
                current.append(character)
                when {
                    escaped -> escaped = false
                    character == '\\' -> escaped = true
                    character == quote -> quote = null
                }
                return@forEach
            }
            when (character) {
                '\'', '"' -> {
                    quote = character
                    current.append(character)
                }
                '(' -> {
                    round += 1
                    current.append(character)
                }
                ')' -> {
                    round -= 1
                    require(round >= 0) { "unbalanced Prolog UI term" }
                    current.append(character)
                }
                '[' -> {
                    square += 1
                    current.append(character)
                }
                ']' -> {
                    square -= 1
                    require(square >= 0) { "unbalanced Prolog UI term" }
                    current.append(character)
                }
                ',' -> if (round == 0 && square == 0) {
                    output += current.toString().trim()
                    current.clear()
                } else {
                    current.append(character)
                }
                else -> current.append(character)
            }
        }
        require(quote == null && round == 0 && square == 0) { "unterminated Prolog UI term" }
        output += current.toString().trim()
        return output
    }

    private fun parseText(raw: String): String {
        val value = raw.trim()
        if (value.length >= 2 && value.first() in setOf('\'', '"') && value.last() == value.first()) {
            return unescape(value.substring(1, value.length - 1), value.first())
        }
        require(value.matches(Regex("[a-zA-Z0-9._:/?-]+"))) {
            "unsupported Prolog UI atom: $value"
        }
        return value
    }

    private fun parseList(raw: String): List<String> {
        val value = raw.trim()
        require(value.startsWith("[") && value.endsWith("]")) {
            "zara_ui platforms must be a list"
        }
        val body = value.substring(1, value.length - 1).trim()
        if (body.isEmpty()) return emptyList()
        return splitTopLevel(body).map(::parseText)
    }

    private fun unescape(body: String, quote: Char): String {
        val output = StringBuilder()
        var escaped = false
        body.forEach { character ->
            if (!escaped && character == '\\') {
                escaped = true
                return@forEach
            }
            if (!escaped) {
                output.append(character)
                return@forEach
            }
            output.append(
                when (character) {
                    'n' -> '\n'
                    'r' -> '\r'
                    't' -> '\t'
                    '\\' -> '\\'
                    quote -> quote
                    else -> character
                },
            )
            escaped = false
        }
        require(!escaped) { "unterminated Prolog UI escape" }
        return output.toString()
    }
}
