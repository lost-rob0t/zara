package ai.zara.ui.complication

enum class PrologComplicationType(val atom: String) {
    SHORT_TEXT("short_text"),
    LONG_TEXT("long_text"),
    RANGED_VALUE("ranged_value"),
    ;

    companion object {
        fun fromAtom(value: String): PrologComplicationType =
            entries.firstOrNull { it.atom == value }
                ?: throw IllegalArgumentException("Unsupported complication type: $value")
    }
}

data class PrologComplicationTemplate(
    val id: String,
    val type: PrologComplicationType,
    val text: String,
    val title: String? = null,
    val description: String = text,
    val rangeValue: Float? = null,
    val rangeMin: Float? = null,
    val rangeMax: Float? = null,
) {
    fun validated(): PrologComplicationTemplate {
        require(id.matches(ID)) { "Complication id must match ${ID.pattern}" }
        require(text.isNotBlank()) { "Complication text is required" }
        require(text.none(Char::isISOControl)) { "Complication text contains control characters" }
        require(description.isNotBlank()) { "Complication description is required" }
        require(description.none(Char::isISOControl)) { "Complication description contains control characters" }
        require(title == null || title.none(Char::isISOControl)) { "Complication title contains control characters" }
        require(title == null || title.length <= MAX_TITLE_CHARS) { "Complication title is too long" }
        require(description.length <= MAX_DESCRIPTION_CHARS) { "Complication description is too long" }
        val maxText = when (type) {
            PrologComplicationType.SHORT_TEXT -> MAX_SHORT_TEXT_CHARS
            PrologComplicationType.LONG_TEXT -> MAX_LONG_TEXT_CHARS
            PrologComplicationType.RANGED_VALUE -> MAX_RANGE_TEXT_CHARS
        }
        require(text.length <= maxText) { "Complication text is too long for ${type.atom}" }

        if (type == PrologComplicationType.RANGED_VALUE) {
            val value = requireNotNull(rangeValue) { "ranged_value requires complication_range/4" }
            val minimum = requireNotNull(rangeMin) { "ranged_value requires complication_range/4" }
            val maximum = requireNotNull(rangeMax) { "ranged_value requires complication_range/4" }
            require(value.isFinite() && minimum.isFinite() && maximum.isFinite()) {
                "Complication range values must be finite"
            }
            require(minimum < maximum) { "Complication range minimum must be below maximum" }
            require(value in minimum..maximum) { "Complication range value must be within its bounds" }
        } else {
            require(rangeValue == null && rangeMin == null && rangeMax == null) {
                "complication_range/4 is only valid for ranged_value templates"
            }
        }
        return this
    }

    companion object {
        private val ID = Regex("[a-z][a-z0-9_]{0,31}")
        const val MAX_SHORT_TEXT_CHARS = 40
        const val MAX_LONG_TEXT_CHARS = 160
        const val MAX_RANGE_TEXT_CHARS = 40
        const val MAX_TITLE_CHARS = 40
        const val MAX_DESCRIPTION_CHARS = 240
    }
}

object PrologComplicationTemplateCompiler {
    private val templateFact = Regex(
        "^complication_template\\(([a-z][a-z0-9_]{0,31}),\\s*(short_text|long_text|ranged_value)\\)\\.$"
    )
    private val stringFact = Regex(
        "^(complication_text|complication_title|complication_description)\\(([a-z][a-z0-9_]{0,31}),\\s*\"((?:\\\\.|[^\"\\\\])*)\"\\)\\.$"
    )
    private val rangeFact = Regex(
        "^complication_range\\(([a-z][a-z0-9_]{0,31}),\\s*([-+]?(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)),\\s*([-+]?(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+)),\\s*([-+]?(?:[0-9]+(?:\\.[0-9]+)?|\\.[0-9]+))\\)\\.$"
    )

    fun compile(source: String): PrologComplicationTemplate {
        require(source.encodeToByteArray().size <= MAX_SOURCE_BYTES) { "Complication template source is too large" }
        val facts = logicalLines(source)
        require(facts.isNotEmpty()) { "Complication template is empty" }

        var id: String? = null
        var type: PrologComplicationType? = null
        var text: String? = null
        var title: String? = null
        var description: String? = null
        var rangeValue: Float? = null
        var rangeMin: Float? = null
        var rangeMax: Float? = null

        facts.forEach { fact ->
            require(!containsRuleOrDirective(fact)) { "Complication templates are facts-only" }
            templateFact.matchEntire(fact)?.let { match ->
                require(id == null) { "Exactly one complication_template/2 fact is allowed" }
                id = match.groupValues[1]
                type = PrologComplicationType.fromAtom(match.groupValues[2])
                return@forEach
            }
            stringFact.matchEntire(fact)?.let { match ->
                val factId = match.groupValues[2]
                checkSameId(id, factId)
                val decoded = decodeString(match.groupValues[3])
                when (match.groupValues[1]) {
                    "complication_text" -> {
                        require(text == null) { "Duplicate complication_text/2 fact" }
                        text = decoded
                    }
                    "complication_title" -> {
                        require(title == null) { "Duplicate complication_title/2 fact" }
                        title = decoded
                    }
                    "complication_description" -> {
                        require(description == null) { "Duplicate complication_description/2 fact" }
                        description = decoded
                    }
                }
                return@forEach
            }
            rangeFact.matchEntire(fact)?.let { match ->
                val factId = match.groupValues[1]
                checkSameId(id, factId)
                require(rangeValue == null) { "Duplicate complication_range/4 fact" }
                rangeValue = match.groupValues[2].toFloatOrNull()
                    ?: throw IllegalArgumentException("Invalid complication range value")
                rangeMin = match.groupValues[3].toFloatOrNull()
                    ?: throw IllegalArgumentException("Invalid complication range minimum")
                rangeMax = match.groupValues[4].toFloatOrNull()
                    ?: throw IllegalArgumentException("Invalid complication range maximum")
                return@forEach
            }
            throw IllegalArgumentException("Unsupported complication template fact: $fact")
        }

        val resolvedId = requireNotNull(id) { "Missing complication_template/2 fact" }
        val resolvedType = requireNotNull(type) { "Missing complication_template/2 type" }
        val resolvedText = requireNotNull(text) { "Missing complication_text/2 fact" }
        return PrologComplicationTemplate(
            id = resolvedId,
            type = resolvedType,
            text = resolvedText,
            title = title,
            description = description ?: resolvedText,
            rangeValue = rangeValue,
            rangeMin = rangeMin,
            rangeMax = rangeMax,
        ).validated()
    }

    fun example(): String = """
        % Facts only. One template per source.
        complication_template(zara_status, short_text).
        complication_text(zara_status, "Ready").
        complication_title(zara_status, "Zara").
        complication_description(zara_status, "Assistant status").
    """.trimIndent() + "\n"

    fun llmPrompt(userRequest: String): String = """
        Design one Wear OS complication as a facts-only Prolog template.
        Return ONLY Prolog facts, no markdown fences and no explanation.
        The only allowed predicates are:
          complication_template(Id, short_text|long_text|ranged_value).
          complication_text(Id, "Text").
          complication_title(Id, "Optional title").
          complication_description(Id, "Accessibility description").
          complication_range(Id, Value, Min, Max).  % ranged_value only
        Use exactly one complication_template/2 and one complication_text/2.
        Id must match [a-z][a-z0-9_]{0,31}.
        short_text and ranged_value text <= ${PrologComplicationTemplate.MAX_SHORT_TEXT_CHARS} chars.
        long_text <= ${PrologComplicationTemplate.MAX_LONG_TEXT_CHARS} chars.
        title <= ${PrologComplicationTemplate.MAX_TITLE_CHARS} chars.
        Do not emit rules, directives, side effects, file paths, URLs, code execution, or unknown predicates.

        User request: ${userRequest.trim()}
    """.trimIndent()

    private fun logicalLines(source: String): List<String> {
        val result = mutableListOf<String>()
        source.lineSequence().forEachIndexed { index, rawLine ->
            val line = stripComment(rawLine).trim()
            if (line.isEmpty()) return@forEachIndexed
            require(line.endsWith('.')) { "Template fact on line ${index + 1} must end with a period" }
            result += line
        }
        return result
    }

    private fun stripComment(line: String): String {
        var quoted = false
        var escaped = false
        line.forEachIndexed { index, character ->
            if (quoted) {
                if (escaped) escaped = false
                else if (character == '\\') escaped = true
                else if (character == '"') quoted = false
            } else {
                if (character == '"') quoted = true
                else if (character == '%') return line.substring(0, index)
            }
        }
        return line
    }

    private fun containsRuleOrDirective(fact: String): Boolean {
        var quoted = false
        var escaped = false
        var index = 0
        while (index < fact.length - 1) {
            val character = fact[index]
            if (quoted) {
                if (escaped) escaped = false
                else if (character == '\\') escaped = true
                else if (character == '"') quoted = false
            } else {
                if (character == '"') quoted = true
                else if (character == ':' && fact[index + 1] == '-') return true
            }
            index += 1
        }
        return false
    }

    private fun checkSameId(currentId: String?, factId: String) {
        require(currentId != null) { "complication_template/2 must be the first fact" }
        require(currentId == factId) { "All complication facts must use template id $currentId" }
    }

    private fun decodeString(raw: String): String = buildString {
        var index = 0
        while (index < raw.length) {
            val character = raw[index]
            if (character != '\\') {
                append(character)
                index += 1
                continue
            }
            require(index + 1 < raw.length) { "Invalid trailing escape in complication string" }
            val escaped = raw[index + 1]
            append(
                when (escaped) {
                    '\\' -> '\\'
                    '"' -> '"'
                    'n' -> '\n'
                    't' -> '\t'
                    else -> throw IllegalArgumentException("Unsupported complication string escape: \\$escaped")
                }
            )
            index += 2
        }
    }

    private const val MAX_SOURCE_BYTES = 16 * 1024
}
