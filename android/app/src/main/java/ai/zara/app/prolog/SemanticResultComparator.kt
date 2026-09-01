package ai.zara.app.prolog

data class SemanticResult(
    val contractVersion: String,
    val terms: List<String>
)

object SemanticResultComparator {

    fun normalize(result: SemanticResult): SemanticResult =
        SemanticResult(
            contractVersion = result.contractVersion,
            terms = result.terms.map(::normalizeTerm).sorted()
        )

    fun equivalent(left: SemanticResult, right: SemanticResult): Boolean {
        if (left.contractVersion != right.contractVersion) {
            return false
        }
        return normalize(left) == normalize(right)
    }

    private fun normalizeTerm(term: String): String {
        val normalized = StringBuilder(term.length)
        var quote: Char? = null
        var escaped = false

        term.forEach { character ->
            if (escaped) {
                normalized.append(character)
                escaped = false
                return@forEach
            }

            if (quote != null) {
                normalized.append(character)
                when {
                    character == '\\' -> escaped = true
                    character == quote -> quote = null
                }
                return@forEach
            }

            when {
                character == '\'' || character == '"' -> {
                    quote = character
                    normalized.append(character)
                }
                character.isWhitespace() -> Unit
                else -> normalized.append(character)
            }
        }

        return normalized.toString()
    }
}
