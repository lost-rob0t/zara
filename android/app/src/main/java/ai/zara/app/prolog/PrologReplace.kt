package ai.zara.app.prolog

data class PrologReplaceResult(
    val text: String,
    val cursor: Int,
    val replacements: Int,
)

object PrologReplace {
    fun replaceNext(text: String, query: String, replacement: String, cursor: Int): PrologReplaceResult {
        require(query.isNotEmpty()) { "Replace query is required" }
        val safeCursor = cursor.coerceIn(0, text.length)
        val afterCursor = text.indexOf(query, safeCursor, ignoreCase = true)
        val found = if (afterCursor >= 0) afterCursor else text.indexOf(query, 0, ignoreCase = true)
        if (found < 0) return PrologReplaceResult(text, safeCursor, 0)
        val updated = text.replaceRange(found, found + query.length, replacement)
        return PrologReplaceResult(updated, found + replacement.length, 1)
    }

    fun replaceAll(text: String, query: String, replacement: String, limit: Int = 500): PrologReplaceResult {
        require(query.isNotEmpty()) { "Replace query is required" }
        require(limit in 1..500) { "Replace limit is invalid" }
        val output = StringBuilder(text.length)
        var offset = 0
        var replacements = 0
        while (replacements < limit) {
            val found = text.indexOf(query, offset, ignoreCase = true)
            if (found < 0) break
            output.append(text, offset, found)
            output.append(replacement)
            offset = found + query.length
            replacements += 1
        }
        output.append(text, offset, text.length)
        return PrologReplaceResult(output.toString(), output.length, replacements)
    }
}
