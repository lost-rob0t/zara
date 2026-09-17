package ai.zara.org.core

sealed interface OrgResultApplyResult {
    data class Applied(val source: String, val nextRevision: Long) : OrgResultApplyResult
    data class Stale(val reason: String) : OrgResultApplyResult
    data class Rejected(val reason: String) : OrgResultApplyResult
}

object OrgNotebookResults {
    fun apply(
        source: String,
        currentRevision: Long,
        request: OrgExecutionRequest,
        result: OrgExecutionResult,
    ): OrgResultApplyResult {
        if (currentRevision != request.sourceRevision) {
            return OrgResultApplyResult.Stale("document revision changed")
        }
        if (result.requestId != request.requestId || result.sourceRevision != request.sourceRevision) {
            return OrgResultApplyResult.Stale("execution result does not match request")
        }
        if (result.blockHash != request.blockHash) {
            return OrgResultApplyResult.Stale("source block changed")
        }
        if (result.status != OrgExecutionStatus.SUCCEEDED) {
            return OrgResultApplyResult.Rejected("only successful executions may write #+RESULTS")
        }
        val block = OrgNotebookBlocks.scan(source, request.documentId)
            .firstOrNull { it.id == request.blockId && it.hash == request.blockHash }
            ?: return OrgResultApplyResult.Stale("source block no longer matches")

        val mode = request.headerArgs["results"]
            ?.lowercase()
            ?.split(Regex("\\s+"))
            ?.firstOrNull()
            ?: "replace"
        if (mode !in setOf("replace", "verbatim", "value")) {
            return OrgResultApplyResult.Rejected("unsupported :results mode $mode")
        }

        val payload = if (mode == "value") result.structuredValue ?: result.stdout else result.stdout
        val resultLines = formatResult(payload)
        val lines = source.lines().toMutableList()
        val insertionIndex = block.endLine.coerceIn(0, lines.size)
        val markerIndex = findExistingMarker(lines, insertionIndex)

        if (markerIndex == null) {
            lines.addAll(insertionIndex, listOf("#+RESULTS:") + resultLines)
        } else {
            val bodyEnd = existingResultEnd(lines, markerIndex + 1)
            repeat(bodyEnd - markerIndex) { lines.removeAt(markerIndex) }
            lines.addAll(markerIndex, listOf("#+RESULTS:") + resultLines)
        }
        return OrgResultApplyResult.Applied(lines.joinToString("\n"), currentRevision + 1)
    }

    private fun formatResult(payload: String): List<String> {
        if (payload.isEmpty()) return listOf(":")
        return payload.lines().map { line -> if (line.isEmpty()) ":" else ": $line" }
    }

    private fun findExistingMarker(lines: List<String>, insertionIndex: Int): Int? {
        var index = insertionIndex
        var blanks = 0
        while (index < lines.size && lines[index].isBlank() && blanks < 2) {
            blanks += 1
            index += 1
        }
        return index.takeIf {
            it < lines.size && lines[it].trim().startsWith("#+RESULTS:", ignoreCase = true)
        }
    }

    private fun existingResultEnd(lines: List<String>, bodyStart: Int): Int {
        var index = bodyStart
        while (index < lines.size) {
            val trimmed = lines[index].trim()
            if (trimmed.isBlank()) break
            if (trimmed.startsWith("*")) break
            if (trimmed.startsWith("#+") && !trimmed.startsWith("#+begin_", ignoreCase = true)) break
            if (!trimmed.startsWith(":") && !trimmed.startsWith("|")) break
            index += 1
        }
        return index
    }
}
