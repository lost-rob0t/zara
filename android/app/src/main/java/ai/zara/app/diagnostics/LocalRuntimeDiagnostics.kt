package ai.zara.app.diagnostics

import java.io.File
import java.time.Instant

class LocalRuntimeDiagnostics(
    private val file: File,
) {
    @Synchronized
    fun record(
        event: String,
        fields: Map<String, Any?> = emptyMap(),
        error: Throwable? = null,
    ) {
        runCatching {
            val parent = checkNotNull(file.parentFile) { "Diagnostics directory is unavailable" }
            check(parent.mkdirs() || parent.isDirectory) { "Diagnostics directory is unavailable" }

            val line = buildString {
                append(Instant.now().toString())
                append(" event=")
                append(sanitize(event, 96))
                fields.toSortedMap().forEach { (key, value) ->
                    append(' ')
                    append(sanitize(key, 64))
                    append('=')
                    append(sanitize(value?.toString() ?: "null", 512))
                }
                if (error != null) {
                    append(" error=")
                    append(sanitize(errorChain(error), 2_048))
                    append(" stack=")
                    append(sanitize(stackSummary(error), 4_096))
                }
                append('\n')
            }

            file.appendText(line)
            if (file.length() > MAX_FILE_BYTES) {
                val text = file.readText()
                val marker = "${Instant.now()} event=diagnostics.rotated\n"
                file.writeText(marker + text.takeLast(MAX_FILE_CHARS / 2))
            }
        }
    }

    @Synchronized
    fun export(header: Map<String, Any?>): String {
        val body = runCatching { if (file.isFile) file.readText() else "" }.getOrDefault("")
        val boundedBody = body.takeLast(MAX_EXPORT_CHARS)
        return buildString {
            append("ZARA-LOCAL-DIAGNOSTICS/1\n")
            header.toSortedMap().forEach { (key, value) ->
                append(sanitize(key, 64))
                append('=')
                append(sanitize(value?.toString() ?: "null", 1_024))
                append('\n')
            }
            append("--- events ---\n")
            if (body.isBlank()) {
                append("(no recorded events)\n")
            } else {
                append(boundedBody)
                if (!endsWith("\n")) append('\n')
            }
        }
    }

    @Synchronized
    fun clear() {
        runCatching {
            if (file.exists()) check(file.delete()) { "Diagnostics log could not be cleared" }
        }
    }

    private fun errorChain(error: Throwable): String {
        val parts = mutableListOf<String>()
        var current: Throwable? = error
        var depth = 0
        while (current != null && depth < MAX_CAUSE_DEPTH) {
            val name = current::class.java.name
            val message = current.message?.takeIf(String::isNotBlank)
            parts += if (message == null) name else "$name:${sanitize(message, 768)}"
            current = current.cause
            depth += 1
        }
        return parts.joinToString(" <- ")
    }

    private fun stackSummary(error: Throwable): String =
        error.stackTrace
            .take(MAX_STACK_FRAMES)
            .joinToString(" <- ") { frame ->
                "${frame.className}.${frame.methodName}:${frame.lineNumber}"
            }

    private fun sanitize(raw: String, max: Int): String {
        var value = raw
            .replace("\r", "\\r")
            .replace("\n", "\\n")
            .replace("\t", "\\t")
            .filter { it.code >= 0x20 && it.code != 0x7f }

        value = SECRET_ASSIGNMENT.replace(value) { match ->
            "${match.groupValues[1]}=<redacted>"
        }
        if (value.length > max) value = value.take(max) + "…"
        return value.ifBlank { "-" }
    }

    companion object {
        private const val MAX_FILE_BYTES = 256L * 1024L
        private const val MAX_FILE_CHARS = 256 * 1024
        private const val MAX_EXPORT_CHARS = 128 * 1024
        private const val MAX_CAUSE_DEPTH = 4
        private const val MAX_STACK_FRAMES = 16

        private val SECRET_ASSIGNMENT = Regex(
            "(?i)(token|secret|password|authorization|api[-_]?key|private[-_ ]?key)\\s*[:=]\\s*[^\\s,;]+",
        )
    }
}
