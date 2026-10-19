package ai.zara.app.diagnostics

import java.time.Instant

object ChatErrorDetails {
    const val MAX_BYTES = 8 * 1024
    private val assignments = Regex("(?i)(token|secret|password|authorization|api[-_ ]?key|private[-_ ]?key)\\s*[:=]\\s*(?:\"[^\"]*(?:\"|$)|'[^']*(?:'|$)|[^\\s,;]+)")
    private val credentials = Regex("(?i)\\b(?:bearer|basic)\\s+[^\\s,;]+")
    private val urlCredentials = Regex("(?i)(https?://)[^/@\\s]+:[^/@\\s]+@")
    private val quoted = Regex("\"[^\"]*(?:\"|$)|'[^']*(?:'|$)")
    private val apiKeys = Regex("\\bsk-[A-Za-z0-9_-]{12,}")
    private val eventName = Regex("[a-z][a-z0-9_.-]{0,95}")

    private fun safeMetadata(value: String): String {
        var text = value.take(4096)
        text = credentials.replace(text, "<redacted>")
        text = assignments.replace(text) { "${it.groupValues[1]}=<redacted>" }
        text = urlCredentials.replace(text) { "${it.groupValues[1]}<redacted>@" }
        text = apiKeys.replace(text, "<redacted>")
        text = quoted.replace(text, "<redacted>")
        return text.replace(Regex("[\\p{Cntrl}]"), " ").take(256).ifBlank { "unavailable" }
    }

    class RetainedFailure(val details: String, cause: Throwable) : RuntimeException("Chat turn failed", cause)

    fun retainedFailure(error: Throwable?): String? {
        var current = error
        repeat(8) {
            if (current is RetainedFailure) return (current as RetainedFailure).details
            current = current?.cause
        }
        return null
    }

    fun causeCode(error: Throwable?): String = when (error) {
        null -> "unavailable"
        is java.util.concurrent.CancellationException -> "cancelled"
        is java.util.concurrent.TimeoutException -> "timeout"
        is java.io.IOException -> "io_failure"
        is IllegalArgumentException -> "invalid_argument"
        is IllegalStateException -> "invalid_state"
        else -> "runtime_failure"
    }

    fun capture(
        version: String,
        versionCode: Int,
        sourceSha: String,
        runtimeMode: String,
        conversationId: String,
        turnId: String?,
        code: String,
        error: Throwable?,
        requestId: String? = null,
        secondaryError: Throwable? = null,
        events: List<String> = emptyList(),
        localServerPhase: String? = null,
        localServerGeneration: Long? = null,
        capturedAtMillis: Long = System.currentTimeMillis(),
    ): String {
        val text = buildString {
            append("ZARA-CHAT-ERROR/1\n")
            fun field(name: String, value: Any?) { append(name).append('=').append(safeMetadata(value?.toString() ?: "unknown")).append('\n') }
            field("captured_at", Instant.ofEpochMilli(capturedAtMillis))
            field("version", version)
            field("version_code", versionCode)
            field("source_sha", sourceSha)
            field("runtime_mode", runtimeMode)
            field("conversation_id", conversationId)
            field("request_id", requestId)
            field("turn_id", turnId)
            field("code", code)
            field("local_server_phase", localServerPhase)
            field("local_server_generation", localServerGeneration)
            fun cause(prefix: String, failure: Throwable?) {
                var root = failure
                repeat(4) { root?.cause?.takeUnless { it === root }?.let { root = it } }
                field("${prefix}cause_class", root?.javaClass?.name ?: "unavailable")
                field("${prefix}cause_code", causeCode(root))
                root?.stackTrace?.take(4)?.forEachIndexed { index, frame ->
                    field("${prefix}frame_$index", "${frame.className}.${frame.methodName}:${frame.lineNumber}")
                }
            }
            cause("", error)
            if (secondaryError != null) cause("fallback_", secondaryError)
            append("--- turn events ---\n")
            events.takeLast(8).filter { eventName.matches(it) }.forEach { append(it).append('\n') }
        }
        val result = StringBuilder()
        var bytes = 0
        for (point in text.codePoints().toArray()) {
            val size = String(Character.toChars(point)).toByteArray(Charsets.UTF_8).size
            if (bytes + size > MAX_BYTES - 16) { result.append("\n[truncated]\n"); break }
            result.appendCodePoint(point)
            bytes += size
        }
        return result.toString()
    }
}
