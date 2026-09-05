package ai.zara.app.voice

import ai.zara.app.runtime.ZaraWireException

object VoiceDiagnosticFailure {
    fun summarize(error: Throwable): String = when (rootCause(error)) {
        is ZaraWireException -> "voice_protocol_error"
        is VoiceStreamBackpressureException -> "voice_backpressure"
        is SecurityException -> "voice_permission_denied"
        else -> "voice_runtime_error"
    }

    private fun rootCause(error: Throwable): Throwable {
        var current = error
        repeat(MAX_CAUSE_DEPTH) {
            val cause = current.cause ?: return current
            if (cause === current) return current
            current = cause
        }
        return current
    }

    private const val MAX_CAUSE_DEPTH = 8
}
