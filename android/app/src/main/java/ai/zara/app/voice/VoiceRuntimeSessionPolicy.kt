package ai.zara.app.voice

import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection

object VoiceRuntimeSessionPolicy {
    fun shouldInterruptPlayback(
        runtime: RuntimeState,
        stream: VoiceStreamState?,
    ): Boolean {
        if (stream == null) return false
        if (runtime.server !is ServerConnection.Connected) return true
        return runtime.sessionId != stream.sessionId
    }
}
