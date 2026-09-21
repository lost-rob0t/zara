package ai.zara.app.ui

import ai.zara.app.telemetry.ClientEventJournal
import ai.zara.app.telemetry.ZaraFailure
import ai.zara.app.telemetry.ZaraFailureCodes
import ai.zara.app.telemetry.ZaraRecovery

data class TurnFailure(
    val title: String,
    val explanation: String,
    val subsystem: String,
    val operation: String,
    val code: String,
    val connectionState: String,
    val recovery: String,
    val incidentId: String?,
    val retryPossible: Boolean,
    val reconnectPossible: Boolean,
    val attempt: Int = 1,
    val maxAttempts: Int = TurnRetryPolicy.MAX_ATTEMPTS,
    val autoRetrying: Boolean = false,
)

object TurnFailures {
    fun from(
        failure: ZaraFailure,
        transportConnected: Boolean,
        incidentId: String?,
    ): TurnFailure {
        val retryPossible = when (failure.recovery) {
            ZaraRecovery.RETRYABLE, ZaraRecovery.RETRYING -> true
            else -> false
        }
        return TurnFailure(
            title = titleFor(failure),
            explanation = explanationFor(failure),
            subsystem = failure.subsystem.name.lowercase(),
            operation = failure.operation.name.lowercase(),
            code = failure.code,
            connectionState = if (transportConnected) "connected" else "disconnected",
            recovery = when (failure.recovery) {
                ZaraRecovery.RETRYING -> "retrying"
                ZaraRecovery.RETRYABLE -> "retryable"
                ZaraRecovery.REQUIRES_ACTION -> "requires_action"
                ZaraRecovery.FATAL -> "fatal"
                ZaraRecovery.UNKNOWN -> "unknown"
            },
            incidentId = incidentId,
            retryPossible = retryPossible,
            reconnectPossible = !transportConnected,
        )
    }

    fun mostSpecific(existing: TurnFailure?, candidate: TurnFailure): TurnFailure {
        if (existing == null) return candidate
        if (existing.incidentId != null && candidate.incidentId == existing.incidentId) return existing
        if (candidate.code == ZaraFailureCodes.UNKNOWN && existing.code != ZaraFailureCodes.UNKNOWN) return existing
        return candidate
    }

    fun renderSummary(failure: TurnFailure): String = buildString {
        append(failure.title)
        append('\n')
        append(failure.explanation)
        append("\nCode: ")
        append(failure.code)
        append("\nConnection: ")
        append(failure.connectionState)
        append("\nRecovery: ")
        append(failure.recovery)
        append("\nAttempt: ")
        append(failure.attempt)
        append('/')
        append(failure.maxAttempts)
        if (failure.incidentId != null) {
            append("\nIncident: ")
            append(failure.incidentId)
        }
        val safe = ClientEventJournal.sanitize(toString())
        return safe.replace(Regex("tcp://[^\\s,;]+"), "<endpoint>")
    }

    fun titleFor(failure: ZaraFailure): String = when {
        failure.subsystem == ai.zara.app.telemetry.ZaraSubsystem.PROTOCOL -> "Remote protocol failed"
        failure.code == ZaraFailureCodes.TRANSPORT_TIMEOUT -> "Remote connection lost"
        failure.code == ZaraFailureCodes.TRANSPORT_CLOSED -> "Remote connection lost"
        failure.code == ZaraFailureCodes.TRANSPORT_CONNECT -> "Remote connection failed"
        failure.code == ZaraFailureCodes.TRANSPORT_DNS -> "Remote server unreachable"
        failure.code == ZaraFailureCodes.REMOTE_NOT_CONNECTED -> "Remote connection required"
        failure.code == ZaraFailureCodes.AUTH_REJECTED -> "Enrollment rejected"
        failure.subsystem == ai.zara.app.telemetry.ZaraSubsystem.VOICE ||
            failure.subsystem == ai.zara.app.telemetry.ZaraSubsystem.STT -> "Voice turn failed"
        failure.subsystem == ai.zara.app.telemetry.ZaraSubsystem.TTS -> "Voice playback failed"
        failure.subsystem == ai.zara.app.telemetry.ZaraSubsystem.REMOTE_AI -> "Remote response failed"
        failure.code == ZaraFailureCodes.UNKNOWN -> "Request failed"
        else -> "Request failed"
    }

    fun explanationFor(failure: ZaraFailure): String = when (failure.code) {
        ZaraFailureCodes.PROTOCOL_VERSION_MISMATCH ->
            "The server speaks an incompatible protocol version. Update Zara on this device or the server."
        ZaraFailureCodes.PROTOCOL_MALFORMED ->
            "The server sent a malformed message. The session will reconnect."
        ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE ->
            "Unexpected message while waiting for the response stream."
        ZaraFailureCodes.PROTOCOL_OUT_OF_ORDER ->
            "Server replies arrived out of order for this request."
        ZaraFailureCodes.PROTOCOL_STALE_GENERATION ->
            "A stale session frame arrived after reconnect and was rejected."
        ZaraFailureCodes.PROTOCOL_UNSUPPORTED_MESSAGE ->
            "The server sent a message type this client does not support."
        ZaraFailureCodes.PROTOCOL_SERVER_ERROR ->
            "The server rejected this request."
        ZaraFailureCodes.PROTOCOL_TURN_CANCELLED ->
            "This turn was cancelled before completion."
        ZaraFailureCodes.PROTOCOL_RUNTIME_ERROR ->
            "The server runtime reported an error."
        ZaraFailureCodes.PROTOCOL_RUNTIME_STOPPED ->
            "The server runtime stopped this session."
        ZaraFailureCodes.TRANSPORT_TIMEOUT ->
            "The remote server stopped responding."
        ZaraFailureCodes.TRANSPORT_CLOSED ->
            "The remote connection dropped."
        ZaraFailureCodes.TRANSPORT_CONNECT ->
            "Could not reach the remote server."
        ZaraFailureCodes.TRANSPORT_DNS ->
            "The remote server address could not be resolved."
        ZaraFailureCodes.REMOTE_NOT_CONNECTED ->
            "Remote mode needs a working server connection before sending."
        ZaraFailureCodes.AUTH_REJECTED ->
            "The server rejected this device enrollment. Re-enroll and reconnect."
        ZaraFailureCodes.VOICE_CAPTURE -> "Microphone capture failed."
        ZaraFailureCodes.VOICE_STT -> "Speech recognition failed before sending."
        ZaraFailureCodes.VOICE_SUBMIT -> "The voice turn could not be submitted."
        ZaraFailureCodes.VOICE_REMOTE_RESPONSE -> "The streamed remote response failed."
        ZaraFailureCodes.VOICE_TTS -> "Speech playback failed after a successful response."
        ZaraFailureCodes.UNKNOWN -> "The request failed for an unidentified reason."
        else -> "The request failed."
    }
}
