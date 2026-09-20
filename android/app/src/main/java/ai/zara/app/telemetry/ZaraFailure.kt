package ai.zara.app.telemetry

import ai.zara.app.auth.AuthenticationException
import ai.zara.app.runtime.StaleTextSessionException
import ai.zara.app.runtime.TextRequestTimeoutException
import ai.zara.app.runtime.ZaraWireException
import ai.zara.app.voice.VoiceStreamBackpressureException
import java.io.IOException

enum class ZaraSubsystem { TRANSPORT, PROTOCOL, AUTH, VOICE, STT, REMOTE_AI, TTS, PROLOG, STORAGE, LIFECYCLE }

enum class ZaraOperation { CONNECT, HANDSHAKE, VOICE_TURN, SUBMIT, STREAM, SPEAK, RESTORE }

enum class ZaraRecovery { RETRYING, RETRYABLE, REQUIRES_ACTION, FATAL, UNKNOWN }

class RemoteUnavailableException(message: String = "remote runtime is not connected") :
    IllegalStateException(message)

object ZaraFailureCodes {
    const val TRANSPORT_DNS = "transport.dns"
    const val TRANSPORT_CONNECT = "transport.connect"
    const val TRANSPORT_TIMEOUT = "transport.timeout"
    const val TRANSPORT_CLOSED = "transport.closed"
    const val PROTOCOL_VERSION_MISMATCH = "protocol.version_mismatch"
    const val PROTOCOL_MALFORMED = "protocol.malformed"
    const val PROTOCOL_UNEXPECTED_MESSAGE = "protocol.unexpected_message"
    const val PROTOCOL_OUT_OF_ORDER = "protocol.out_of_order"
    const val PROTOCOL_STALE_GENERATION = "protocol.stale_generation"
    const val PROTOCOL_UNSUPPORTED_MESSAGE = "protocol.unsupported_message"
    const val PROTOCOL_SERVER_ERROR = "protocol.server_error"
    const val PROTOCOL_TURN_CANCELLED = "protocol.turn_cancelled"
    const val PROTOCOL_RUNTIME_ERROR = "protocol.runtime_error"
    const val PROTOCOL_RUNTIME_STOPPED = "protocol.runtime_stopped"
    const val AUTH_REJECTED = "auth.rejected"
    const val VOICE_CAPTURE = "voice.capture"
    const val VOICE_STT = "voice.stt"
    const val VOICE_SUBMIT = "voice.submit"
    const val VOICE_REMOTE_RESPONSE = "voice.remote_response"
    const val VOICE_TTS = "voice.tts"
    const val LIFECYCLE_RESTORE = "lifecycle.restore"
    const val REMOTE_NOT_CONNECTED = "remote.not_connected"
    const val UNKNOWN = "unknown"

    val ALL: Set<String> = setOf(
        TRANSPORT_DNS, TRANSPORT_CONNECT, TRANSPORT_TIMEOUT, TRANSPORT_CLOSED,
        PROTOCOL_VERSION_MISMATCH, PROTOCOL_MALFORMED, PROTOCOL_UNEXPECTED_MESSAGE,
        PROTOCOL_OUT_OF_ORDER, PROTOCOL_STALE_GENERATION, PROTOCOL_UNSUPPORTED_MESSAGE,
        PROTOCOL_SERVER_ERROR, PROTOCOL_TURN_CANCELLED, PROTOCOL_RUNTIME_ERROR,
        PROTOCOL_RUNTIME_STOPPED, AUTH_REJECTED, VOICE_CAPTURE, VOICE_STT, VOICE_SUBMIT,
        VOICE_REMOTE_RESPONSE, VOICE_TTS, LIFECYCLE_RESTORE, REMOTE_NOT_CONNECTED, UNKNOWN,
    )
}

data class ZaraFailure(
    val subsystem: ZaraSubsystem,
    val operation: ZaraOperation,
    val phase: String?,
    val code: String,
    val message: String,
    val causeClass: String,
    val serverCode: String?,
    val retryable: Boolean?,
    val recovery: ZaraRecovery,
    val connectionGeneration: Long?,
    val requestId: String?,
    val turnId: String?,
)

object ZaraFailures {
    private const val MAX_MESSAGE_CHARS = 512
    private val TRANSPORT_CODES = setOf(
        ZaraFailureCodes.TRANSPORT_DNS,
        ZaraFailureCodes.TRANSPORT_CONNECT,
        ZaraFailureCodes.TRANSPORT_TIMEOUT,
        ZaraFailureCodes.TRANSPORT_CLOSED,
    )

    fun classify(
        error: Throwable,
        operation: ZaraOperation,
        phase: String? = null,
        connectionGeneration: Long? = null,
        requestId: String? = null,
        turnId: String? = null,
    ): ZaraFailure {
        val root = rootCause(error)
        val (code, serverCode, retryable) = when (root) {
            is ZaraWireException -> Triple(root.code, root.serverCode, root.retryable)
            is TextRequestTimeoutException -> Triple(ZaraFailureCodes.TRANSPORT_TIMEOUT, null, null)
            is StaleTextSessionException -> Triple(ZaraFailureCodes.PROTOCOL_STALE_GENERATION, null, null)
            is RemoteUnavailableException -> Triple(ZaraFailureCodes.REMOTE_NOT_CONNECTED, null, null)
            is AuthenticationException -> Triple(ZaraFailureCodes.AUTH_REJECTED, null, null)
            is VoiceStreamBackpressureException -> Triple(ZaraFailureCodes.VOICE_REMOTE_RESPONSE, null, null)
            is IOException -> Triple(ZaraFailureCodes.TRANSPORT_CLOSED, null, null)
            else -> Triple(ZaraFailureCodes.UNKNOWN, null, null)
        }
        return ZaraFailure(
            subsystem = subsystemFor(code, operation),
            operation = operation,
            phase = phase,
            code = code,
            message = bounded(root.message ?: root.javaClass.simpleName),
            causeClass = root.javaClass.name,
            serverCode = serverCode,
            retryable = retryable,
            recovery = recoveryFor(code, retryable),
            connectionGeneration = connectionGeneration,
            requestId = requestId,
            turnId = turnId,
        )
    }

    fun subsystemFor(code: String, operation: ZaraOperation): ZaraSubsystem = when {
        code in TRANSPORT_CODES -> ZaraSubsystem.TRANSPORT
        code.startsWith("protocol.") -> ZaraSubsystem.PROTOCOL
        code == ZaraFailureCodes.AUTH_REJECTED -> ZaraSubsystem.AUTH
        code == ZaraFailureCodes.REMOTE_NOT_CONNECTED -> ZaraSubsystem.TRANSPORT
        code.startsWith("voice.") -> when (operation) {
            ZaraOperation.VOICE_TURN, ZaraOperation.SPEAK -> ZaraSubsystem.VOICE
            else -> ZaraSubsystem.VOICE
        }
        code.startsWith("lifecycle.") -> ZaraSubsystem.LIFECYCLE
        operation == ZaraOperation.VOICE_TURN -> ZaraSubsystem.VOICE
        else -> ZaraSubsystem.LIFECYCLE
    }

    fun recoveryFor(code: String, retryable: Boolean?): ZaraRecovery = when {
        code == ZaraFailureCodes.PROTOCOL_SERVER_ERROR && retryable == true -> ZaraRecovery.RETRYABLE
        code == ZaraFailureCodes.PROTOCOL_SERVER_ERROR && retryable == false -> ZaraRecovery.REQUIRES_ACTION
        code == ZaraFailureCodes.PROTOCOL_TURN_CANCELLED -> ZaraRecovery.RETRYABLE
        code == ZaraFailureCodes.PROTOCOL_RUNTIME_ERROR -> if (retryable == false) ZaraRecovery.FATAL else ZaraRecovery.RETRYABLE
        code in TRANSPORT_CODES -> ZaraRecovery.RETRYABLE
        code.startsWith("protocol.") -> ZaraRecovery.RETRYABLE
        code == ZaraFailureCodes.AUTH_REJECTED -> ZaraRecovery.REQUIRES_ACTION
        code == ZaraFailureCodes.REMOTE_NOT_CONNECTED -> ZaraRecovery.REQUIRES_ACTION
        code.startsWith("voice.") -> ZaraRecovery.RETRYABLE
        code == ZaraFailureCodes.LIFECYCLE_RESTORE -> ZaraRecovery.RETRYABLE
        else -> ZaraRecovery.UNKNOWN
    }

    fun isSessionDesyncing(code: String, retryable: Boolean?): Boolean = when (code) {
        ZaraFailureCodes.TRANSPORT_DNS,
        ZaraFailureCodes.TRANSPORT_CONNECT,
        ZaraFailureCodes.TRANSPORT_TIMEOUT,
        ZaraFailureCodes.TRANSPORT_CLOSED,
        ZaraFailureCodes.PROTOCOL_VERSION_MISMATCH,
        ZaraFailureCodes.PROTOCOL_MALFORMED,
        ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
        ZaraFailureCodes.PROTOCOL_OUT_OF_ORDER,
        ZaraFailureCodes.PROTOCOL_STALE_GENERATION,
        ZaraFailureCodes.PROTOCOL_UNSUPPORTED_MESSAGE,
        ZaraFailureCodes.PROTOCOL_RUNTIME_STOPPED,
        -> true
        ZaraFailureCodes.PROTOCOL_RUNTIME_ERROR -> retryable == false
        else -> false
    }

    fun bounded(text: String): String {
        val cleaned = text.replace(Regex("[\\r\\n\\t]"), " ").trim()
        return if (cleaned.length <= MAX_MESSAGE_CHARS) cleaned else cleaned.take(MAX_MESSAGE_CHARS)
    }

    private fun rootCause(error: Throwable): Throwable {
        var current = error
        var depth = 0
        while (current.cause != null && current.cause !== current && depth < 8) {
            current = current.cause!!
            depth += 1
        }
        return current
    }
}
