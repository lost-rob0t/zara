package ai.zara.app.telemetry

import java.util.concurrent.atomic.AtomicLong

enum class ClientEventOutcome { SUCCESS, FAILURE, CANCELLED }

object ClientEventNames {
    const val REMOTE_CONNECT_BEGIN = "remote.connect.begin"
    const val REMOTE_CONNECT_READY = "remote.connect.ready"
    const val REMOTE_CONNECT_FAILED = "remote.connect.failed"
    const val REMOTE_DISCONNECTED = "remote.disconnected"
    const val REMOTE_RECONNECT_BEGIN = "remote.reconnect.begin"
    const val REMOTE_RECONNECT_READY = "remote.reconnect.ready"
    const val REMOTE_RECONNECT_FAILED = "remote.reconnect.failed"
    const val PROTOCOL_HANDSHAKE_BEGIN = "protocol.handshake.begin"
    const val PROTOCOL_HANDSHAKE_NEGOTIATED = "protocol.handshake.negotiated"
    const val PROTOCOL_HANDSHAKE_FAILED = "protocol.handshake.failed"
    const val PROTOCOL_MESSAGE_TX = "protocol.message.tx"
    const val PROTOCOL_MESSAGE_RX = "protocol.message.rx"
    const val PROTOCOL_MESSAGE_REJECTED = "protocol.message.rejected"
    const val PROTOCOL_FAILED = "protocol.failed"
    const val VOICE_CAPTURE_BEGIN = "voice.capture.begin"
    const val VOICE_CAPTURE_COMPLETE = "voice.capture.complete"
    const val VOICE_CAPTURE_FAILED = "voice.capture.failed"
    const val VOICE_STT_BEGIN = "voice.stt.begin"
    const val VOICE_STT_COMPLETE = "voice.stt.complete"
    const val VOICE_STT_FAILED = "voice.stt.failed"
    const val VOICE_SUBMIT_BEGIN = "voice.submit.begin"
    const val VOICE_SUBMIT_COMPLETE = "voice.submit.complete"
    const val VOICE_SUBMIT_FAILED = "voice.submit.failed"
    const val VOICE_RESPONSE_FIRST_CHUNK = "voice.response.first_chunk"
    const val VOICE_RESPONSE_COMPLETE = "voice.response.complete"
    const val VOICE_RESPONSE_FAILED = "voice.response.failed"
    const val VOICE_TTS_BEGIN = "voice.tts.begin"
    const val VOICE_TTS_COMPLETE = "voice.tts.complete"
    const val VOICE_TTS_FAILED = "voice.tts.failed"
    const val VOICE_PLAYBACK_COMPLETE = "voice.playback.complete"
    const val VOICE_TURN_FAILED = "voice.turn.failed"
    const val SESSION_RESTORE_BEGIN = "session.restore.begin"
    const val SESSION_RESTORE_COMPLETE = "session.restore.complete"
    const val SESSION_RESTORE_FAILED = "session.restore.failed"

    val ALL: Set<String> = setOf(
        REMOTE_CONNECT_BEGIN, REMOTE_CONNECT_READY, REMOTE_CONNECT_FAILED,
        REMOTE_DISCONNECTED, REMOTE_RECONNECT_BEGIN, REMOTE_RECONNECT_READY,
        REMOTE_RECONNECT_FAILED, PROTOCOL_HANDSHAKE_BEGIN, PROTOCOL_HANDSHAKE_NEGOTIATED,
        PROTOCOL_HANDSHAKE_FAILED, PROTOCOL_MESSAGE_TX, PROTOCOL_MESSAGE_RX,
        PROTOCOL_MESSAGE_REJECTED, PROTOCOL_FAILED, VOICE_CAPTURE_BEGIN,
        VOICE_CAPTURE_COMPLETE, VOICE_CAPTURE_FAILED, VOICE_STT_BEGIN, VOICE_STT_COMPLETE,
        VOICE_STT_FAILED, VOICE_SUBMIT_BEGIN, VOICE_SUBMIT_COMPLETE, VOICE_SUBMIT_FAILED,
        VOICE_RESPONSE_FIRST_CHUNK, VOICE_RESPONSE_COMPLETE, VOICE_RESPONSE_FAILED,
        VOICE_TTS_BEGIN, VOICE_TTS_COMPLETE, VOICE_TTS_FAILED, VOICE_PLAYBACK_COMPLETE,
        VOICE_TURN_FAILED, SESSION_RESTORE_BEGIN, SESSION_RESTORE_COMPLETE,
        SESSION_RESTORE_FAILED,
    )
}

data class ClientEvent(
    val sequence: Long,
    val wallTimeMillis: Long,
    val monotonicNanos: Long,
    val name: String,
    val subsystem: ZaraSubsystem?,
    val operation: ZaraOperation?,
    val phase: String?,
    val sessionGeneration: Long?,
    val connectionGeneration: Long?,
    val sessionId: String?,
    val requestId: String?,
    val turnId: String?,
    val outcome: ClientEventOutcome?,
    val code: String?,
    val message: String?,
    val recovery: ZaraRecovery?,
    val messageType: String?,
    val messageSequence: Long?,
    val messageBytes: Long?,
)

class ClientEventLog(
    private val capacity: Int = 512,
    private val wallClock: () -> Long = System::currentTimeMillis,
    private val monotonicClock: () -> Long = System::nanoTime,
) {
    init {
        require(capacity > 0) { "event log capacity must be positive" }
    }

    private val counter = AtomicLong(0)
    private val events = ArrayDeque<ClientEvent>()
    private val lock = Any()

    fun record(
        name: String,
        subsystem: ZaraSubsystem? = null,
        operation: ZaraOperation? = null,
        phase: String? = null,
        sessionGeneration: Long? = null,
        connectionGeneration: Long? = null,
        sessionId: String? = null,
        requestId: String? = null,
        turnId: String? = null,
        outcome: ClientEventOutcome? = null,
        code: String? = null,
        message: String? = null,
        recovery: ZaraRecovery? = null,
    ): ClientEvent {
        require(name in ClientEventNames.ALL) { "unknown telemetry event name: $name" }
        val event = ClientEvent(
            sequence = counter.incrementAndGet(),
            wallTimeMillis = wallClock(),
            monotonicNanos = monotonicClock(),
            name = name,
            subsystem = subsystem,
            operation = operation,
            phase = phase,
            sessionGeneration = sessionGeneration,
            connectionGeneration = connectionGeneration,
            sessionId = sessionId,
            requestId = requestId,
            turnId = turnId,
            outcome = outcome,
            code = code,
            message = message?.let(::sanitize),
            recovery = recovery,
            messageType = null,
            messageSequence = null,
            messageBytes = null,
        )
        append(event)
        return event
    }

    fun recordProtocolMessage(
        direction: Direction,
        messageType: String,
        messageSequence: Long?,
        messageBytes: Long,
        connectionGeneration: Long? = null,
        sessionId: String? = null,
        requestId: String? = null,
        turnId: String? = null,
    ): ClientEvent {
        val name = when (direction) {
            Direction.TX -> ClientEventNames.PROTOCOL_MESSAGE_TX
            Direction.RX -> ClientEventNames.PROTOCOL_MESSAGE_RX
        }
        val event = ClientEvent(
            sequence = counter.incrementAndGet(),
            wallTimeMillis = wallClock(),
            monotonicNanos = monotonicClock(),
            name = name,
            subsystem = ZaraSubsystem.PROTOCOL,
            operation = null,
            phase = null,
            sessionGeneration = null,
            connectionGeneration = connectionGeneration,
            sessionId = sessionId,
            requestId = requestId,
            turnId = turnId,
            outcome = null,
            code = null,
            message = null,
            recovery = null,
            messageType = messageType,
            messageSequence = messageSequence,
            messageBytes = messageBytes,
        )
        append(event)
        return event
    }

    fun snapshot(): List<ClientEvent> = synchronized(lock) { events.sortedBy(ClientEvent::sequence) }

    fun clear() = synchronized(lock) { events.clear() }

    private fun append(event: ClientEvent) {
        synchronized(lock) {
            events.addLast(event)
            while (events.size > capacity) events.removeFirst()
        }
    }

    companion object {
        private val SECRET_PATTERN = Regex(
            "(?i)(token|secret|password|authorization|api[-_]?key|private[-_ ]?key)\\s*[:=]\\s*[^\\s,;]+"
        )
        private const val MAX_MESSAGE_CHARS = 512

        fun sanitize(text: String): String {
            val redacted = SECRET_PATTERN.replace(text) { match ->
                "${match.groupValues[1]}=<redacted>"
            }
            val bounded = if (redacted.length <= MAX_MESSAGE_CHARS) redacted else redacted.take(MAX_MESSAGE_CHARS)
            return bounded.replace(Regex("[\\r\\n\\t]"), " ").ifBlank { "-" }
        }
    }

    enum class Direction { TX, RX }
}
