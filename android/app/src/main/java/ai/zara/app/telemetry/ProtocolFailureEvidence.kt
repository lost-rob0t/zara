package ai.zara.app.telemetry

import java.util.ArrayDeque

data class ProtocolMessageEvidence(
    val direction: String,
    val messageType: String?,
    val messageId: String?,
    val replyTo: String?,
    val sessionId: String?,
    val turnId: String?,
    val sequence: Long?,
    val bytes: Long,
    val frameCount: Int,
    val state: String,
)

data class ProtocolFailureEvidence(
    val connectionGeneration: Long,
    val sessionId: String,
    val requestId: String,
    val turnId: String?,
    val phase: String,
    val expectedMessageType: String,
    val pendingRequests: Int,
    val requestTimeoutMillis: Int,
    val elapsedMillis: Long,
    val lastRx: ProtocolMessageEvidence?,
    val lastTx: ProtocolMessageEvidence?,
    val messages: List<ProtocolMessageEvidence>,
    val droppedMessages: Long,
)

class TextTurnProtocolTrace(
    private val connectionGeneration: Long,
    sessionId: String,
    requestId: String,
    private val requestTimeoutMillis: Int,
    private val nanoClock: () -> Long = System::nanoTime,
) {
    private val sessionId = requireNotNull(token(sessionId))
    private val requestId = requireNotNull(token(requestId))
    private val startedAtNanos = nanoClock()
    private val messages = ArrayDeque<ProtocolMessageEvidence>()
    private var lastRx: ProtocolMessageEvidence? = null
    private var lastTx: ProtocolMessageEvidence? = null
    private var droppedMessages = 0L
    private var turnId: String? = null
    private var phase = "awaiting_turn_accepted"
    private var expectedMessageType = "turn.accepted"
    private var pendingRequests = 1

    init {
        require(connectionGeneration > 0)
        require(requestTimeoutMillis > 0)
    }

    fun transmitAttempt(bytes: Long, frameCount: Int) {
        val event = ProtocolMessageEvidence(
            direction = "tx",
            messageType = "turn.submit",
            messageId = requestId,
            replyTo = null,
            sessionId = sessionId,
            turnId = null,
            sequence = null,
            bytes = bytes,
            frameCount = frameCount,
            state = "attempted",
        )
        append(event)
        lastTx = event
    }

    fun receive(bytes: Long, frameCount: Int) {
        val event = ProtocolMessageEvidence(
            direction = "rx",
            messageType = null,
            messageId = null,
            replyTo = null,
            sessionId = null,
            turnId = null,
            sequence = null,
            bytes = bytes,
            frameCount = frameCount,
            state = "undecoded",
        )
        append(event)
        lastRx = event
    }

    fun decoded(
        messageType: String,
        messageId: String?,
        replyTo: String?,
        sessionId: String?,
        turnId: String?,
        sequence: Long?,
    ) {
        val current = messages.peekLast() ?: return
        if (current.direction != "rx") return
        val decoded = current.copy(
            messageType = token(messageType, 64),
            messageId = token(messageId),
            replyTo = token(replyTo),
            sessionId = token(sessionId),
            turnId = token(turnId),
            sequence = sequence,
            state = "decoded",
        )
        messages.removeLast()
        messages.addLast(decoded)
        lastRx = decoded
    }

    fun accepted(turnId: String) {
        this.turnId = token(turnId)
        phase = "awaiting_turn_completion"
        expectedMessageType = "turn.completed|assistant.response"
        pendingRequests = 0
    }

    fun snapshot(): ProtocolFailureEvidence = ProtocolFailureEvidence(
        connectionGeneration = connectionGeneration,
        sessionId = sessionId,
        requestId = requestId,
        turnId = turnId,
        phase = phase,
        expectedMessageType = expectedMessageType,
        pendingRequests = pendingRequests,
        requestTimeoutMillis = requestTimeoutMillis,
        elapsedMillis = ((nanoClock() - startedAtNanos).coerceAtLeast(0L) / 1_000_000L),
        lastRx = lastRx,
        lastTx = lastTx,
        messages = messages.toList(),
        droppedMessages = droppedMessages,
    )

    private fun append(event: ProtocolMessageEvidence) {
        require(event.bytes >= 0)
        require(event.frameCount >= 0)
        if (messages.size == MAX_MESSAGES) {
            messages.removeFirst()
            if (droppedMessages < Long.MAX_VALUE) droppedMessages += 1
        }
        messages.addLast(event)
    }

    companion object {
        private const val MAX_MESSAGES = 32
        private val UNSAFE_TOKEN_CHARACTER = Regex("[^A-Za-z0-9_.:-]")

        private fun token(value: String?, limit: Int = 128): String? =
            value?.take(limit)?.replace(UNSAFE_TOKEN_CHARACTER, "_")
    }
}
