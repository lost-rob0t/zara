package ai.zara.app.runtime

import java.util.ArrayDeque
import java.util.Collections

/** Payload-free evidence captured by the socket-owning actor, not the reconnecting UI. */
data class ProtocolFrameSummary(
    val direction: String,
    val messageType: String,
    val messageId: String?,
    val replyTo: String?,
    val sessionId: String?,
    val turnId: String?,
    val messageSequence: Long?,
    val frameCount: Int,
    val envelopeBytes: Long?,
    val totalBytes: Long,
    val decodeStatus: String,
)

data class ProtocolFailureContext(
    val connectionGeneration: Long,
    val sessionId: String?,
    val phase: String,
    val expectedMessage: String,
    val requestId: String?,
    val turnId: String?,
    val pendingRequestCount: Int,
    val phaseElapsedMillis: Long,
    val lastRx: ProtocolFrameSummary?,
    val lastTx: ProtocolFrameSummary?,
    val recentFrames: List<ProtocolFrameSummary>,
    val droppedFrameCount: Long,
)

/** All mutations run on ZaraTextClientActor's existing single-thread executor. */
internal class ProtocolFailureTrace(
    private val monotonicNanos: () -> Long = System::nanoTime,
) {
    private var active = false
    private var generation = 0L
    private var sessionId: String? = null
    private var phase = "not_started"
    private var expectedMessage = "none"
    private var requestId: String? = null
    private var turnId: String? = null
    private var pendingRequestCount = 0
    private var phaseStartedNanos = 0L
    private var lastRx: ProtocolFrameSummary? = null
    private var lastTx: ProtocolFrameSummary? = null
    private var droppedFrameCount = 0L
    private val frames = ArrayDeque<ProtocolFrameSummary>()

    fun begin(generation: Long, sessionId: String?, requestId: String, phase: String, expected: String) {
        require(generation > 0)
        this.generation = generation
        this.sessionId = safeToken(sessionId)
        turnId = null
        lastRx = null
        lastTx = null
        droppedFrameCount = 0
        frames.clear()
        active = true
        expect(phase, expected, requestId, pending = 1)
    }

    fun expect(phase: String, expected: String, requestId: String?, pending: Int, sessionId: String? = this.sessionId) {
        require(pending in 0..1)
        require(phase.length in 1..64 && expected.length in 1..64)
        this.sessionId = safeToken(sessionId)
        this.phase = phase
        expectedMessage = expected
        this.requestId = safeToken(requestId)
        pendingRequestCount = pending
        phaseStartedNanos = monotonicNanos()
    }

    fun accepted(sessionId: String, turnId: String) {
        this.sessionId = safeToken(sessionId)
        this.turnId = safeToken(turnId)
        expect("streaming_turn", "turn.completed", requestId, pending = 0)
    }

    fun finish() {
        active = false
    }

    fun transmitted(type: String, requestId: String, sessionId: String?, bytes: List<ByteArray>) {
        if (!active) return
        val frame = summarize("tx", bytes).copy(
            messageType = type,
            messageId = safeToken(requestId),
            sessionId = safeToken(sessionId),
            decodeStatus = "encoded",
        )
        lastTx = frame
        retain(frame)
    }

    fun received(bytes: List<ByteArray>) {
        if (!active) return
        val frame = summarize("rx", bytes)
        lastRx = frame
        retain(frame)
    }

    fun decoded(message: TextServerMessage) {
        val previous = lastRx ?: return
        val frame = previous.copy(
            messageType = typeOf(message),
            messageId = safeToken(message.id),
            replyTo = when (message) {
                is TextServerMessage.HelloOk -> safeToken(message.replyTo)
                is TextServerMessage.TurnAccepted -> safeToken(message.replyTo)
                is TextServerMessage.ProtocolError -> safeToken(message.replyTo)
                else -> null
            },
            sessionId = safeToken(message.sessionId),
            turnId = safeToken(turnOf(message)),
            messageSequence = sequenceOf(message),
            decodeStatus = "decoded",
        )
        replaceLastRx(frame)
    }

    fun decodedControl(type: String, sessionId: String? = null, replyTo: String? = null) {
        val previous = lastRx ?: return
        replaceLastRx(previous.copy(
            messageType = type,
            sessionId = safeToken(sessionId),
            replyTo = safeToken(replyTo),
            decodeStatus = "decoded",
        ))
    }

    fun decodeFailed() {
        val previous = lastRx ?: return
        if (previous.decodeStatus == "not_decoded") {
            replaceLastRx(previous.copy(decodeStatus = "decode_failed"))
        }
    }

    fun snapshot(): ProtocolFailureContext? {
        if (!active) return null
        return ProtocolFailureContext(
            connectionGeneration = generation,
            sessionId = sessionId,
            phase = phase,
            expectedMessage = expectedMessage,
            requestId = requestId,
            turnId = turnId,
            pendingRequestCount = pendingRequestCount,
            phaseElapsedMillis = ((monotonicNanos() - phaseStartedNanos) / 1_000_000L).coerceAtLeast(0L),
            lastRx = lastRx,
            lastTx = lastTx,
            recentFrames = Collections.unmodifiableList(frames.toList()),
            droppedFrameCount = droppedFrameCount,
        )
    }

    private fun replaceLastRx(frame: ProtocolFrameSummary) {
        if (!active || frames.peekLast()?.direction != "rx") return
        frames.removeLast()
        frames.addLast(frame)
        lastRx = frame
    }

    private fun retain(frame: ProtocolFrameSummary) {
        if (frames.size == MAX_FRAMES) {
            frames.removeFirst()
            droppedFrameCount += 1
        }
        frames.addLast(frame)
    }

    private fun summarize(direction: String, bytes: List<ByteArray>) = ProtocolFrameSummary(
        direction = direction,
        messageType = "undecoded",
        messageId = null,
        replyTo = null,
        sessionId = null,
        turnId = null,
        messageSequence = null,
        frameCount = bytes.size,
        envelopeBytes = bytes.getOrNull(1)?.size?.toLong(),
        totalBytes = bytes.sumOf { it.size.toLong() },
        decodeStatus = "not_decoded",
    )

    companion object {
        const val MAX_FRAMES = 16

        fun typeOf(message: TextServerMessage): String = when (message) {
            is TextServerMessage.HelloOk -> "hello.ok"
            is TextServerMessage.TurnAccepted -> "turn.accepted"
            is TextServerMessage.Progress -> when (message.type) {
                "turn.started", "assistant.started" -> message.type
                else -> "unknown.progress"
            }
            is TextServerMessage.AssistantDelta -> "assistant.delta"
            is TextServerMessage.AssistantCompleted -> "assistant.completed"
            is TextServerMessage.TurnCompleted -> "turn.completed"
            is TextServerMessage.AssistantResponse -> "assistant.response"
            is TextServerMessage.ProtocolError -> "protocol.error"
            is TextServerMessage.TurnCancelled -> "turn.cancelled"
            is TextServerMessage.RuntimeError -> "runtime.error"
            is TextServerMessage.RuntimeStopped -> "runtime.stopped"
        }

        private fun safeToken(value: String?): String? = value?.takeIf {
            it.length in 1..128 && it.all { character ->
                character in 'a'..'z' || character in 'A'..'Z' || character in '0'..'9' || character in "._:-"
            }
        }

        private fun turnOf(message: TextServerMessage): String? = when (message) {
            is TextServerMessage.TurnAccepted -> message.turnId
            is TextServerMessage.Progress -> message.turnId
            is TextServerMessage.AssistantDelta -> message.turnId
            is TextServerMessage.AssistantCompleted -> message.turnId
            is TextServerMessage.TurnCompleted -> message.turnId
            is TextServerMessage.AssistantResponse -> message.turnId
            is TextServerMessage.TurnCancelled -> message.turnId
            is TextServerMessage.RuntimeError -> message.turnId
            is TextServerMessage.RuntimeStopped -> message.turnId
            is TextServerMessage.HelloOk, is TextServerMessage.ProtocolError -> null
        }

        private fun sequenceOf(message: TextServerMessage): Long? = when (message) {
            is TextServerMessage.Progress -> message.sequence
            is TextServerMessage.AssistantDelta -> message.sequence
            is TextServerMessage.AssistantCompleted -> message.sequence
            is TextServerMessage.TurnCompleted -> message.sequence
            is TextServerMessage.AssistantResponse -> message.sequence
            is TextServerMessage.TurnCancelled -> message.sequence
            is TextServerMessage.RuntimeError -> message.sequence
            is TextServerMessage.RuntimeStopped -> message.sequence
            is TextServerMessage.HelloOk, is TextServerMessage.TurnAccepted, is TextServerMessage.ProtocolError -> null
        }
    }
}

/** Keep the original exception type/identity while carrying immutable actor evidence through futures. */
private class ProtocolFailureEvidence(val context: ProtocolFailureContext) :
    Exception("ZARA protocol failure context (metadata only)", null, false, false)

internal fun attachProtocolFailureContext(error: Throwable, context: ProtocolFailureContext?) {
    if (context == null || protocolFailureContext(error) != null) return
    error.addSuppressed(ProtocolFailureEvidence(context))
}

internal fun protocolFailureContext(error: Throwable): ProtocolFailureContext? {
    var current = error
    repeat(9) {
        current.suppressed.take(8).filterIsInstance<ProtocolFailureEvidence>().firstOrNull()?.let { evidence ->
            return evidence.context
        }
        val cause = current.cause ?: return null
        if (cause === current) return null
        current = cause
    }
    return null
}
