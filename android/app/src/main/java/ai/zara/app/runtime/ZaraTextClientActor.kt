package ai.zara.app.runtime

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

fun interface TextDealerFactory {
    fun create(endpoint: String): TextDealer
}

interface TextDealer : AutoCloseable {
    fun send(frames: List<ByteArray>)
    fun receive(timeoutMillis: Int): List<ByteArray>?
    override fun close()
}

class StaleTextSessionException(message: String) : IllegalStateException(message)
class TextRequestTimeoutException(message: String) : IllegalStateException(message)


data class ConnectedTextSession(
    val generation: Long,
    val sessionId: String,
)

data class TextTurnResult(
    val conversationId: String?,
    val turnId: String,
    val text: String,
    val success: Boolean,
)

class ZaraTextClientActor(
    private val dealerFactory: TextDealerFactory,
    private val requestIds: Iterator<String> = generateSequence { java.util.UUID.randomUUID().toString().replace("-", "") }.iterator(),
    private val timestamps: Iterator<Long> = generateSequence { System.nanoTime() }.iterator(),
    private val requestTimeoutMillis: Int = 5_000,
    private val executor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-android-text-client").apply { isDaemon = true }
    },
) : TextSessionClient {
    private var dealer: TextDealer? = null
    private var session: ConnectedTextSession? = null
    private val correlations = RequestCorrelations(limit = 256)
    private var closed = false

    init {
        require(requestTimeoutMillis > 0) { "request timeout must be positive" }
    }

    override fun connect(
        profile: ServerProfile,
        generation: Long,
    ): CompletableFuture<ConnectedTextSession> =
        submit {
            require(generation > 0) { "generation must be positive" }
            replaceDealer(profile)
            correlations.clear()
            session = null
            val requestId = nextRequestId()
            val active = requireNotNull(dealer)
            active.send(ZaraTextCodec.encodeHello(requestId, nextTimestamp()))
            val response = receiveMessage(active)
            if (response is TextServerMessage.ProtocolError) {
                throw ZaraWireException("hello failed: ${response.code}")
            }
            val hello = response as? TextServerMessage.HelloOk
                ?: throw ZaraWireException("expected hello.ok")
            if (hello.replyTo != requestId) throw ZaraWireException("hello reply correlation mismatch")

            val capabilityRequestId = nextRequestId()
            active.send(
                ZaraCapabilityCodec.encodeSnapshot(
                    requestId = capabilityRequestId,
                    sessionId = hello.sessionId,
                    capabilities = emptySet(),
                    timestampNs = nextTimestamp(),
                )
            )
            val capabilityFrames = active.receive(requestTimeoutMillis)
                ?: throw TextRequestTimeoutException("ZARA/1 capability negotiation timed out")
            val capabilityAck = ZaraCapabilityCodec.decodeSnapshotOk(capabilityFrames)
            if (capabilityAck.replyTo != capabilityRequestId) {
                throw ZaraWireException("capability snapshot reply correlation mismatch")
            }
            if (capabilityAck.sessionId != hello.sessionId) {
                throw ZaraWireException("capability snapshot session is stale")
            }
            if (capabilityAck.capabilities.isNotEmpty()) {
                throw ZaraWireException("server acknowledged unadvertised device capabilities")
            }

            val connected = ConnectedTextSession(generation, hello.sessionId)
            session = connected
            connected
        }

    override fun submitText(
        generation: Long,
        sessionId: String,
        conversationId: String?,
        text: String,
    ): CompletableFuture<TextTurnResult> = submit {
        val current = session
            ?: throw StaleTextSessionException("text client is not connected")
        if (current.generation != generation || current.sessionId != sessionId) {
            throw StaleTextSessionException("text request belongs to a stale session")
        }
        val active = dealer ?: throw StaleTextSessionException("text dealer is unavailable")
        val requestId = nextRequestId()
        correlations.register(requestId, generation, sessionId)
        try {
            active.send(
                ZaraTextCodec.encodeTurnSubmit(
                    requestId = requestId,
                    sessionId = sessionId,
                    conversationId = conversationId,
                    text = text,
                    timestampNs = nextTimestamp(),
                )
            )
            val first = receiveMessage(active)
            if (first is TextServerMessage.ProtocolError) {
                verifySession(first.sessionId, sessionId)
                correlations.complete(requestId, generation, sessionId)
                throw ZaraWireException("turn submit failed: ${first.code}")
            }
            val accepted = first as? TextServerMessage.TurnAccepted
                ?: throw ZaraWireException("expected turn.accepted")
            verifySession(accepted.sessionId, sessionId)
            if (accepted.replyTo != requestId) throw ZaraWireException("turn reply correlation mismatch")
            if (correlations.complete(requestId, generation, sessionId) != CorrelationResult.Accepted) {
                throw ZaraWireException("turn reply was stale or unknown")
            }
            if (conversationId != null && accepted.conversationId != conversationId) {
                throw ZaraWireException("turn conversation correlation mismatch")
            }

            var assistantCompletion: TextTurnResult? = null
            while (true) {
                when (val event = receiveMessage(active)) {
                    is TextServerMessage.Progress -> {
                        verifyEvent(
                            event.sessionId,
                            event.turnId,
                            accepted.turnId,
                            event.conversationId,
                            accepted.conversationId,
                        )
                    }
                    is TextServerMessage.AssistantDelta -> {
                        verifyEvent(
                            event.sessionId,
                            event.turnId,
                            accepted.turnId,
                            event.conversationId,
                            accepted.conversationId,
                        )
                    }
                    is TextServerMessage.AssistantCompleted -> {
                        verifyEvent(
                            event.sessionId,
                            event.turnId,
                            accepted.turnId,
                            event.conversationId,
                            accepted.conversationId,
                        )
                        if (assistantCompletion != null) {
                            throw ZaraWireException("duplicate assistant completion")
                        }
                        assistantCompletion = TextTurnResult(
                            conversationId = event.conversationId,
                            turnId = event.turnId,
                            text = event.text,
                            success = event.success,
                        )
                    }
                    is TextServerMessage.TurnCompleted -> {
                        verifyEvent(
                            event.sessionId,
                            event.turnId,
                            accepted.turnId,
                            event.conversationId,
                            accepted.conversationId,
                        )
                        val completion = assistantCompletion
                            ?: throw ZaraWireException("turn completed before assistant completion")
                        if (event.success != completion.success) {
                            throw ZaraWireException("turn completion success mismatch")
                        }
                        return@submit completion
                    }
                    is TextServerMessage.AssistantResponse -> {
                        verifySession(event.sessionId, sessionId)
                        if (event.turnId != null && event.turnId != accepted.turnId) {
                            throw ZaraWireException("assistant response turn is stale")
                        }
                        if (accepted.conversationId != null && event.conversationId != accepted.conversationId) {
                            throw ZaraWireException("assistant response conversation is stale")
                        }
                        return@submit TextTurnResult(
                            conversationId = event.conversationId ?: accepted.conversationId,
                            turnId = accepted.turnId,
                            text = event.text,
                            success = !event.truncated,
                        )
                    }
                    is TextServerMessage.ProtocolError -> {
                        verifySession(event.sessionId, sessionId)
                        throw ZaraWireException("turn failed: ${event.code}")
                    }
                    is TextServerMessage.HelloOk, is TextServerMessage.TurnAccepted ->
                        throw ZaraWireException("unexpected response during assistant turn")
                }
            }
            error("unreachable")
        } finally {
            correlations.complete(requestId, generation, sessionId)
        }
    }

    override fun disconnect(): CompletableFuture<Unit> = submit {
        session = null
        correlations.clear()
        dealer?.close()
        dealer = null
    }

    override fun close() {
        if (closed) return
        try {
            disconnect().get()
        } finally {
            closed = true
            executor.shutdownNow()
        }
    }

    private fun replaceDealer(profile: ServerProfile) {
        dealer?.close()
        dealer = null
        val created = dealerFactory.create(profile.endpoint)
        dealer = created
    }

    private fun receiveMessage(active: TextDealer): TextServerMessage {
        val frames = active.receive(requestTimeoutMillis)
            ?: throw TextRequestTimeoutException("ZARA/1 response timed out")
        return ZaraTextCodec.decode(frames)
    }

    private fun verifySession(actual: String?, expected: String) {
        if (actual != expected) throw ZaraWireException("message session is stale")
    }

    private fun verifyEvent(
        actualSession: String,
        actualTurn: String,
        expectedTurn: String,
        actualConversation: String?,
        expectedConversation: String?,
    ) {
        verifySession(actualSession, requireNotNull(session).sessionId)
        if (actualTurn != expectedTurn) throw ZaraWireException("assistant event turn is stale")
        if (expectedConversation != null && actualConversation != expectedConversation) {
            throw ZaraWireException("assistant event conversation is stale")
        }
    }

    private fun nextRequestId(): String {
        if (!requestIds.hasNext()) throw IllegalStateException("request id source exhausted")
        return requestIds.next()
    }

    private fun nextTimestamp(): Long {
        if (!timestamps.hasNext()) throw IllegalStateException("timestamp source exhausted")
        return timestamps.next()
    }

    private fun <T> submit(block: () -> T): CompletableFuture<T> {
        if (closed) return CompletableFuture.failedFuture(IllegalStateException("text client is closed"))
        val future = CompletableFuture<T>()
        executor.execute {
            try {
                future.complete(block())
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }
        return future
    }
}