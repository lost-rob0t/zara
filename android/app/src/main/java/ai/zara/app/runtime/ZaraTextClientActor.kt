package ai.zara.app.runtime

import ai.zara.app.device.DeviceActionErrorCode
import ai.zara.app.device.DeviceActionResult
import ai.zara.app.voice.VoiceCaptureContext
import ai.zara.app.voice.VoiceCommandClient
import ai.zara.app.voice.VoiceInboundMessage
import ai.zara.app.voice.VoiceServerReply
import ai.zara.app.voice.VoiceStreamEvent
import ai.zara.app.voice.ZaraVoiceInboundCodec
import ai.zara.app.voice.ZaraVoiceCodec
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
    private val audioOutputFormats: List<AudioOutputFormat> = emptyList(),
    private val deviceCapabilities: () -> Set<DeviceCapability> = { emptySet() },
    private val deviceActionHandler: DeviceActionHandler? = null,
    private val epochNanoseconds: () -> Long = { System.currentTimeMillis() * 1_000_000L },
) : TextSessionClient, VoiceCommandClient {
    companion object {
        private const val MAX_INTERLEAVED_VOICE_EVENTS = 256
        private const val MAX_TERMINAL_DEVICE_ACTIONS = 256
    }

    private val protocolTrace = ProtocolFailureTrace()
    private var dealer: TextDealer? = null
    private var session: ConnectedTextSession? = null
    private val correlations = RequestCorrelations(limit = 256)
    private val terminalDeviceActions = linkedSetOf<String>()
    private var voiceStreamObserver: ((VoiceStreamEvent) -> Unit)? = null
    private var voiceStreamFailureObserver: ((Throwable) -> Unit)? = null
    private var connectionFailureObserver: ((ai.zara.app.telemetry.ZaraFailure) -> Unit)? = null
    private var staleFrameObserver: ((messageType: String, currentGeneration: Long) -> Unit)? = null
    private var voicePumpActive = false
    private var selectedAudioOutputFormat: AudioOutputFormat? = null
    private var closed = false

    init {
        require(requestTimeoutMillis > 0) { "request timeout must be positive" }
        require(audioOutputFormats.size <= 8) { "audio output offer exceeds format limit" }
        require(audioOutputFormats.distinct() == audioOutputFormats) {
            "audio output offer contains duplicates"
        }
    }

    fun setVoiceStreamObserver(observer: ((VoiceStreamEvent) -> Unit)?) {
        voiceStreamObserver = observer
    }

    fun setVoiceStreamFailureObserver(observer: ((Throwable) -> Unit)?) {
        voiceStreamFailureObserver = observer
    }

    fun setConnectionFailureObserver(observer: ((ai.zara.app.telemetry.ZaraFailure) -> Unit)?) {
        connectionFailureObserver = observer
    }

    fun setStaleFrameObserver(observer: ((messageType: String, currentGeneration: Long) -> Unit)?) {
        staleFrameObserver = observer
    }

    fun negotiatedAudioOutputFormat(): AudioOutputFormat? = selectedAudioOutputFormat

    override fun connect(
        profile: ServerProfile,
        generation: Long,
    ): CompletableFuture<ConnectedTextSession> =
        submit {
            require(generation > 0) { "generation must be positive" }
            replaceDealer(profile)
            correlations.clear()
            terminalDeviceActions.clear()
            session = null
            val requestId = nextRequestId()
            protocolTrace.begin(generation, null, requestId, "awaiting_hello", "hello.ok")
            val active = requireNotNull(dealer)
            val helloSessionId = if (audioOutputFormats.isEmpty()) {
                sendTraced(active, ZaraTextCodec.encodeHello(requestId, nextTimestamp()), "hello", requestId, null)
                val response = receiveMessage(active)
                if (response is TextServerMessage.ProtocolError) {
                    throw ZaraWireException(
                        "hello failed: ${response.code}",
                        code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_SERVER_ERROR,
                        serverCode = response.code,
                        retryable = response.retryable,
                    )
                }
                val hello = response as? TextServerMessage.HelloOk
                    ?: throw ZaraWireException(
                        "expected hello.ok",
                        code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
                    )
                if (hello.replyTo != requestId) throw ZaraWireException("hello reply correlation mismatch")
                selectedAudioOutputFormat = null
                hello.sessionId
            } else {
                sendTraced(
                    active,
                    ZaraVoiceHelloCodec.encodeHello(
                        requestId = requestId,
                        timestampNs = nextTimestamp(),
                        audioOutputFormats = audioOutputFormats,
                    ),
                    "hello", requestId, null,
                )
                val helloFrames = active.receive(requestTimeoutMillis)
                    ?: throw TextRequestTimeoutException("ZARA/1 voice hello timed out")
                protocolTrace.received(helloFrames)
                val hello = ZaraVoiceHelloCodec.decodeHelloOk(helloFrames)
                protocolTrace.decodedControl("hello.ok", hello.sessionId, hello.replyTo)
                if (hello.replyTo != requestId) throw ZaraWireException("voice hello reply correlation mismatch")
                if (hello.audioOutputFormat !in audioOutputFormats) {
                    throw ZaraWireException("server selected an unoffered audio output format")
                }
                selectedAudioOutputFormat = hello.audioOutputFormat
                hello.sessionId
            }

            val advertisedCapabilities = deviceCapabilities()
            if (advertisedCapabilities.isNotEmpty() && deviceActionHandler == null) {
                throw ZaraWireException("executable device capabilities require an action handler")
            }
            val capabilityRequestId = nextRequestId()
            protocolTrace.expect(
                "awaiting_capabilities", "capability.snapshot.ok", capabilityRequestId,
                pending = 1, sessionId = helloSessionId,
            )
            sendTraced(
                active,
                ZaraCapabilityCodec.encodeSnapshot(
                    requestId = capabilityRequestId,
                    sessionId = helloSessionId,
                    capabilities = advertisedCapabilities,
                    timestampNs = nextTimestamp(),
                ),
                "capability.snapshot", capabilityRequestId, helloSessionId,
            )
            val capabilityFrames = active.receive(requestTimeoutMillis)
                ?: throw TextRequestTimeoutException("ZARA/1 capability negotiation timed out")
            protocolTrace.received(capabilityFrames)
            val capabilityAck = ZaraCapabilityCodec.decodeSnapshotOk(capabilityFrames)
            protocolTrace.decodedControl("capability.snapshot.ok", capabilityAck.sessionId, capabilityAck.replyTo)
            if (capabilityAck.replyTo != capabilityRequestId) {
                throw ZaraWireException("capability snapshot reply correlation mismatch")
            }
            if (capabilityAck.sessionId != helloSessionId) {
                throw ZaraWireException("capability snapshot session is stale")
            }
            if (capabilityAck.capabilities != advertisedCapabilities) {
                throw ZaraWireException("server capability acknowledgement differs from advertisement")
            }

            val connected = ConnectedTextSession(generation, helloSessionId)
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
        try {
            submitTextInternal(generation, sessionId, current, conversationId, text)
        } catch (error: Throwable) {
            reportConnectionFailureIfCurrent(error, current, ai.zara.app.telemetry.ZaraOperation.SUBMIT)
            throw error
        }
    }

    private fun submitTextInternal(
        generation: Long,
        sessionId: String,
        connected: ConnectedTextSession,
        conversationId: String?,
        text: String,
    ): TextTurnResult {
        val current = connected
        val active = dealer ?: throw StaleTextSessionException("text dealer is unavailable")
        val requestId = nextRequestId()
        protocolTrace.begin(generation, sessionId, requestId, "awaiting_turn_acceptance", "turn.accepted")
        correlations.register(requestId, generation, sessionId)
        try {
            sendTraced(
                active,
                ZaraTextCodec.encodeTurnSubmit(
                    requestId = requestId,
                    sessionId = sessionId,
                    conversationId = conversationId,
                    text = text,
                    timestampNs = nextTimestamp(),
                ),
                "turn.submit", requestId, sessionId,
            )
            val first = receiveMessage(active)
            if (first is TextServerMessage.ProtocolError) {
                verifySession(first.sessionId, sessionId)
                correlations.complete(requestId, generation, sessionId)
                throw ZaraWireException(
                            "turn submit failed: ${first.code}",
                            code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_SERVER_ERROR,
                            serverCode = first.code,
                            retryable = first.retryable,
                        )
            }
            val accepted = first as? TextServerMessage.TurnAccepted
                ?: throw ZaraWireException(
                    "expected turn.accepted; received ${ProtocolFailureTrace.typeOf(first)}",
                    code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
                )
            verifySession(accepted.sessionId, sessionId)
            if (accepted.replyTo != requestId) throw ZaraWireException(
                        "turn reply correlation mismatch",
                        code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_OUT_OF_ORDER,
                    )
            if (correlations.complete(requestId, generation, sessionId) != CorrelationResult.Accepted) {
                throw ZaraWireException("turn reply was stale or unknown")
            }
            if (conversationId != null && accepted.conversationId != conversationId) {
                throw ZaraWireException("turn conversation correlation mismatch")
            }

            protocolTrace.accepted(sessionId, accepted.turnId)
            var assistantCompletion: TextTurnResult? = null
            while (true) {
                when (val event = receiveMessage(active)) {
                    is TextServerMessage.Progress -> verifyEvent(
                        event.sessionId,
                        event.turnId,
                        accepted.turnId,
                        event.conversationId,
                        accepted.conversationId,
                    )
                    is TextServerMessage.AssistantDelta -> verifyEvent(
                        event.sessionId,
                        event.turnId,
                        accepted.turnId,
                        event.conversationId,
                        accepted.conversationId,
                    )
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
                        return completion
                    }
                    is TextServerMessage.AssistantResponse -> {
                        verifySession(event.sessionId, sessionId)
                        if (event.turnId != null && event.turnId != accepted.turnId) {
                            throw ZaraWireException("assistant response turn is stale")
                        }
                        if (accepted.conversationId != null && event.conversationId != accepted.conversationId) {
                            throw ZaraWireException("assistant response conversation is stale")
                        }
                        return TextTurnResult(
                            conversationId = event.conversationId ?: accepted.conversationId,
                            turnId = accepted.turnId,
                            text = event.text,
                            success = !event.truncated,
                        )
                    }
                    is TextServerMessage.ProtocolError -> {
                        verifySession(event.sessionId, sessionId)
                        throw ZaraWireException(
                            "turn failed: ${event.code}",
                            code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_SERVER_ERROR,
                            serverCode = event.code,
                            retryable = event.retryable,
                        )
                    }
                    is TextServerMessage.TurnCancelled -> {
                        verifyEvent(
                            event.sessionId,
                            event.turnId,
                            accepted.turnId,
                            event.conversationId,
                            accepted.conversationId,
                        )
                        throw ZaraWireException(
                            "turn cancelled: ${event.reason}",
                            code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_TURN_CANCELLED,
                        )
                    }
                    is TextServerMessage.RuntimeError -> {
                        verifySession(event.sessionId, sessionId)
                        throw ZaraWireException(
                            "server runtime error: ${event.reason}",
                            code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_RUNTIME_ERROR,
                            retryable = !event.fatal,
                        )
                    }
                    is TextServerMessage.RuntimeStopped -> {
                        verifySession(event.sessionId, sessionId)
                        throw ZaraWireException(
                            "server runtime stopped: ${event.reason}",
                            code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_RUNTIME_STOPPED,
                            retryable = true,
                        )
                    }
                    is TextServerMessage.HelloOk, is TextServerMessage.TurnAccepted ->
                        throw ZaraWireException(
                            "unexpected response during assistant turn",
                            code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
                        )
                }
            }
            error("unreachable")
        } finally {
            correlations.complete(requestId, generation, sessionId)
        }
    }

    override fun startVoice(context: VoiceCaptureContext): CompletableFuture<Unit> =
        voiceCommand(context, expectedType = "audio.input.started", expectedSequence = null) { requestId, timestamp ->
            ZaraVoiceCodec.encodeStart(
                requestId = requestId,
                sessionId = context.sessionId,
                conversationId = context.conversationId,
                streamId = context.streamId,
                timestampNs = timestamp,
            )
        }

    override fun sendVoiceChunk(
        context: VoiceCaptureContext,
        sequence: Long,
        pcm: ByteArray,
    ): CompletableFuture<Unit> =
        voiceCommand(context, expectedType = "audio.input.accepted", expectedSequence = sequence) { requestId, timestamp ->
            ZaraVoiceCodec.encodeChunk(
                requestId = requestId,
                sessionId = context.sessionId,
                conversationId = context.conversationId,
                streamId = context.streamId,
                sequence = sequence,
                timestampNs = timestamp,
                pcm = pcm,
            )
        }

    override fun commitVoice(context: VoiceCaptureContext): CompletableFuture<Unit> =
        voiceCommand(
            context,
            expectedType = "audio.input.committed",
            expectedSequence = null,
            startPumpAfterReply = true,
        ) { requestId, timestamp ->
            ZaraVoiceCodec.encodeCommit(
                requestId = requestId,
                sessionId = context.sessionId,
                conversationId = context.conversationId,
                streamId = context.streamId,
                timestampNs = timestamp,
            )
        }

    override fun cancelVoice(context: VoiceCaptureContext): CompletableFuture<Unit> =
        voiceCommand(context, expectedType = "audio.input.cancelled", expectedSequence = null) { requestId, timestamp ->
            ZaraVoiceCodec.encodeCancel(
                requestId = requestId,
                sessionId = context.sessionId,
                conversationId = context.conversationId,
                streamId = context.streamId,
                timestampNs = timestamp,
            )
        }

    override fun disconnect(): CompletableFuture<Unit> = submit {
        voicePumpActive = false
        session = null
        correlations.clear()
        terminalDeviceActions.clear()
        selectedAudioOutputFormat = null
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

    private fun voiceCommand(
        context: VoiceCaptureContext,
        expectedType: String,
        expectedSequence: Long?,
        startPumpAfterReply: Boolean = false,
        frames: (requestId: String, timestampNs: Long) -> List<ByteArray>,
    ): CompletableFuture<Unit> = submit {
        val current = session ?: throw StaleTextSessionException("voice client is not connected")
        if (current.sessionId != context.sessionId) {
            throw StaleTextSessionException("voice request belongs to a stale session")
        }
        try {
            voiceCommandInternal(context, current, expectedType, expectedSequence, startPumpAfterReply, frames)
        } catch (error: Throwable) {
            reportConnectionFailureIfCurrent(error, current, ai.zara.app.telemetry.ZaraOperation.VOICE_TURN)
            throw error
        }
    }

    private fun voiceCommandInternal(
        context: VoiceCaptureContext,
        connected: ConnectedTextSession,
        expectedType: String,
        expectedSequence: Long?,
        startPumpAfterReply: Boolean,
        frames: (requestId: String, timestampNs: Long) -> List<ByteArray>,
    ) {
        val current = connected
        val active = dealer ?: throw StaleTextSessionException("voice dealer is unavailable")
        val requestId = nextRequestId()
        active.send(frames(requestId, nextTimestamp()))
        when (val reply = receiveVoiceReply(active)) {
            is VoiceServerReply.ProtocolError -> {
                if (reply.replyTo != null && reply.replyTo != requestId) {
                    throw ZaraWireException(
                        "voice error reply correlation mismatch",
                        code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_OUT_OF_ORDER,
                    )
                }
                if (reply.sessionId != null && reply.sessionId != current.sessionId) {
                    throw ZaraWireException(
                        "voice error session is stale",
                        code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_STALE_GENERATION,
                    )
                }
                throw ZaraWireException(
                    "voice command failed: ${reply.code}",
                    code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_SERVER_ERROR,
                    serverCode = reply.code,
                    retryable = reply.retryable,
                )
            }
            is VoiceServerReply.Acknowledged -> {
                if (reply.type != expectedType) throw ZaraWireException("unexpected voice acknowledgement type")
                if (reply.replyTo != requestId) throw ZaraWireException("voice reply correlation mismatch")
                if (reply.sessionId != current.sessionId) throw ZaraWireException("voice acknowledgement session is stale")
                if (reply.streamId != context.streamId) throw ZaraWireException("voice acknowledgement stream is stale")
                if (reply.sequence != expectedSequence) throw ZaraWireException("voice acknowledgement sequence mismatch")
                if (
                    context.conversationId != null &&
                    reply.conversationId != null &&
                    reply.conversationId != context.conversationId
                ) {
                    throw ZaraWireException("voice acknowledgement conversation is stale")
                }
            }
        }
        if (startPumpAfterReply) startVoicePump()
    }

    private fun receiveVoiceReply(active: TextDealer): VoiceServerReply {
        var interleavedEvents = 0
        while (true) {
            val frames = active.receive(requestTimeoutMillis)
                ?: throw TextRequestTimeoutException("ZARA/1 voice acknowledgement timed out")
            if (handleDeviceServerMessage(active, frames)) continue
            val inbound = decodeVoiceInbound(frames)
            if (inbound == null) continue
            when (inbound) {
                is VoiceInboundMessage.Stream -> {
                    if (interleavedEvents >= MAX_INTERLEAVED_VOICE_EVENTS) {
                        throw ZaraWireException(
                            "voice acknowledgement displaced by too many stream events",
                            code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_OUT_OF_ORDER,
                        )
                    }
                    dispatchVoiceStream(inbound.event)
                    interleavedEvents += 1
                }
                is VoiceInboundMessage.Reply -> return inbound.reply
            }
        }
    }

    private fun decodeVoiceInbound(frames: List<ByteArray>): VoiceInboundMessage? {
        val voiceError: ZaraWireException
        try {
            return ZaraVoiceInboundCodec.decode(frames)
        } catch (error: ZaraWireException) {
            voiceError = error
        }
        val textMessage = try {
            ZaraTextCodec.decode(frames)
        } catch (_: ZaraWireException) {
            throw voiceError
        }
        when (textMessage) {
            is TextServerMessage.ProtocolError -> throw ZaraWireException(
                "voice stream failed: ${textMessage.code}",
                code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_SERVER_ERROR,
                serverCode = textMessage.code,
                retryable = textMessage.retryable,
            )
            is TextServerMessage.TurnCancelled -> throw ZaraWireException(
                "voice turn cancelled: ${textMessage.reason}",
                code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_TURN_CANCELLED,
            )
            is TextServerMessage.RuntimeError -> throw ZaraWireException(
                "server runtime error during voice stream: ${textMessage.reason}",
                code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_RUNTIME_ERROR,
                retryable = !textMessage.fatal,
            )
            is TextServerMessage.RuntimeStopped -> throw ZaraWireException(
                "server runtime stopped during voice stream: ${textMessage.reason}",
                code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_RUNTIME_STOPPED,
                retryable = true,
            )
            is TextServerMessage.HelloOk,
            is TextServerMessage.TurnAccepted,
            is TextServerMessage.Progress,
            is TextServerMessage.AssistantDelta,
            is TextServerMessage.AssistantCompleted,
            is TextServerMessage.TurnCompleted,
            is TextServerMessage.AssistantResponse,
            -> return null
        }
    }

    private fun startVoicePump() {
        if (voicePumpActive || closed || session == null || dealer == null) return
        voicePumpActive = true
        executor.execute(::pollVoiceEvents)
    }

    private fun pollVoiceEvents() {
        if (!voicePumpActive || closed || session == null) return
        val active = dealer ?: return
        try {
            val frames = active.receive(25)
            if (frames == null) {
                Thread.sleep(10)
            } else if (!handleDeviceServerMessage(active, frames)) {
                when (val inbound = decodeVoiceInbound(frames)) {
                    null -> Unit
                    is VoiceInboundMessage.Stream -> dispatchVoiceStream(inbound.event)
                    is VoiceInboundMessage.Reply ->
                        throw ZaraWireException(
                            "unsolicited voice acknowledgement",
                            code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE,
                        )
                }
            }
        } catch (error: InterruptedException) {
            Thread.currentThread().interrupt()
            voicePumpActive = false
            return
        } catch (error: Throwable) {
            voicePumpActive = false
            reportConnectionFailureIfCurrent(error, session, ai.zara.app.telemetry.ZaraOperation.STREAM)
            voiceStreamFailureObserver?.invoke(error)
            return
        }
        if (voicePumpActive && !closed && session != null && dealer != null) {
            executor.execute(::pollVoiceEvents)
        }
    }

    private fun dispatchVoiceStream(event: VoiceStreamEvent) {
        val current = session ?: throw StaleTextSessionException("voice client is not connected")
        if (event.sessionId != current.sessionId) {
            staleFrameObserver?.invoke(event.javaClass.simpleName, current.generation)
            return
        }
        val negotiated = selectedAudioOutputFormat
        if (event is VoiceStreamEvent.AudioStarted && negotiated != null) {
            if (event.sampleRate != negotiated.sampleRate || event.channels != negotiated.channels) {
                throw ZaraWireException("audio output start does not match negotiated format")
            }
        }
        voiceStreamObserver?.invoke(event)
    }

    private fun replaceDealer(profile: ServerProfile) {
        voicePumpActive = false
        selectedAudioOutputFormat = null
        dealer?.close()
        dealer = null
        val created = dealerFactory.create(profile.endpoint)
        dealer = created
    }

    private fun reportConnectionFailureIfCurrent(
        error: Throwable,
        failingSession: ConnectedTextSession?,
        operation: ai.zara.app.telemetry.ZaraOperation,
    ) {
        val current = failingSession ?: return
        if (closed) return
        val liveSession = session ?: return
        if (liveSession.generation != current.generation || liveSession.sessionId != current.sessionId) return
        protocolTrace.decodeFailed()
        attachProtocolFailureContext(error, protocolTrace.snapshot())
        val failure = ai.zara.app.telemetry.ZaraFailures.classify(
            error,
            operation,
            connectionGeneration = current.generation,
        )
        if (!ai.zara.app.telemetry.ZaraFailures.isSessionDesyncing(failure.code, failure.retryable)) return
        connectionFailureObserver?.invoke(failure)
    }

    private fun sendTraced(
        active: TextDealer,
        frames: List<ByteArray>,
        messageType: String,
        requestId: String,
        sessionId: String?,
    ) {
        active.send(frames)
        protocolTrace.transmitted(messageType, requestId, sessionId, frames)
    }

    private fun receiveMessage(active: TextDealer): TextServerMessage {
        while (true) {
            val frames = active.receive(requestTimeoutMillis)
                ?: throw TextRequestTimeoutException("ZARA/1 response timed out")
            protocolTrace.received(frames)
            try {
                val voiceEvent = ai.zara.app.voice.ZaraVoiceStreamCodec.decode(frames)
                protocolTrace.decodedControl("voice.stream", voiceEvent.sessionId)
                dispatchVoiceStream(voiceEvent)
                continue
            } catch (_: ZaraWireException) {
                if (handleDeviceServerMessage(active, frames)) continue
                return ZaraTextCodec.decode(frames).also(protocolTrace::decoded)
            }
        }
    }

    private fun handleDeviceServerMessage(
        active: TextDealer,
        frames: List<ByteArray>,
    ): Boolean {
        val message = try {
            ZaraDeviceActionCodec.decodeServerMessage(frames)
        } catch (_: ZaraWireException) {
            return false
        }
        protocolTrace.decodedControl("device.action", message.sessionId)
        val current = session ?: throw StaleTextSessionException("device action arrived without a live session")
        if (message.sessionId != current.sessionId) {
            throw StaleTextSessionException("device action belongs to a stale session")
        }
        val handler = deviceActionHandler
            ?: throw ZaraWireException("server sent a device action without an advertised handler")
        when (message) {
            is DeviceServerMessage.Request -> {
                if (message.capability !in deviceCapabilities()) {
                    throw ZaraWireException("server requested an unadvertised device capability")
                }
                if (message.deadlineNs <= epochNanoseconds()) {
                    throw ZaraWireException("device action deadline expired before acceptance")
                }
                if (message.actionId in terminalDeviceActions) {
                    throw ZaraWireException("duplicate terminal device action")
                }
                active.send(
                    ZaraDeviceActionCodec.encodeAccepted(
                        requestId = nextRequestId(),
                        sessionId = current.sessionId,
                        actionId = message.actionId,
                        timestampNs = nextTimestamp(),
                    )
                )
                val result = try {
                    handler.execute(message)
                } catch (_: SecurityException) {
                    DeviceActionResult.Error(DeviceActionErrorCode.PermissionDenied)
                } catch (_: Throwable) {
                    DeviceActionResult.Error(DeviceActionErrorCode.Failed)
                }
                when (result) {
                    DeviceActionResult.Completed -> active.send(
                        ZaraDeviceActionCodec.encodeCompleted(
                            requestId = nextRequestId(),
                            sessionId = current.sessionId,
                            actionId = message.actionId,
                            timestampNs = nextTimestamp(),
                        )
                    )
                    is DeviceActionResult.Error -> active.send(
                        ZaraDeviceActionCodec.encodeError(
                            requestId = nextRequestId(),
                            sessionId = current.sessionId,
                            actionId = message.actionId,
                            code = result.code,
                            message = result.message,
                            timestampNs = nextTimestamp(),
                        )
                    )
                }
                rememberTerminalDeviceAction(message.actionId)
            }
            is DeviceServerMessage.Cancel -> {
                handler.cancel(message)
                rememberTerminalDeviceAction(message.actionId)
            }
        }
        return true
    }

    private fun rememberTerminalDeviceAction(actionId: String) {
        terminalDeviceActions += actionId
        while (terminalDeviceActions.size > MAX_TERMINAL_DEVICE_ACTIONS) {
            val oldest = terminalDeviceActions.firstOrNull() ?: return
            terminalDeviceActions.remove(oldest)
        }
    }

    private fun verifySession(actual: String?, expected: String) {
        if (actual != expected) throw ZaraWireException(
            "message session is stale",
            code = ai.zara.app.telemetry.ZaraFailureCodes.PROTOCOL_STALE_GENERATION,
        )
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
                val result = block()
                protocolTrace.finish()
                future.complete(result)
            } catch (error: Throwable) {
                protocolTrace.decodeFailed()
                attachProtocolFailureContext(error, protocolTrace.snapshot())
                protocolTrace.finish()
                future.completeExceptionally(error)
            }
        }
        return future
    }
}
