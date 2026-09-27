package ai.zara.app.runtime

import ai.zara.app.auth.JeroMqCurveKeyCodec
import ai.zara.app.telemetry.ZaraFailureCodes
import ai.zara.app.voice.VoiceCaptureContext
import ai.zara.app.voice.VoiceStreamEvent
import java.io.File
import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue
import org.junit.Test
import org.zeromq.SocketType
import org.zeromq.ZContext
import org.zeromq.ZMQ

/**
 * End-to-end regression for the incident class from #1299/#1302: a real
 * ZaraTextClientActor over real CURVE ZMQ against the deterministic recovery
 * fixture. Proves successful text + voice turns before failure, typed
 * terminal failures for each injected protocol/transport fault, reconnect
 * recovery, and stale-generation fencing.
 */
class RemoteRecoveryInteropTest {
    private val streamEvents = CopyOnWriteArrayList<String>()
    private val staleFrames = CopyOnWriteArrayList<String>()

    @Test
    fun recoveryMatrixSurvivesVoiceTurnsProtocolFailuresAndReconnects() {
        val fixturePath = System.getenv("ZARA_RECOVERY_FIXTURE")
        assumeTrue("recovery fixture is supplied by scripts/test-android.sh", fixturePath != null)
        val fixture = readFixture(File(requireNotNull(fixturePath)))
        val endpoint = fixture.getValue("endpoint")
        val controlFifo = fixture.getValue("control_fifo")
        val factory = TextDealerFactory { endpoint: String ->
            assertEquals(endpoint, endpoint)
            RecoveryJeroMqDealer(
                endpoint = endpoint,
                serverPublic = fixture.getValue("server_public"),
                clientPublic = fixture.getValue("client_public"),
                clientSecret = fixture.getValue("client_secret"),
            )
        }

        val failures = CopyOnWriteArrayList<ai.zara.app.telemetry.ZaraFailure>()
        val actor = ZaraTextClientActor(
            dealerFactory = factory,
            requestTimeoutMillis = 5_000,
            audioOutputFormats = listOf(AudioOutputFormat.pcmS16leMono(24_000)),
        )
        actor.setConnectionFailureObserver { failure -> failures += failure }
        actor.setVoiceStreamObserver { event -> streamEvents += event.javaClass.simpleName }
        actor.setStaleFrameObserver { messageType, _ -> staleFrames += messageType }

        val generation = 1L
        val session = actor.connect(ServerProfile.create(endpoint), generation).get(20, TimeUnit.SECONDS)

        val firstTurn = actor.submitText(generation, session.sessionId, null, "hello").get(20, TimeUnit.SECONDS)
        assertEquals("stock server response", firstTurn.text)
        assertEquals(0, failures.size)

        val voiceSession = voiceTurn(actor, session.sessionId)
        assertEquals("voice events: ${streamEvents.joinToString()} failures: ${failures.joinToString()}", true, voiceSession)
        assertTrue(streamEvents.contains("SpeechStarted"))
        assertTrue(streamEvents.contains("SpeechEnded"))
        assertTrue(streamEvents.contains("Transcript"))
        assertEquals(0, failures.size)

        arm(controlFifo, "MALFORMED")
        runCatching {
            actor.submitText(generation, session.sessionId, null, "trigger malformed").get(20, TimeUnit.SECONDS)
        }
        val malformedFailure = awaitFailure(failures)
        assertEquals(ZaraFailureCodes.PROTOCOL_MALFORMED, malformedFailure.code)
        assertTrue(streamEvents.contains("SpeechStarted"))

        val generation2 = 2L
        val secondSession = actor.connect(ServerProfile.create(endpoint), generation2).get(20, TimeUnit.SECONDS)
        assertEquals(true, secondSession.sessionId.isNotBlank())
        val secondTurn = actor.submitText(
            generation2,
            secondSession.sessionId,
            null,
            "after malformed recovery",
        ).get(20, TimeUnit.SECONDS)
        assertEquals("stock server response", secondTurn.text)

        arm(controlFifo, "STALE_TEXT")
        val staleDrainedTurn = actor.submitText(
            generation2,
            secondSession.sessionId,
            null,
            "after stale voice lifecycle",
        ).get(20, TimeUnit.SECONDS)
        assertEquals("stock server response", staleDrainedTurn.text)
        assertTrue(staleFrames.contains("TurnCompleted"))
        assertTrue(staleFrames.contains("AssistantResponse"))
        assertEquals(0, failures.size)

        arm(controlFifo, "SLOW_TURN")
        val slowTurn = actor.submitText(
            generation2,
            secondSession.sessionId,
            null,
            "quiet model turn",
        ).get(20, TimeUnit.SECONDS)
        assertEquals("stock server response", slowTurn.text)
        assertEquals(0, failures.size)

        arm(controlFifo, "OUT_OF_ORDER")
        runCatching {
            actor.submitText(generation2, secondSession.sessionId, null, "trigger out of order").get(20, TimeUnit.SECONDS)
        }
        val orderFailure = awaitFailureAfter(failures, malformedFailure)
        assertEquals(ZaraFailureCodes.PROTOCOL_UNEXPECTED_MESSAGE, orderFailure.code)

        val generation3 = 3L
        val thirdSession = actor.connect(ServerProfile.create(endpoint), generation3).get(20, TimeUnit.SECONDS)

        arm(controlFifo, "STALE")
        val staleVoice = voiceTurn(actor, thirdSession.sessionId)
        assertEquals(true, staleVoice)
        assertTrue(streamEvents.count { it == "Transcript" } >= 2)

        arm(controlFifo, "CLOSE")
        runCatching {
            actor.submitText(generation3, thirdSession.sessionId, null, "trigger close").get(20, TimeUnit.SECONDS)
        }
        val closeFailure = awaitFailureAfter(failures, orderFailure)
        assertEquals(ZaraFailureCodes.TRANSPORT_TIMEOUT, closeFailure.code)

        val generation4 = 4L
        val fourthSession = actor.connect(ServerProfile.create(endpoint), generation4).get(20, TimeUnit.SECONDS)
        val finalTurn = actor.submitText(
            generation4,
            fourthSession.sessionId,
            null,
            "final recovery turn",
        ).get(20, TimeUnit.SECONDS)
        assertEquals("stock server response", finalTurn.text)
        actor.close()
    }

    @Test
    fun versionMismatchFailsHelloWithTypedCode() {
        val fixturePath = System.getenv("ZARA_RECOVERY_FIXTURE")
        assumeTrue("recovery fixture is supplied by scripts/test-android.sh", fixturePath != null)
        val fixture = readFixture(File(requireNotNull(fixturePath)))
        val endpoint = fixture.getValue("endpoint")
        arm(fixture.getValue("control_fifo"), "VERSION_MISMATCH")

        val actor = ZaraTextClientActor(
            dealerFactory = { RecoveryJeroMqDealer(endpoint, fixture.getValue("server_public"), fixture.getValue("client_public"), fixture.getValue("client_secret")) },
            requestTimeoutMillis = 5_000,
        )
        try {
            val error = runCatching {
                actor.connect(ServerProfile.create(endpoint), 1).get(20, TimeUnit.SECONDS)
            }.exceptionOrNull() ?: error("expected hello to fail")
            val root = generateSequence(error) { it.cause?.takeIf { c -> c !== it } }.last()
            val wire = root as? ZaraWireException ?: error("expected ZaraWireException, got $root")
            assertEquals(ZaraFailureCodes.PROTOCOL_VERSION_MISMATCH, wire.code)
        } finally {
            actor.close()
        }
    }

    private fun voiceTurn(actor: ZaraTextClientActor, sessionId: String): Boolean {
        val before = streamEvents.size
        val context = VoiceCaptureContext(sessionId, "recovery-conversation", "mic-interop")
        actor.startVoice(context).get(20, TimeUnit.SECONDS)
        actor.sendVoiceChunk(context, 0, ByteArray(1024)).get(20, TimeUnit.SECONDS)
        actor.commitVoice(context).get(20, TimeUnit.SECONDS)
        val deadline = System.currentTimeMillis() + 5_000
        fun recent(): List<String> = streamEvents.toList().drop(before)
        while (!recent().contains("AudioDone") && System.currentTimeMillis() < deadline) {
            Thread.sleep(20)
        }
        return recent().contains("AudioDone")
    }

    private fun awaitFailure(failures: List<ai.zara.app.telemetry.ZaraFailure>): ai.zara.app.telemetry.ZaraFailure {
        val deadline = System.currentTimeMillis() + 5_000
        while (failures.isEmpty() && System.currentTimeMillis() < deadline) Thread.sleep(20)
        assertTrue("expected a typed connection failure", failures.isNotEmpty())
        return failures.last()
    }

    private fun awaitFailureAfter(
        failures: List<ai.zara.app.telemetry.ZaraFailure>,
        previous: ai.zara.app.telemetry.ZaraFailure,
    ): ai.zara.app.telemetry.ZaraFailure {
        val deadline = System.currentTimeMillis() + 5_000
        while ((!failures.isNotEmpty() || failures.last() === previous) && System.currentTimeMillis() < deadline) {
            Thread.sleep(20)
        }
        assertTrue("expected a new typed connection failure", failures.isNotEmpty() && failures.last() !== previous)
        return failures.last()
    }

    private fun arm(controlFifo: String, mode: String) {
        java.io.FileOutputStream(controlFifo).use { stream ->
            stream.write("ARM $mode\n".toByteArray())
            stream.flush()
        }
        Thread.sleep(150)
    }

    private fun readFixture(file: File): Map<String, String> {
        assertTrue("recovery fixture file must be owner-readable", file.isFile)
        val values = file.readLines()
            .filter(String::isNotBlank)
            .associate { line ->
                val separator = line.indexOf('=')
                require(separator > 0) { "invalid recovery fixture entry" }
                line.substring(0, separator) to line.substring(separator + 1)
            }
        assertEquals(
            setOf("endpoint", "server_public", "client_public", "client_secret", "control_fifo"),
            values.keys,
        )
        return values
    }
}

private class RecoveryJeroMqDealer(
    endpoint: String,
    serverPublic: String,
    clientPublic: String,
    clientSecret: String,
) : TextDealer {
    private val context = ZContext()
    private val socket: ZMQ.Socket = context.createSocket(SocketType.DEALER)
    private var closed = false

    init {
        socket.setLinger(0)
        socket.setHandshakeIvl(2_000)
        check(socket.setImmediate(true))
        check(socket.setHeartbeatIvl(100))
        check(socket.setHeartbeatTimeout(5_000))
        check(socket.setSendTimeOut(2_000))
        check(socket.setCurveServerKey(JeroMqCurveKeyCodec.decode(serverPublic)))
        check(socket.setCurvePublicKey(JeroMqCurveKeyCodec.decode(clientPublic)))
        check(socket.setCurveSecretKey(JeroMqCurveKeyCodec.decode(clientSecret)))
        check(socket.connect(endpoint)) { "JeroMQ could not connect to the recovery fixture" }
    }

    override fun send(frames: List<ByteArray>) {
        check(!closed) { "dealer is closed" }
        frames.forEachIndexed { index, frame ->
            val flags = if (index == frames.lastIndex) 0 else ZMQ.SNDMORE
            check(socket.send(frame, flags)) { "JeroMQ failed to send ZARA/1 frame" }
        }
    }

    override fun receive(timeoutMillis: Int): List<ByteArray>? {
        check(!closed) { "dealer is closed" }
        socket.receiveTimeOut = timeoutMillis
        val first = socket.recv(0) ?: return null
        val frames = mutableListOf(first)
        while (socket.hasReceiveMore()) {
            frames += socket.recv(0) ?: error("truncated ZARA/1 multipart")
            require(frames.size <= 18) { "ZARA/1 multipart exceeds frame limit" }
        }
        return frames
    }

    override fun close() {
        if (closed) return
        closed = true
        socket.close()
        context.close()
    }
}
