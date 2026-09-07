package ai.zara.app.runtime

import ai.zara.app.auth.JeroMqCurveKeyCodec
import ai.zara.app.voice.ManualVoiceCapture
import ai.zara.app.voice.VoiceCaptureContext
import java.io.File
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue
import org.junit.Test
import org.zeromq.SocketType
import org.zeromq.ZContext
import org.zeromq.ZMQ

class StockZaraServerVoiceInteropTest {
    @Test
    fun actualJeroMqActorAuthenticatesAndCompletesVoiceInputLifecycleAgainstStockServer() {
        val fixturePath = System.getenv("ZARA_STOCK_FIXTURE")
        assumeTrue("stock ZaraServer fixture is supplied by scripts/test-android.sh", fixturePath != null)
        val fixture = readFixture(File(requireNotNull(fixturePath)))
        val factory = TextDealerFactory { endpoint ->
            assertEquals(fixture.getValue("endpoint"), endpoint)
            StockVoiceJeroMqDealer(
                endpoint = endpoint,
                serverPublic = fixture.getValue("server_public"),
                clientPublic = fixture.getValue("client_public"),
                clientSecret = fixture.getValue("client_secret"),
            )
        }
        val actor = ZaraTextClientActor(factory, requestTimeoutMillis = 2_000)
        try {
            val session = actor.connect(
                ServerProfile.create(fixture.getValue("endpoint")),
                generation = 1,
            ).get(5, TimeUnit.SECONDS)
            assertTrue(session.sessionId.isNotBlank())

            val committed = VoiceCaptureContext(
                sessionId = session.sessionId,
                conversationId = null,
                streamId = "android-stock-mic-commit",
            )
            val pcm = ByteArray(ManualVoiceCapture.PCM_FRAME_BYTES)
            actor.startVoice(committed).get(5, TimeUnit.SECONDS)
            actor.sendVoiceChunk(committed, 0, pcm).get(5, TimeUnit.SECONDS)
            actor.commitVoice(committed).get(5, TimeUnit.SECONDS)

            val cancelled = committed.copy(streamId = "android-stock-mic-cancel")
            actor.startVoice(cancelled).get(5, TimeUnit.SECONDS)
            actor.cancelVoice(cancelled).get(5, TimeUnit.SECONDS)
        } finally {
            actor.close()
        }
    }

    private fun readFixture(file: File): Map<String, String> {
        assertTrue("stock fixture file must be owner-readable", file.isFile)
        return file.readLines()
            .filter(String::isNotBlank)
            .associate { line ->
                val separator = line.indexOf('=')
                require(separator > 0) { "invalid stock fixture entry" }
                line.substring(0, separator) to line.substring(separator + 1)
            }
    }
}

private class StockVoiceJeroMqDealer(
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
        check(socket.setCurveServerKey(JeroMqCurveKeyCodec.decode(serverPublic)))
        check(socket.setCurvePublicKey(JeroMqCurveKeyCodec.decode(clientPublic)))
        check(socket.setCurveSecretKey(JeroMqCurveKeyCodec.decode(clientSecret)))
        check(socket.connect(endpoint)) { "JeroMQ could not connect to stock ZaraServer" }
    }

    override fun send(frames: List<ByteArray>) {
        check(!closed) { "dealer is closed" }
        require(frames.isNotEmpty()) { "ZARA/1 frames are required" }
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
