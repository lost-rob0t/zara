package ai.zara.app.runtime

import ai.zara.app.auth.JeroMqCurveKeyCodec
import java.io.File
import java.net.Socket
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue
import org.junit.Test
import org.zeromq.SocketType
import org.zeromq.ZContext
import org.zeromq.ZMQ

class StockZaraServerInteropTest {
    @Test
    fun actualJeroMqActorAuthenticatesAndCompletesTextTurnAgainstStockServer() {
        val fixturePath = System.getenv("ZARA_STOCK_FIXTURE")
        assumeTrue("stock ZaraServer fixture is supplied by scripts/test-android.sh", fixturePath != null)
        val fixture = readFixture(File(requireNotNull(fixturePath)))
        val factory = TextDealerFactory { endpoint ->
            assertEquals(fixture.getValue("endpoint"), endpoint)
            FixtureJeroMqDealer(
                endpoint = endpoint,
                serverPublic = fixture.getValue("server_public"),
                clientPublic = fixture.getValue("client_public"),
                clientSecret = fixture.getValue("client_secret"),
                acceptanceHost = fixture.getValue("acceptance_host"),
                acceptancePort = fixture.getValue("acceptance_port").toInt(),
            )
        }
        val actor = ZaraTextClientActor(factory, requestTimeoutMillis = 2_000)
        try {
            val session = actor.connect(
                ServerProfile.create(fixture.getValue("endpoint")),
                generation = 1,
            ).get(5, TimeUnit.SECONDS)
            assertTrue(session.sessionId.isNotBlank())

            val result = actor.submitText(
                generation = 1,
                sessionId = session.sessionId,
                conversationId = null,
                text = "hello from Android",
            ).get(5, TimeUnit.SECONDS)

            assertEquals("stock server response", result.text)
            assertEquals(true, result.success)
            assertTrue(result.turnId.startsWith("android-stock-turn-"))
        } finally {
            actor.close()
        }
    }

    private fun readFixture(file: File): Map<String, String> {
        assertTrue("stock fixture file must be owner-readable", file.isFile)
        val values = file.readLines()
            .filter(String::isNotBlank)
            .associate { line ->
                val separator = line.indexOf('=')
                require(separator > 0) { "invalid stock fixture entry" }
                line.substring(0, separator) to line.substring(separator + 1)
            }
        assertEquals(
            setOf(
                "endpoint",
                "server_public",
                "client_public",
                "client_secret",
                "acceptance_host",
                "acceptance_port",
            ),
            values.keys,
        )
        return values
    }
}

private class FixtureJeroMqDealer(
    endpoint: String,
    serverPublic: String,
    clientPublic: String,
    clientSecret: String,
    private val acceptanceHost: String,
    private val acceptancePort: Int,
) : TextDealer {
    private val context = ZContext()
    private val socket: ZMQ.Socket = context.createSocket(SocketType.DEALER)
    private var acceptanceSignalled = false
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
        if (!acceptanceSignalled && ZaraTextCodec.decode(frames) is TextServerMessage.TurnAccepted) {
            Socket(acceptanceHost, acceptancePort).use { barrier ->
                barrier.getOutputStream().write('A'.code)
                barrier.getOutputStream().flush()
            }
            acceptanceSignalled = true
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
