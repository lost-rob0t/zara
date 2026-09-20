package ai.zara.app.auth

import org.junit.Assert.assertArrayEquals
import org.junit.Test
import org.zeromq.SocketType
import org.zeromq.ZCert
import org.zeromq.ZContext

class JeroMqCurveSocketAliasingTest {
    @Test
    fun `configured socket retains secret after transient auth buffer is zeroized`() {
        val client = ZCert()
        val server = ZCert()
        val credential = CurveCredential(client.publicKey, client.secretKey)
        val expectedSecret = client.secretKey.copyOf()
        val context = ZContext()
        val socket = context.createSocket(SocketType.DEALER)

        try {
            CurveAuthConfigurator().configure(
                socket = JeroMqCurveSocket(socket),
                credential = credential,
                serverPin = ServerPin(server.publicKey),
            )

            assertArrayEquals(
                "JeroMQ must not retain the transient secret buffer that the auth boundary zeroizes",
                expectedSecret,
                socket.curveSecretKey,
            )
        } finally {
            expectedSecret.fill(0)
            credential.destroy()
            socket.close()
            context.close()
        }
    }
}
