package ai.zara.app.auth

import org.zeromq.SocketType
import org.zeromq.ZCert
import org.zeromq.ZContext
import org.zeromq.ZMQ

interface CurveCredentialGenerator {
    fun generate(): CurveCredential
}

class JeroMqCurveCredentialGenerator : CurveCredentialGenerator {
    override fun generate(): CurveCredential {
        val certificate = ZCert()
        val publicKey = certificate.publicKey
        val secretKey = certificate.secretKey
        return try {
            CurveCredential(publicKey, secretKey)
        } finally {
            secretKey.fill(0)
        }
    }
}

object JeroMqCurveKeyCodec {
    fun encode(key: ByteArray): String {
        require(key.size == ZMQ.Curve.KEY_SIZE) { "CURVE key must be 32 bytes" }
        return ZMQ.Curve.z85Encode(key)
    }

    fun decode(value: String): ByteArray {
        require(value.length == ZMQ.Curve.KEY_SIZE_Z85) { "CURVE Z85 key must be 40 characters" }
        val decoded = try {
            ZMQ.Curve.z85Decode(value)
        } catch (error: RuntimeException) {
            throw IllegalArgumentException("CURVE key must be valid Z85", error)
        }
        require(decoded.size == ZMQ.Curve.KEY_SIZE) { "CURVE key must decode to 32 bytes" }
        return decoded
    }
}

class JeroMqCurveSocket(
    private val socket: ZMQ.Socket,
) : CurveSocketOptions {
    override fun setServerKey(key: ByteArray): Boolean = socket.setCurveServerKey(key)

    override fun setPublicKey(key: ByteArray): Boolean = socket.setCurvePublicKey(key)

    override fun setSecretKey(key: ByteArray): Boolean = socket.setCurveSecretKey(key)
}

class JeroMqCurveDealerFactory(
    private val enrollment: EnrollmentRepository,
) {
    fun create(context: ZContext): ZMQ.Socket {
        val socket = context.createSocket(SocketType.DEALER)
        try {
            socket.setLinger(0)
            socket.setHandshakeIvl(5_000)
            enrollment.configure(JeroMqCurveSocket(socket))
            return socket
        } catch (error: Exception) {
            socket.close()
            throw error
        }
    }
}
