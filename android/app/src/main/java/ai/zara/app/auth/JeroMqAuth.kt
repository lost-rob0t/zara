package ai.zara.app.auth

import org.zeromq.ZCert
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

class JeroMqCurveSocket(
    private val socket: ZMQ.Socket,
) : CurveSocketOptions {
    override fun setServerKey(key: ByteArray): Boolean = socket.setCurveServerKey(key)

    override fun setPublicKey(key: ByteArray): Boolean = socket.setCurvePublicKey(key)

    override fun setSecretKey(key: ByteArray): Boolean = socket.setCurveSecretKey(key)
}
