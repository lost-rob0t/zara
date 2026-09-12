package ai.zara.app.auth

import java.io.File
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class PairingTest {
    private val serverKey = "a".repeat(40)
    private val clientKey = "b".repeat(40)

    @Test fun `qr payload parses strict zara pairing uri`() {
        val endpoint = "tcp://10.20.30.40:7731"
        val raw = "zara://pair/v1" +
            "?broker_host=10.20.30.40" +
            "&broker_port=45231" +
            "&endpoint=${encode(endpoint)}" +
            "&server_key=${encode(serverKey)}" +
            "&token=${encode("token-123")}" +
            "&expires=2000"

        val payload = PairingPayload.parse(raw, nowEpochSeconds = 1900)

        assertEquals("10.20.30.40", payload.brokerHost)
        assertEquals(45231, payload.brokerPort)
        assertEquals(endpoint, payload.endpoint)
        assertEquals(serverKey, payload.serverKey)
        assertEquals("token-123", payload.token)
        assertEquals(2000L, payload.expiresAtEpochSeconds)
    }

    @Test fun `qr payload rejects expired duplicate unknown and non tcp bootstrap data`() {
        val base = "zara://pair/v1?broker_host=host&broker_port=45231" +
            "&endpoint=${encode("tcp://host:7731")}" +
            "&server_key=${encode(serverKey)}&token=abc&expires=2000"

        expectFailure { PairingPayload.parse(base, nowEpochSeconds = 2001) }
        expectFailure { PairingPayload.parse("$base&token=other", nowEpochSeconds = 1900) }
        expectFailure { PairingPayload.parse("$base&extra=nope", nowEpochSeconds = 1900) }
        expectFailure {
            PairingPayload.parse(
                base.replace(encode("tcp://host:7731"), encode("ipc:///tmp/zara.sock")),
                nowEpochSeconds = 1900,
            )
        }
    }

    @Test fun `verification code and automatic device id match server contract`() {
        assertEquals("android-" + sha256Prefix(clientKey), PairingProtocol.deriveDeviceId(clientKey))
        val code = PairingProtocol.verificationCode("pairing-token", clientKey)
        assertEquals(6, code.length)
        assertTrue(code.all(Char::isDigit))
        assertEquals(code, PairingProtocol.verificationCode("pairing-token", clientKey))
        assertTrue(code != PairingProtocol.verificationCode("pairing-token", "c".repeat(40)))
    }

    @Test fun `pairing reuses existing private identity instead of rotating it`() {
        val directory = Files.createTempDirectory("zara-pairing-identity").toFile()
        val publicKey = JeroMqCurveKeyCodec.decode(clientKey)
        val secretKey = ByteArray(32) { (it + 17).toByte() }
        val generator = CountingGenerator(publicKey, secretKey)
        val repository = EnrollmentRepository(
            credentials = WrappedCredentialStore(
                File(directory, "credential.bin"),
                XorCipher(0x5A),
            ),
            serverPins = ServerPinStore(File(directory, "pin.bin")),
            generator = generator,
        )

        assertEquals(clientKey, repository.identityZ85OrCreate())
        assertEquals(clientKey, repository.identityZ85OrCreate())
        assertEquals(1, generator.calls)
    }

    private fun encode(value: String): String =
        URLEncoder.encode(value, StandardCharsets.UTF_8.name())

    private fun expectFailure(block: () -> Unit) {
        var failed = false
        try {
            block()
        } catch (_: IllegalArgumentException) {
            failed = true
        } catch (_: AuthenticationException) {
            failed = true
        }
        assertTrue(failed)
    }

    private fun sha256Prefix(value: String): String {
        val digest = java.security.MessageDigest.getInstance("SHA-256")
            .digest(value.toByteArray(StandardCharsets.US_ASCII))
        return digest.take(6).joinToString("") { "%02x".format(it.toInt() and 0xff) }
    }
}

private class CountingGenerator(
    private val publicKey: ByteArray,
    private val secretKey: ByteArray,
) : CurveCredentialGenerator {
    var calls: Int = 0
        private set

    override fun generate(): CurveCredential {
        calls += 1
        return CurveCredential(publicKey, secretKey)
    }
}

private class XorCipher(private val key: Int) : CredentialCipher {
    override fun seal(plaintext: ByteArray): SealedCredential = SealedCredential(
        iv = ByteArray(12) { it.toByte() },
        ciphertext = plaintext.map { (it.toInt() xor key).toByte() }.toByteArray(),
    )

    override fun open(sealed: SealedCredential): ByteArray =
        sealed.ciphertext.map { (it.toInt() xor key).toByte() }.toByteArray()
}
