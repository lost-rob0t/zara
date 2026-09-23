package ai.zara.app.auth

import java.io.File
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.Instant
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class PairingGenerationFenceTest {
    private val clientKey = "b".repeat(40)
    private val serverKey = "a".repeat(40)

    @Test
    fun `superseded reserved pairing cannot create identity or commit trust`() {
        val directory = Files.createTempDirectory("zara-pairing-generation").toFile()
        val publicKey = JeroMqCurveKeyCodec.decode(clientKey)
        val secretKey = ByteArray(32) { (it + 41).toByte() }
        val generator = FenceCountingGenerator(publicKey, secretKey)
        val repository = EnrollmentRepository(
            credentials = WrappedCredentialStore(
                File(directory, "credential.bin"),
                FenceXorCipher(0x27),
            ),
            serverPins = ServerPinStore(File(directory, "pin.bin")),
            generator = generator,
        )
        val client = PairingClient(repository, connectTimeoutMs = 100)
        val staleGeneration = client.reservePairing()
        val currentGeneration = client.reservePairing()
        val payload = "zara://pair/v1" +
            "?broker_host=127.0.0.1" +
            "&broker_port=9" +
            "&endpoint=${encode("tcp://127.0.0.1:7731")}" +
            "&server_key=${encode(serverKey)}" +
            "&token=${encode("pairing-token")}" +
            "&expires=${Instant.now().epochSecond + 60}"

        val error = assertThrows(PairingException::class.java) {
            client.pairReserved(staleGeneration, payload)
        }

        assertTrue(error.message!!.contains("cancelled"))
        assertEquals(0, generator.calls)
        assertEquals(null, repository.pinnedServerPublicKeyZ85())
        assertTrue(currentGeneration > staleGeneration)
        client.close()
    }

    @Test
    fun `coordinator reserves client generation before queued pairing work`() {
        val source = File("src/main/java/ai/zara/app/AndroidPairingCoordinator.kt").readText()
        val reservation = source.indexOf("pairingClient.reservePairing()")
        val queue = source.indexOf("executor.execute")
        val reservedCall = source.indexOf("pairingClient.pairReserved(")

        assertTrue(reservation >= 0)
        assertTrue(queue > reservation)
        assertTrue(reservedCall > queue)
    }

    private fun encode(value: String): String =
        URLEncoder.encode(value, StandardCharsets.UTF_8.name())
}

private class FenceCountingGenerator(
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

private class FenceXorCipher(private val key: Int) : CredentialCipher {
    override fun seal(plaintext: ByteArray): SealedCredential = SealedCredential(
        iv = ByteArray(12) { it.toByte() },
        ciphertext = plaintext.map { (it.toInt() xor key).toByte() }.toByteArray(),
    )

    override fun open(sealed: SealedCredential): ByteArray =
        sealed.ciphertext.map { (it.toInt() xor key).toByte() }.toByteArray()
}
