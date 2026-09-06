package ai.zara.app.auth

import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AuthenticationRevocationAdversarialTest {

    @Test
    fun revoked_client_credential_invalidates_ready_enrollment_and_blocks_socket_auth() {
        val harness = readyHarness("credential-revoked")
        assertTrue(harness.repository.state() is EnrollmentState.Ready)

        assertTrue(harness.credentials.delete())

        assertTrue(harness.repository.state() is EnrollmentState.Unenrolled)
        assertAuthRejectedWithoutSocketMutation(harness.repository)
    }

    @Test
    fun revoked_server_pin_returns_to_awaiting_pin_and_blocks_socket_auth() {
        val harness = readyHarness("pin-revoked")
        assertTrue(harness.repository.state() is EnrollmentState.Ready)

        assertTrue(harness.serverPins.delete())

        val state = harness.repository.state()
        assertTrue(state is EnrollmentState.AwaitingServerPin)
        state as EnrollmentState.AwaitingServerPin
        assertArrayEquals(harness.clientPublicKey, state.publicKey)
        assertAuthRejectedWithoutSocketMutation(harness.repository)
    }

    @Test
    fun corrupted_server_pin_invalidates_ready_enrollment_and_blocks_socket_auth() {
        val harness = readyHarness("pin-corrupt")
        assertTrue(harness.repository.state() is EnrollmentState.Ready)

        harness.serverPinFile.writeBytes(ByteArray(31) { 0x44 })

        assertTrue(harness.repository.state() is EnrollmentState.Corrupt)
        assertAuthRejectedWithoutSocketMutation(harness.repository)
    }

    private fun readyHarness(name: String): RevocationHarness {
        val directory = Files.createTempDirectory("zara-auth-$name").toFile()
        val credentialFile = File(directory, "credential.bin")
        val pinFile = File(directory, "server-pin.bin")
        val clientPublicKey = ByteArray(32) { (it + 1).toByte() }
        val clientSecretKey = ByteArray(32) { (it + 65).toByte() }
        val serverKey = ByteArray(32) { (it + 97).toByte() }
        val credentials = WrappedCredentialStore(credentialFile, XorCredentialCipher(0x5A))
        val pins = ServerPinStore(pinFile)
        val repository = EnrollmentRepository(
            credentials = credentials,
            serverPins = pins,
            generator = StaticCurveCredentialGenerator(clientPublicKey, clientSecretKey),
        )
        assertArrayEquals(clientPublicKey, repository.createIdentity())
        repository.pinServer(serverKey)
        return RevocationHarness(repository, credentials, pins, pinFile, clientPublicKey)
    }

    private fun assertAuthRejectedWithoutSocketMutation(repository: EnrollmentRepository) {
        val socket = MutationTrackingCurveSocket()
        var rejected = false
        try {
            repository.configure(socket)
        } catch (_: AuthenticationException) {
            rejected = true
        }
        assertTrue(rejected)
        assertFalse(socket.mutated)
    }
}

private data class RevocationHarness(
    val repository: EnrollmentRepository,
    val credentials: WrappedCredentialStore,
    val serverPins: ServerPinStore,
    val serverPinFile: File,
    val clientPublicKey: ByteArray,
)

private class XorCredentialCipher(private val mask: Int) : CredentialCipher {
    override fun seal(plaintext: ByteArray): SealedCredential =
        SealedCredential(
            iv = byteArrayOf(1),
            ciphertext = plaintext.map { (it.toInt() xor mask).toByte() }.toByteArray(),
        )

    override fun open(sealed: SealedCredential): ByteArray =
        sealed.ciphertext.map { (it.toInt() xor mask).toByte() }.toByteArray()
}

private class StaticCurveCredentialGenerator(
    private val publicKey: ByteArray,
    private val secretKey: ByteArray,
) : CurveCredentialGenerator {
    override fun generate(): CurveCredential = CurveCredential(publicKey, secretKey)
}

private class MutationTrackingCurveSocket : CurveSocketOptions {
    var mutated = false
        private set

    override fun setServerKey(key: ByteArray): Boolean {
        mutated = true
        return true
    }

    override fun setPublicKey(key: ByteArray): Boolean {
        mutated = true
        return true
    }

    override fun setSecretKey(key: ByteArray): Boolean {
        mutated = true
        return true
    }
}
