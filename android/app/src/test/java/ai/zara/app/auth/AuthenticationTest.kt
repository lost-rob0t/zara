package ai.zara.app.auth

import java.io.File
import java.nio.file.Files
import java.security.MessageDigest
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AuthenticationTest {

    @Test fun `wrapped credential round trips without persisting plaintext secret`() {
        val directory = Files.createTempDirectory("zara-auth").toFile()
        val file = File(directory, "curve-credential.bin")
        val cipher = TaggedCipher(0x31)
        val store = WrappedCredentialStore(file, cipher)
        val credential = CurveCredential(
            publicKey = ByteArray(32) { (it + 1).toByte() },
            secretKey = ByteArray(32) { (0x60 + it).toByte() },
        )

        store.save(credential)

        val persisted = file.readBytes()
        assertFalse(persisted.containsSequence(credential.secretKey))
        val loaded = store.load()
        assertTrue(loaded is CredentialLoadResult.Ready)
        loaded as CredentialLoadResult.Ready
        assertArrayEquals(credential.publicKey, loaded.credential.publicKey)
        assertArrayEquals(credential.secretKey, loaded.credential.secretKey)
    }

    @Test fun `wrong wrapping key reports corrupt instead of silently replacing identity`() {
        val directory = Files.createTempDirectory("zara-auth-corrupt").toFile()
        val file = File(directory, "curve-credential.bin")
        val credential = CurveCredential(ByteArray(32) { 7 }, ByteArray(32) { 11 })
        WrappedCredentialStore(file, TaggedCipher(0x22)).save(credential)

        val loaded = WrappedCredentialStore(file, TaggedCipher(0x55)).load()

        assertTrue(loaded is CredentialLoadResult.Corrupt)
    }

    @Test fun `missing credential is explicitly unenrolled`() {
        val directory = Files.createTempDirectory("zara-auth-missing").toFile()
        val loaded = WrappedCredentialStore(
            File(directory, "curve-credential.bin"),
            TaggedCipher(0x44),
        ).load()

        assertTrue(loaded is CredentialLoadResult.Unenrolled)
    }

    @Test fun `curve keys and server pins require exactly 32 bytes`() {
        expectIllegalArgument { CurveCredential(ByteArray(31), ByteArray(32)) }
        expectIllegalArgument { CurveCredential(ByteArray(32), ByteArray(33)) }
        expectIllegalArgument { ServerPin(ByteArray(31)) }
        ServerPin(ByteArray(32))
    }

    @Test fun `server pin comparison is exact and constant contract`() {
        val expected = ByteArray(32) { it.toByte() }
        val pin = ServerPin(expected)
        val wrong = expected.copyOf().also { it[31] = (it[31].toInt() xor 1).toByte() }

        assertTrue(pin.matches(expected.copyOf()))
        assertFalse(pin.matches(wrong))
    }

    @Test fun `auth configurator applies client identity and pinned server key`() {
        val publicKey = ByteArray(32) { (it + 3).toByte() }
        val secretKey = ByteArray(32) { (it + 71).toByte() }
        val serverKey = ByteArray(32) { (it + 101).toByte() }
        val socket = RecordingCurveSocket()

        CurveAuthConfigurator().configure(
            socket = socket,
            credential = CurveCredential(publicKey, secretKey),
            serverPin = ServerPin(serverKey),
        )

        assertArrayEquals(serverKey, socket.serverKey)
        assertArrayEquals(publicKey, socket.publicKey)
        assertArrayEquals(secretKey, socket.secretKey)
    }

    @Test fun `auth configurator fails closed when socket rejects any curve option`() {
        val credential = CurveCredential(ByteArray(32) { 1 }, ByteArray(32) { 2 })
        val pin = ServerPin(ByteArray(32) { 3 })

        listOf(Reject.SERVER, Reject.PUBLIC, Reject.SECRET).forEach { rejection ->
            val socket = RecordingCurveSocket(reject = rejection)
            var failed = false
            try {
                CurveAuthConfigurator().configure(socket, credential, pin)
            } catch (_: AuthenticationException) {
                failed = true
            }
            assertTrue("$rejection must fail authentication", failed)
        }
    }

    private fun expectIllegalArgument(block: () -> Unit) {
        var failed = false
        try {
            block()
        } catch (_: IllegalArgumentException) {
            failed = true
        }
        assertTrue(failed)
    }
}

private class TaggedCipher(private val key: Int) : CredentialCipher {
    override fun seal(plaintext: ByteArray): SealedCredential {
        val iv = ByteArray(12) { (it + key).toByte() }
        val body = plaintext.map { (it.toInt() xor key).toByte() }.toByteArray()
        val tag = digest(byteArrayOf(key.toByte()) + plaintext).copyOfRange(0, 16)
        return SealedCredential(iv, body + tag)
    }

    override fun open(sealed: SealedCredential): ByteArray {
        require(sealed.ciphertext.size >= 16)
        val body = sealed.ciphertext.copyOfRange(0, sealed.ciphertext.size - 16)
        val tag = sealed.ciphertext.copyOfRange(sealed.ciphertext.size - 16, sealed.ciphertext.size)
        val plaintext = body.map { (it.toInt() xor key).toByte() }.toByteArray()
        val expected = digest(byteArrayOf(key.toByte()) + plaintext).copyOfRange(0, 16)
        if (!MessageDigest.isEqual(tag, expected)) {
            throw CredentialCipherException("authentication tag mismatch")
        }
        return plaintext
    }

    private fun digest(value: ByteArray): ByteArray = MessageDigest.getInstance("SHA-256").digest(value)
}

private enum class Reject { NONE, SERVER, PUBLIC, SECRET }

private class RecordingCurveSocket(private val reject: Reject = Reject.NONE) : CurveSocketOptions {
    var serverKey: ByteArray? = null
    var publicKey: ByteArray? = null
    var secretKey: ByteArray? = null

    override fun setServerKey(key: ByteArray): Boolean {
        if (reject == Reject.SERVER) return false
        serverKey = key.copyOf()
        return true
    }

    override fun setPublicKey(key: ByteArray): Boolean {
        if (reject == Reject.PUBLIC) return false
        publicKey = key.copyOf()
        return true
    }

    override fun setSecretKey(key: ByteArray): Boolean {
        if (reject == Reject.SECRET) return false
        secretKey = key.copyOf()
        return true
    }
}

private fun ByteArray.containsSequence(needle: ByteArray): Boolean {
    if (needle.isEmpty() || needle.size > size) return false
    for (start in 0..size - needle.size) {
        var matches = true
        for (offset in needle.indices) {
            if (this[start + offset] != needle[offset]) {
                matches = false
                break
            }
        }
        if (matches) return true
    }
    return false
}
