package ai.zara.app.auth

import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.EOFException
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.security.MessageDigest

private const val CURVE_KEY_BYTES = 32
private const val CREDENTIAL_MAGIC = 0x5A435231
private const val CREDENTIAL_VERSION = 1
private const val MAX_IV_BYTES = 64
private const val MAX_CIPHERTEXT_BYTES = 4096

class CredentialCipherException(message: String, cause: Throwable? = null) : Exception(message, cause)

class AuthenticationException(message: String, cause: Throwable? = null) : Exception(message, cause)

data class SealedCredential(
    val iv: ByteArray,
    val ciphertext: ByteArray,
) {
    init {
        require(iv.isNotEmpty() && iv.size <= MAX_IV_BYTES) { "credential IV has invalid size" }
        require(ciphertext.isNotEmpty() && ciphertext.size <= MAX_CIPHERTEXT_BYTES) {
            "credential ciphertext has invalid size"
        }
    }
}

interface CredentialCipher {
    @Throws(CredentialCipherException::class)
    fun seal(plaintext: ByteArray): SealedCredential

    @Throws(CredentialCipherException::class)
    fun open(sealed: SealedCredential): ByteArray
}

class CurveCredential(publicKey: ByteArray, secretKey: ByteArray) {
    val publicKey: ByteArray = publicKey.copyOf()
    val secretKey: ByteArray = secretKey.copyOf()

    init {
        require(this.publicKey.size == CURVE_KEY_BYTES) { "CURVE public key must be 32 bytes" }
        require(this.secretKey.size == CURVE_KEY_BYTES) { "CURVE secret key must be 32 bytes" }
    }
}

sealed interface CredentialLoadResult {
    data object Unenrolled : CredentialLoadResult
    data class Ready(val credential: CurveCredential) : CredentialLoadResult
    data class Corrupt(val reason: String) : CredentialLoadResult
}

class WrappedCredentialStore(
    private val file: File,
    private val cipher: CredentialCipher,
) {
    fun save(credential: CurveCredential) {
        file.parentFile?.mkdirs()
        val secret = credential.secretKey.copyOf()
        val sealed = try {
            cipher.seal(secret)
        } finally {
            secret.fill(0)
        }
        val temp = File(file.parentFile, ".${file.name}.tmp")
        try {
            FileOutputStream(temp).use { raw ->
                DataOutputStream(BufferedOutputStream(raw)).use { output ->
                    output.writeInt(CREDENTIAL_MAGIC)
                    output.writeInt(CREDENTIAL_VERSION)
                    output.writeInt(credential.publicKey.size)
                    output.write(credential.publicKey)
                    output.writeInt(sealed.iv.size)
                    output.write(sealed.iv)
                    output.writeInt(sealed.ciphertext.size)
                    output.write(sealed.ciphertext)
                    output.flush()
                }
                raw.fd.sync()
            }
            atomicReplace(temp, file)
        } finally {
            if (temp.exists()) temp.delete()
        }
    }

    fun load(): CredentialLoadResult {
        if (!file.exists()) return CredentialLoadResult.Unenrolled
        return try {
            val encoded = readEnvelope()
            val secret = cipher.open(encoded.sealed)
            try {
                CredentialLoadResult.Ready(CurveCredential(encoded.publicKey, secret))
            } finally {
                secret.fill(0)
            }
        } catch (error: Exception) {
            CredentialLoadResult.Corrupt(error.message ?: error.javaClass.simpleName)
        }
    }

    fun delete(): Boolean = !file.exists() || file.delete()

    private fun readEnvelope(): EncodedCredential {
        DataInputStream(BufferedInputStream(FileInputStream(file))).use { input ->
            require(input.readInt() == CREDENTIAL_MAGIC) { "invalid credential magic" }
            require(input.readInt() == CREDENTIAL_VERSION) { "unsupported credential version" }
            val publicKey = readBounded(input, CURVE_KEY_BYTES, CURVE_KEY_BYTES, "public key")
            val iv = readBounded(input, 1, MAX_IV_BYTES, "IV")
            val ciphertext = readBounded(input, 1, MAX_CIPHERTEXT_BYTES, "ciphertext")
            require(input.read() == -1) { "trailing credential data" }
            return EncodedCredential(publicKey, SealedCredential(iv, ciphertext))
        }
    }

    private fun readBounded(
        input: DataInputStream,
        minimum: Int,
        maximum: Int,
        label: String,
    ): ByteArray {
        val length = input.readInt()
        require(length in minimum..maximum) { "$label has invalid size" }
        val value = ByteArray(length)
        try {
            input.readFully(value)
        } catch (error: EOFException) {
            value.fill(0)
            throw error
        }
        return value
    }

    private fun atomicReplace(source: File, destination: File) {
        try {
            Files.move(
                source.toPath(),
                destination.toPath(),
                StandardCopyOption.ATOMIC_MOVE,
                StandardCopyOption.REPLACE_EXISTING,
            )
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source.toPath(), destination.toPath(), StandardCopyOption.REPLACE_EXISTING)
        }
    }

    private data class EncodedCredential(
        val publicKey: ByteArray,
        val sealed: SealedCredential,
    )
}

class ServerPin(key: ByteArray) {
    private val key: ByteArray = key.copyOf()

    init {
        require(this.key.size == CURVE_KEY_BYTES) { "CURVE server pin must be 32 bytes" }
    }

    fun bytes(): ByteArray = key.copyOf()

    fun matches(candidate: ByteArray): Boolean =
        candidate.size == CURVE_KEY_BYTES && MessageDigest.isEqual(key, candidate)
}

sealed interface ServerPinLoadResult {
    data object Missing : ServerPinLoadResult
    data class Ready(val pin: ServerPin) : ServerPinLoadResult
    data class Corrupt(val reason: String) : ServerPinLoadResult
}

class ServerPinStore(private val file: File) {
    fun save(pin: ServerPin) {
        file.parentFile?.mkdirs()
        val temp = File(file.parentFile, ".${file.name}.tmp")
        try {
            temp.writeBytes(pin.bytes())
            try {
                Files.move(
                    temp.toPath(),
                    file.toPath(),
                    StandardCopyOption.ATOMIC_MOVE,
                    StandardCopyOption.REPLACE_EXISTING,
                )
            } catch (_: AtomicMoveNotSupportedException) {
                Files.move(temp.toPath(), file.toPath(), StandardCopyOption.REPLACE_EXISTING)
            }
        } finally {
            if (temp.exists()) temp.delete()
        }
    }

    fun load(): ServerPinLoadResult {
        if (!file.exists()) return ServerPinLoadResult.Missing
        return try {
            ServerPinLoadResult.Ready(ServerPin(file.readBytes()))
        } catch (error: Exception) {
            ServerPinLoadResult.Corrupt(error.message ?: error.javaClass.simpleName)
        }
    }

    fun delete(): Boolean = !file.exists() || file.delete()
}

interface CurveSocketOptions {
    fun setServerKey(key: ByteArray): Boolean
    fun setPublicKey(key: ByteArray): Boolean
    fun setSecretKey(key: ByteArray): Boolean
}

class CurveAuthConfigurator {
    fun configure(
        socket: CurveSocketOptions,
        credential: CurveCredential,
        serverPin: ServerPin,
    ) {
        val serverKey = serverPin.bytes()
        val publicKey = credential.publicKey.copyOf()
        val secretKey = credential.secretKey.copyOf()
        try {
            if (!socket.setServerKey(serverKey)) {
                throw AuthenticationException("failed to set pinned CURVE server key")
            }
            if (!socket.setPublicKey(publicKey)) {
                throw AuthenticationException("failed to set CURVE client public key")
            }
            if (!socket.setSecretKey(secretKey)) {
                throw AuthenticationException("failed to set CURVE client secret key")
            }
        } finally {
            secretKey.fill(0)
        }
    }
}
