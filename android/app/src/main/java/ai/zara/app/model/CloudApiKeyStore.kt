package ai.zara.app.model

import ai.zara.app.auth.CredentialCipher
import ai.zara.app.auth.SealedCredential
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

class CloudApiKeyStore(
    private val file: File,
    private val cipher: CredentialCipher,
) {
    fun save(apiKey: String) {
        val normalized = apiKey.trim()
        require(normalized.isNotEmpty()) { "API key is required" }
        require(normalized.length <= MAX_API_KEY_CHARS) { "API key is too large" }
        require(normalized.none { it == '\r' || it == '\n' }) { "API key contains invalid characters" }
        val plaintext = normalized.encodeToByteArray()
        val sealed = try {
            cipher.seal(plaintext)
        } finally {
            plaintext.fill(0)
        }
        val parent = file.parentFile
        check(parent == null || parent.mkdirs() || parent.isDirectory) {
            "Cloud model credential directory is unavailable"
        }
        val temporary = File(parent, ".${file.name}.tmp")
        try {
            FileOutputStream(temporary).use { raw ->
                val output = DataOutputStream(BufferedOutputStream(raw))
                output.writeInt(MAGIC)
                output.writeInt(VERSION)
                output.writeInt(sealed.iv.size)
                output.write(sealed.iv)
                output.writeInt(sealed.ciphertext.size)
                output.write(sealed.ciphertext)
                output.flush()
                raw.fd.sync()
            }
            atomicReplace(temporary, file)
        } finally {
            temporary.delete()
        }
    }

    fun load(): String? {
        if (!file.isFile) return null
        DataInputStream(BufferedInputStream(FileInputStream(file))).use { input ->
            require(input.readInt() == MAGIC) { "Invalid cloud API credential" }
            require(input.readInt() == VERSION) { "Unsupported cloud API credential version" }
            val iv = readBounded(input, 1, MAX_IV_BYTES, "IV")
            val ciphertext = readBounded(input, 1, MAX_CIPHERTEXT_BYTES, "ciphertext")
            require(input.read() == -1) { "Trailing cloud API credential data" }
            val plaintext = cipher.open(SealedCredential(iv, ciphertext))
            return try {
                val value = plaintext.toString(Charsets.UTF_8)
                require(value.isNotBlank() && value.length <= MAX_API_KEY_CHARS) {
                    "Cloud API credential is invalid"
                }
                value
            } finally {
                plaintext.fill(0)
            }
        }
    }

    fun clear(): Boolean = !file.exists() || file.delete()

    fun exists(): Boolean = file.isFile

    private fun readBounded(
        input: DataInputStream,
        minimum: Int,
        maximum: Int,
        label: String,
    ): ByteArray {
        val length = input.readInt()
        require(length in minimum..maximum) { "$label has invalid size" }
        return ByteArray(length).also(input::readFully)
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

    companion object {
        private const val MAGIC = 0x5A414931
        private const val VERSION = 1
        private const val MAX_API_KEY_CHARS = 4_096
        private const val MAX_IV_BYTES = 64
        private const val MAX_CIPHERTEXT_BYTES = 8_192
    }
}
