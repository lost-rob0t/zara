package ai.zara.app.samsunghealth

import ai.zara.app.auth.AndroidKeystoreCredentialCipher
import ai.zara.app.auth.CredentialCipher
import ai.zara.app.auth.SealedCredential
import ai.zara.ui.health.HealthGoalMetric
import ai.zara.ui.health.HealthGoalTarget
import android.content.Context
import java.io.BufferedInputStream
import java.io.BufferedOutputStream
import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.EOFException
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

class HealthGoalStoreException(message: String, cause: Throwable? = null) : Exception(message, cause)

class HealthGoalStore(
    private val file: File,
    private val cipher: CredentialCipher,
) {
    fun loadOrDefaults(): List<HealthGoalTarget> =
        if (file.exists()) load() else HealthGoalMetric.entries.map { HealthGoalTarget(it, it.defaultTarget) }

    fun load(): List<HealthGoalTarget> {
        if (!file.exists()) return emptyList()
        try {
            DataInputStream(BufferedInputStream(FileInputStream(file))).use { input ->
                require(input.readInt() == ENVELOPE_MAGIC) { "invalid health goal envelope" }
                require(input.readInt() == VERSION) { "unsupported health goal version" }
                val iv = input.readBounded(MAX_IV_BYTES, "IV")
                val ciphertext = input.readBounded(MAX_CIPHERTEXT_BYTES, "ciphertext")
                require(input.read() == -1) { "trailing health goal envelope data" }
                val plaintext = cipher.open(SealedCredential(iv, ciphertext))
                try {
                    return decode(plaintext)
                } finally {
                    plaintext.fill(0)
                }
            }
        } catch (error: Exception) {
            throw HealthGoalStoreException("Health goals could not be unlocked", error)
        }
    }

    fun save(goals: List<HealthGoalTarget>) {
        val normalized = normalize(goals)
        val plaintext = encode(normalized)
        val sealed = try {
            cipher.seal(plaintext)
        } finally {
            plaintext.fill(0)
        }
        file.parentFile?.mkdirs()
        val temp = File(file.parentFile, ".${file.name}.tmp")
        try {
            FileOutputStream(temp).use { raw ->
                DataOutputStream(BufferedOutputStream(raw)).use { output ->
                    output.writeInt(ENVELOPE_MAGIC)
                    output.writeInt(VERSION)
                    output.writeInt(sealed.iv.size)
                    output.write(sealed.iv)
                    output.writeInt(sealed.ciphertext.size)
                    output.write(sealed.ciphertext)
                    output.flush()
                    raw.fd.sync()
                }
            }
            atomicReplace(temp, file)
        } catch (error: Exception) {
            throw HealthGoalStoreException("Health goals could not be secured", error)
        } finally {
            temp.delete()
        }
    }

    private fun encode(goals: List<HealthGoalTarget>): ByteArray {
        val bytes = ByteArrayOutputStream()
        DataOutputStream(bytes).use { output ->
            output.writeInt(DATA_MAGIC)
            output.writeInt(VERSION)
            output.writeInt(goals.size)
            goals.forEach { goal ->
                output.writeUTF(goal.metric.atom)
                output.writeInt(goal.target)
            }
        }
        return bytes.toByteArray()
    }

    private fun decode(bytes: ByteArray): List<HealthGoalTarget> {
        DataInputStream(ByteArrayInputStream(bytes)).use { input ->
            require(input.readInt() == DATA_MAGIC) { "invalid health goal data" }
            require(input.readInt() == VERSION) { "unsupported health goal data version" }
            val count = input.readInt()
            require(count in 0..HealthGoalMetric.entries.size) { "health goal count is invalid" }
            val goals = buildList {
                repeat(count) {
                    val atom = input.readUTF()
                    val metric = requireNotNull(HealthGoalMetric.fromAtom(atom)) { "unknown health goal metric" }
                    add(HealthGoalTarget(metric, input.readInt()))
                }
            }
            require(input.read() == -1) { "trailing health goal data" }
            return normalize(goals)
        }
    }

    private fun normalize(goals: List<HealthGoalTarget>): List<HealthGoalTarget> {
        require(goals.size <= HealthGoalMetric.entries.size) { "too many health goals" }
        val byMetric = goals.associateBy { it.metric }
        require(byMetric.size == goals.size) { "duplicate health goal metric" }
        return HealthGoalMetric.entries.mapNotNull(byMetric::get)
    }

    private fun DataInputStream.readBounded(maximum: Int, label: String): ByteArray {
        val size = readInt()
        require(size in 1..maximum) { "$label has invalid size" }
        return ByteArray(size).also { value ->
            try {
                readFully(value)
            } catch (error: EOFException) {
                value.fill(0)
                throw error
            }
        }
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
        private const val ENVELOPE_MAGIC = 0x5A484731
        private const val DATA_MAGIC = 0x5A484744
        private const val VERSION = 1
        private const val MAX_IV_BYTES = 64
        private const val MAX_CIPHERTEXT_BYTES = 4096

        fun create(context: Context): HealthGoalStore = HealthGoalStore(
            File(context.noBackupFilesDir, "zara/health/goals.bin"),
            AndroidKeystoreCredentialCipher(alias = "zara.health.goals.v1"),
        )
    }
}
