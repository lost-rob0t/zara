package ai.zara.app.samsunghealth

import android.content.Context
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

class HealthOpenPgpRecipientStoreException(message: String, cause: Throwable? = null) : Exception(message, cause)

class HealthOpenPgpRecipientStore(
    private val file: File,
    private val keyIds: (ByteArray) -> Set<Long> = OpenPgpHealthExporter()::recipientKeyIds,
) {
    fun load(): List<ByteArray> {
        if (!file.exists()) return emptyList()
        try {
            DataInputStream(BufferedInputStream(FileInputStream(file))).use { input ->
                require(input.readInt() == MAGIC) { "invalid health OpenPGP recipient envelope" }
                require(input.readInt() == VERSION) { "unsupported health OpenPGP recipient version" }
                val count = input.readInt()
                require(count in 0..MAX_RECIPIENTS) { "health OpenPGP recipient count is invalid" }
                val values = buildList {
                    repeat(count) {
                        val size = input.readInt()
                        require(size in 1..MAX_KEY_BYTES) { "health OpenPGP public key size is invalid" }
                        val value = ByteArray(size)
                        try {
                            input.readFully(value)
                        } catch (error: EOFException) {
                            value.fill(0)
                            throw error
                        }
                        require(keyIds(value).isNotEmpty()) { "health OpenPGP public key cannot encrypt" }
                        add(value)
                    }
                }
                require(input.read() == -1) { "trailing health OpenPGP recipient data" }
                require(values.flatMap(keyIds).distinct().size <= MAX_RECIPIENTS) {
                    "too many health OpenPGP recipients"
                }
                return values
            }
        } catch (error: HealthOpenPgpRecipientStoreException) {
            throw error
        } catch (error: Exception) {
            throw HealthOpenPgpRecipientStoreException("Health OpenPGP recipients could not be read", error)
        }
    }

    fun add(armoredPublicKey: ByteArray) {
        val incomingIds = try {
            keyIds(armoredPublicKey)
        } catch (error: Exception) {
            throw HealthOpenPgpRecipientStoreException("The selected file is not a usable OpenPGP public key", error)
        }
        if (incomingIds.isEmpty()) {
            throw HealthOpenPgpRecipientStoreException("The selected OpenPGP key cannot encrypt health exports")
        }
        val retained = load().filter { existing -> keyIds(existing).intersect(incomingIds).isEmpty() }
        save(retained + listOf(armoredPublicKey.copyOf()))
    }

    private fun save(values: List<ByteArray>) {
        require(values.size <= MAX_RECIPIENTS) { "too many health OpenPGP key files" }
        require(values.flatMap(keyIds).distinct().size <= MAX_RECIPIENTS) {
            "too many health OpenPGP recipients"
        }
        file.parentFile?.mkdirs()
        val temp = File(file.parentFile, ".${file.name}.tmp")
        try {
            FileOutputStream(temp).use { raw ->
                DataOutputStream(BufferedOutputStream(raw)).use { output ->
                    output.writeInt(MAGIC)
                    output.writeInt(VERSION)
                    output.writeInt(values.size)
                    values.forEach { value ->
                        require(value.size in 1..MAX_KEY_BYTES) { "health OpenPGP public key size is invalid" }
                        output.writeInt(value.size)
                        output.write(value)
                    }
                    output.flush()
                    raw.fd.sync()
                }
            }
            atomicReplace(temp, file)
        } catch (error: Exception) {
            throw HealthOpenPgpRecipientStoreException("Health OpenPGP recipients could not be saved", error)
        } finally {
            temp.delete()
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
        private const val MAGIC = 0x5A484B31
        private const val VERSION = 1
        const val MAX_RECIPIENTS = 32
        const val MAX_KEY_BYTES = 256 * 1024

        fun create(context: Context): HealthOpenPgpRecipientStore = HealthOpenPgpRecipientStore(
            File(context.noBackupFilesDir, "zara/health/openpgp-recipients.bin"),
        )
    }
}
