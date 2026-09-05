package ai.zara.app.runtime

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

private const val STATE_MAGIC = 0x5A415231
private const val STATE_VERSION = 1
private const val MAX_STATE_BYTES = 64 * 1024
private const val MAX_ENDPOINT_BYTES = 2048
private const val MAX_CONVERSATION_ID_BYTES = 256

data class RestorableClientState(
    val profile: ServerProfile,
    val selectedConversationId: String?,
)

class ClientStateStore(private val file: File) {
    fun save(state: RestorableClientState) {
        val directory = file.absoluteFile.parentFile
            ?: throw IllegalStateException("client state path has no parent directory")
        check(directory.exists() || directory.mkdirs()) {
            "client state directory could not be created"
        }
        val temp = Files.createTempFile(
            directory.toPath(),
            ".${file.name}.",
            ".tmp",
        ).toFile()
        try {
            FileOutputStream(temp).use { raw ->
                DataOutputStream(BufferedOutputStream(raw)).use { output ->
                    output.writeInt(STATE_MAGIC)
                    output.writeInt(STATE_VERSION)
                    writeString(output, state.profile.endpoint, MAX_ENDPOINT_BYTES)
                    output.writeBoolean(state.selectedConversationId != null)
                    state.selectedConversationId?.let {
                        writeString(output, it, MAX_CONVERSATION_ID_BYTES)
                    }
                    output.flush()
                    raw.fd.sync()
                }
            }
            replace(temp, file)
        } finally {
            if (temp.exists()) temp.delete()
        }
    }

    fun load(): RestorableClientState? {
        if (!file.exists() || file.length() !in 1..MAX_STATE_BYTES.toLong()) return null
        return try {
            DataInputStream(BufferedInputStream(FileInputStream(file))).use { input ->
                require(input.readInt() == STATE_MAGIC)
                require(input.readInt() == STATE_VERSION)
                val endpoint = readString(input, MAX_ENDPOINT_BYTES)
                val conversation = if (input.readBoolean()) {
                    readString(input, MAX_CONVERSATION_ID_BYTES)
                } else {
                    null
                }
                require(input.read() == -1)
                RestorableClientState(
                    profile = ServerProfile.create(endpoint),
                    selectedConversationId = conversation,
                )
            }
        } catch (_: Exception) {
            null
        }
    }

    private fun writeString(output: DataOutputStream, value: String, maximum: Int) {
        val encoded = value.encodeToByteArray()
        require(encoded.isNotEmpty() && encoded.size <= maximum)
        output.writeInt(encoded.size)
        output.write(encoded)
    }

    private fun readString(input: DataInputStream, maximum: Int): String {
        val size = input.readInt()
        require(size in 1..maximum)
        val encoded = ByteArray(size)
        input.readFully(encoded)
        return encoded.decodeToString(throwOnInvalidSequence = true)
    }

    private fun replace(source: File, destination: File) {
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
}
