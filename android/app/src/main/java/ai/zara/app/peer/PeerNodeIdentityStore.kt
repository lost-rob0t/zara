package ai.zara.app.peer

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
import java.security.SecureRandom

private const val STORE_MAGIC = 0x5A504E31
private const val STORE_VERSION = 1
private const val MAX_STORE_BYTES = 64 * 1024
private const val MAX_NODE_ID_CHARS = 128
private val NODE_ID = Regex("[A-Za-z0-9][A-Za-z0-9._:-]{0,127}")

data class StoredPeerNodeIdentity(
    val nodeId: String,
    val enrollmentGeneration: Long,
)

/**
 * Durable node id and enrollment generation for this Android peer node. The
 * node identity cannot rotate in-band: only the explicit [replace] enrollment
 * authority seam changes it, and process recreation restores it unchanged so a
 * restarted listener advertises the same identity while starting truthfully
 * stopped.
 */
class PeerNodeIdentityStore(private val file: File) {
    fun load(): StoredPeerNodeIdentity? {
        if (!file.exists() || file.length() !in 1..MAX_STORE_BYTES.toLong()) return null
        return try {
            DataInputStream(BufferedInputStream(FileInputStream(file))).use { input ->
                require(input.readInt() == STORE_MAGIC)
                require(input.readInt() == STORE_VERSION)
                val nodeId = readNodeId(input)
                val generation = input.readLong()
                require(generation > 0)
                require(input.read() == -1)
                StoredPeerNodeIdentity(nodeId, generation)
            }
        } catch (_: Exception) {
            null
        }
    }

    fun ensure(): StoredPeerNodeIdentity =
        load() ?: create()

    fun replace(nodeId: String, enrollmentGeneration: Long): StoredPeerNodeIdentity {
        validate(nodeId, enrollmentGeneration)
        val identity = StoredPeerNodeIdentity(nodeId, enrollmentGeneration)
        save(identity)
        return identity
    }

    private fun create(): StoredPeerNodeIdentity {
        val suffix = StringBuilder()
        val random = ByteArray(8)
        SecureRandom().nextBytes(random)
        random.forEach { byte -> suffix.append("%02x".format(byte)) }
        val identity = StoredPeerNodeIdentity(
            nodeId = "zara-android-$suffix",
            enrollmentGeneration = 1,
        )
        save(identity)
        return identity
    }

    private fun validate(nodeId: String, enrollmentGeneration: Long) {
        require(nodeId.length <= MAX_NODE_ID_CHARS && NODE_ID.matches(nodeId)) {
            "node id must use the bounded canonical peer-id grammar"
        }
        require(enrollmentGeneration > 0) { "enrollment generation must be positive" }
    }

    private fun save(identity: StoredPeerNodeIdentity) {
        val directory = file.absoluteFile.parentFile
            ?: throw IllegalStateException("peer node identity path has no parent directory")
        check(directory.exists() || directory.mkdirs()) {
            "peer node identity directory could not be created"
        }
        val temp = Files.createTempFile(directory.toPath(), ".${file.name}.", ".tmp").toFile()
        try {
            FileOutputStream(temp).use { raw ->
                DataOutputStream(BufferedOutputStream(raw)).use { output ->
                    output.writeInt(STORE_MAGIC)
                    output.writeInt(STORE_VERSION)
                    val encoded = identity.nodeId.encodeToByteArray()
                    output.writeInt(encoded.size)
                    output.write(encoded)
                    output.writeLong(identity.enrollmentGeneration)
                    output.flush()
                    raw.fd.sync()
                }
            }
            replaceFile(temp, file)
        } finally {
            if (temp.exists()) temp.delete()
        }
    }

    private fun readNodeId(input: DataInputStream): String {
        val size = input.readInt()
        require(size in 1..MAX_NODE_ID_CHARS)
        val encoded = ByteArray(size)
        input.readFully(encoded)
        return encoded.decodeToString(throwOnInvalidSequence = true)
    }

    private fun replaceFile(source: File, destination: File) {
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
