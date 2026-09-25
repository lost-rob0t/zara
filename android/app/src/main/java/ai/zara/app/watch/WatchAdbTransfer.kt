package ai.zara.app.watch

import io.github.muntashirakon.adb.AbsAdbConnectionManager
import io.github.muntashirakon.adb.AdbStream
import java.io.File
import java.io.FileInputStream
import java.io.IOException
import java.io.InputStream
import java.io.ByteArrayOutputStream

object WatchAdbTransfer {
    private const val CHUNK = 64 * 1024
    private const val FILE_MODE = 33188

    fun push(
        manager: AbsAdbConnectionManager,
        file: File,
        remotePath: String,
        onProgress: (Long, Long) -> Unit,
    ) {
        val total = file.length()
        manager.openStream("sync:").use { stream ->
            val input = stream.openInputStream()
            val pathAndMode = "$remotePath,$FILE_MODE".toByteArray(Charsets.UTF_8)
            stream.sendChunk("SEND", pathAndMode, pathAndMode.size)
            var sent = 0L
            FileInputStream(file).use { source ->
                val buffer = ByteArray(CHUNK)
                while (true) {
                    val read = source.read(buffer)
                    if (read < 0) break
                    if (read == 0) continue
                    stream.sendChunk("DATA", buffer, read)
                    sent += read
                    onProgress(sent, total)
                }
            }
            stream.sendHeader("DONE", (file.lastModified() / 1000L).toInt())
            val response = ByteArray(8)
            input.readFullyOrThrow(response)
            when (val id = String(response, 0, 4, Charsets.US_ASCII)) {
                "OKAY" -> Unit
                "FAIL" -> {
                    val message = ByteArray(readLe32(response, 4).coerceIn(0, 4096))
                    runCatching { input.readFullyOrThrow(message) }
                    throw IOException("Push rejected: ${String(message, Charsets.UTF_8)}")
                }
                else -> throw IOException("Unexpected sync reply '$id'")
            }
            runCatching { stream.sendHeader("QUIT", 0) }
        }
    }

    fun exec(
        manager: AbsAdbConnectionManager,
        command: String,
        maxBytes: Int,
    ): ByteArray {
        require(maxBytes in 1..(32 * 1024 * 1024)) { "ADB exec byte limit is invalid" }
        manager.openStream("exec:$command").use { stream ->
            val input = stream.openInputStream()
            val output = ByteArrayOutputStream()
            val buffer = ByteArray(16 * 1024)
            while (true) {
                val read = try {
                    input.read(buffer)
                } catch (_: IOException) {
                    break
                }
                if (read < 0) break
                if (read == 0) continue
                if (output.size() + read > maxBytes) {
                    throw IOException("ADB exec response exceeds byte limit")
                }
                output.write(buffer, 0, read)
            }
            return output.toByteArray()
        }
    }

    fun shell(manager: AbsAdbConnectionManager, command: String): String {
        manager.openStream("shell:$command").use { stream ->
            val input = stream.openInputStream()
            val output = StringBuilder()
            val buffer = ByteArray(8 * 1024)
            while (true) {
                val read = try {
                    input.read(buffer)
                } catch (_: IOException) {
                    break
                }
                if (read < 0) break
                if (read > 0) output.append(String(buffer, 0, read, Charsets.UTF_8))
            }
            return output.toString()
        }
    }

    private fun AdbStream.sendChunk(id: String, payload: ByteArray, length: Int) {
        val packet = ByteArray(8 + length)
        writeHeaderInto(packet, id, length)
        payload.copyInto(packet, 8, 0, length)
        write(packet, 0, packet.size)
    }

    private fun AdbStream.sendHeader(id: String, value: Int) {
        val packet = ByteArray(8)
        writeHeaderInto(packet, id, value)
        write(packet, 0, packet.size)
    }

    private fun writeHeaderInto(target: ByteArray, id: String, value: Int) {
        val idBytes = id.toByteArray(Charsets.US_ASCII)
        idBytes.copyInto(target, 0, 0, 4)
        target[4] = (value and 0xFF).toByte()
        target[5] = ((value shr 8) and 0xFF).toByte()
        target[6] = ((value shr 16) and 0xFF).toByte()
        target[7] = ((value shr 24) and 0xFF).toByte()
    }

    private fun readLe32(source: ByteArray, offset: Int): Int =
        (source[offset].toInt() and 0xFF) or
            ((source[offset + 1].toInt() and 0xFF) shl 8) or
            ((source[offset + 2].toInt() and 0xFF) shl 16) or
            ((source[offset + 3].toInt() and 0xFF) shl 24)

    private fun InputStream.readFullyOrThrow(target: ByteArray) {
        var offset = 0
        while (offset < target.size) {
            val read = read(target, offset, target.size - offset)
            if (read < 0) throw IOException("Connection closed before the device replied")
            offset += read
        }
    }
}
