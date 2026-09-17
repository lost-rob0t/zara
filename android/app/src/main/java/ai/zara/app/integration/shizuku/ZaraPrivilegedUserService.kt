package ai.zara.app.integration.shizuku

import android.content.Context
import android.os.Process
import java.io.ByteArrayOutputStream
import java.util.concurrent.TimeUnit
import kotlin.concurrent.thread

class ZaraPrivilegedUserService() : IZaraPrivilegedService.Stub() {
    @Suppress("UNUSED_PARAMETER")
    constructor(context: Context) : this()

    override fun uid(): Int = Process.myUid()

    override fun executeShell(command: String, timeoutMillis: Int): String {
        require(command.encodeToByteArray().size <= MAX_COMMAND_BYTES) { "command is too large" }
        val timeout = timeoutMillis.coerceIn(1_000, 300_000)
        val process = ProcessBuilder("/system/bin/sh", "-c", command)
            .redirectErrorStream(true)
            .start()
        val output = ByteArrayOutputStream(minOf(MAX_OUTPUT_BYTES, 8192))
        val reader = thread(name = "zara-shizuku-drain", isDaemon = true) {
            process.inputStream.use { input ->
                val buffer = ByteArray(8192)
                while (true) {
                    val count = input.read(buffer)
                    if (count < 0) break
                    val remaining = MAX_OUTPUT_BYTES - output.size()
                    if (remaining > 0) {
                        output.write(buffer, 0, minOf(count, remaining))
                    }
                }
            }
        }
        val finished = process.waitFor(timeout.toLong(), TimeUnit.MILLISECONDS)
        if (!finished) {
            process.destroyForcibly()
            reader.join(1_000)
            return "timed_out=true\n${output.toString(Charsets.UTF_8.name())}"
        }
        reader.join(1_000)
        return buildString {
            append("uid=").append(Process.myUid()).append('\n')
            append("exit=").append(process.exitValue()).append('\n')
            append(output.toString(Charsets.UTF_8.name()))
        }.take(MAX_OUTPUT_CHARS)
    }

    override fun destroy() {
        System.exit(0)
    }

    private companion object {
        const val MAX_COMMAND_BYTES = 256 * 1024
        const val MAX_OUTPUT_BYTES = 256 * 1024
        const val MAX_OUTPUT_CHARS = 256 * 1024
    }
}
