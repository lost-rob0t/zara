package ai.zara.app.watch

import android.content.Context
import java.io.File
import java.io.FileOutputStream
import java.net.HttpURLConnection
import java.net.URL
import java.security.MessageDigest

class WearApkRepository(
    private val context: Context,
) {
    fun downloadWear(onProgress: (Long, Long) -> Unit): File =
        download(
            apkUrl = WEAR_APK_URL,
            checksumUrl = WEAR_CHECKSUM_URL,
            fileName = "zara-wear-latest.apk",
            label = "Wear",
            onProgress = onProgress,
        )

    fun downloadAgenda(onProgress: (Long, Long) -> Unit): File =
        download(
            apkUrl = AGENDA_APK_URL,
            checksumUrl = AGENDA_CHECKSUM_URL,
            fileName = "zara-agenda-latest.apk",
            label = "Agenda",
            onProgress = onProgress,
        )

    private fun download(
        apkUrl: String,
        checksumUrl: String,
        fileName: String,
        label: String,
        onProgress: (Long, Long) -> Unit,
    ): File {
        val expected = readText(checksumUrl).trim().split(Regex("\\s+")).firstOrNull()
            ?: error("$label APK checksum is empty")
        require(expected.matches(Regex("[0-9a-fA-F]{64}"))) { "$label APK checksum is invalid" }

        val target = File(context.cacheDir, fileName)
        val partial = File(context.cacheDir, "$fileName.part")
        partial.delete()

        val connection = open(apkUrl)
        try {
            val total = connection.contentLengthLong
            connection.inputStream.use { input ->
                FileOutputStream(partial).use { output ->
                    val buffer = ByteArray(64 * 1024)
                    var written = 0L
                    while (true) {
                        val read = input.read(buffer)
                        if (read < 0) break
                        if (read == 0) continue
                        output.write(buffer, 0, read)
                        written += read
                        onProgress(written, total)
                    }
                }
            }
        } finally {
            connection.disconnect()
        }

        val actual = sha256(partial)
        if (!actual.equals(expected, ignoreCase = true)) {
            partial.delete()
            error("$label APK checksum mismatch")
        }
        target.delete()
        if (!partial.renameTo(target)) {
            partial.copyTo(target, overwrite = true)
            partial.delete()
        }
        return target
    }

    private fun readText(url: String): String {
        val connection = open(url)
        return try {
            connection.inputStream.bufferedReader().use { it.readText() }
        } finally {
            connection.disconnect()
        }
    }

    private fun open(url: String): HttpURLConnection {
        val connection = URL(url).openConnection() as HttpURLConnection
        connection.connectTimeout = 15_000
        connection.readTimeout = 30_000
        connection.instanceFollowRedirects = true
        connection.setRequestProperty("User-Agent", "Zara-Android")
        connection.connect()
        if (connection.responseCode !in 200..299) {
            val code = connection.responseCode
            connection.disconnect()
            error("Watch artifact download failed with HTTP $code")
        }
        return connection
    }

    private fun sha256(file: File): String {
        val digest = MessageDigest.getInstance("SHA-256")
        file.inputStream().use { input ->
            val buffer = ByteArray(64 * 1024)
            while (true) {
                val read = input.read(buffer)
                if (read < 0) break
                if (read > 0) digest.update(buffer, 0, read)
            }
        }
        return digest.digest().joinToString("") { "%02x".format(it) }
    }

    companion object {
        const val WEAR_APK_URL =
            "https://github.com/lost-rob0t/zara/releases/download/android-latest/zara-wear-latest.apk"
        const val WEAR_CHECKSUM_URL =
            "https://github.com/lost-rob0t/zara/releases/download/android-latest/zara-wear-latest.apk.sha256"
        const val AGENDA_APK_URL =
            "https://github.com/lost-rob0t/zara/releases/download/android-latest/zara-agenda-latest.apk"
        const val AGENDA_CHECKSUM_URL =
            "https://github.com/lost-rob0t/zara/releases/download/android-latest/zara-agenda-latest.apk.sha256"
    }
}
