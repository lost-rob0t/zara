package ai.zara.app.plugins

import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.security.MessageDigest
import java.util.Locale

internal object PluginApkSecurity {
    const val MAX_APK_BYTES = 256L * 1024 * 1024
    private val sha256Pattern = Regex("[0-9a-fA-F]{64}")

    fun normalizeSha256(value: String): String {
        val normalized = value.trim()
        require(sha256Pattern.matches(normalized)) { "Enter the publisher's 64-character SHA-256." }
        return normalized.lowercase(Locale.ROOT)
    }

    fun copyVerified(
        input: InputStream,
        output: OutputStream,
        expectedSha256: String,
        maxBytes: Long = MAX_APK_BYTES,
        cancelled: () -> Boolean = { Thread.currentThread().isInterrupted },
    ): Long {
        val expected = normalizeSha256(expectedSha256)
        require(maxBytes in 1..MAX_APK_BYTES) { "Invalid APK size limit." }
        val digest = MessageDigest.getInstance("SHA-256")
        val buffer = ByteArray(DEFAULT_BUFFER_SIZE)
        var copied = 0L
        while (true) {
            if (cancelled()) throw IOException("APK verification cancelled.")
            val count = input.read(buffer)
            if (count < 0) break
            if (count == 0) throw IOException("The APK provider stopped returning data.")
            require(count.toLong() <= maxBytes - copied) { "The APK exceeds the 256 MiB limit." }
            output.write(buffer, 0, count)
            digest.update(buffer, 0, count)
            copied += count
        }
        require(copied > 0) { "The selected APK is empty." }
        require(hex(digest.digest()) == expected) { "APK SHA-256 does not match the publisher's checksum." }
        return copied
    }

    fun validateIdentity(packageName: String, hostPackageName: String, certificates: List<String>) {
        require(packageName.isNotBlank() && packageName.length <= 255) { "The APK has no valid package identity." }
        require(packageName != hostPackageName) { "Use Settings > General > Self update to update Zara itself." }
        require(certificates.size in 1..8 && certificates.all(sha256Pattern::matches)) {
            "The APK has no readable signing certificate."
        }
    }

    fun matchesCallback(expectedSession: Int, expectedNonce: String, session: Int, nonce: String?): Boolean =
        expectedSession >= 0 && expectedNonce.isNotEmpty() &&
            expectedSession == session && expectedNonce == nonce

    fun hex(bytes: ByteArray): String = bytes.joinToString("") { "%02x".format(it) }
}
