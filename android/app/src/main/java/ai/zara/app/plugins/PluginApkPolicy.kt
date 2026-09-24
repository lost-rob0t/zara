package ai.zara.app.plugins

import java.io.IOException
import java.io.InputStream
import java.io.OutputStream
import java.io.Serializable
import java.security.MessageDigest

data class PluginApkIdentity(
    val packageName: String,
    val versionName: String,
    val versionCode: Long,
    val signerSha256: List<String>,
    val minSdk: Int,
    val hasSplits: Boolean,
) : Serializable

data class CopiedPluginApk(val bytes: Long, val sha256: String)

object PluginApkPolicy {
    const val MAX_APK_BYTES = 256L * 1024 * 1024
    private val packagePattern = Regex("[A-Za-z][A-Za-z0-9_]*(\\.[A-Za-z][A-Za-z0-9_]*)+")
    private val digestPattern = Regex("[0-9a-f]{64}")

    fun validateIdentity(identity: PluginApkIdentity, hostPackageName: String, sdkInt: Int) {
        require(identity.packageName.length <= 255 && packagePattern.matches(identity.packageName)) {
            "APK package identity is invalid"
        }
        require(identity.packageName != hostPackageName) { "Use Self Update to update Zara" }
        require(identity.versionCode >= 0) { "APK version is invalid" }
        require(identity.versionName.length <= 128 && identity.versionName.none(Char::isISOControl)) {
            "APK version label is invalid"
        }
        require(identity.signerSha256.size in 1..8 && identity.signerSha256.all(digestPattern::matches)) {
            "APK signing identity is missing or invalid"
        }
        require(identity.minSdk in 1..sdkInt) { "APK requires a newer Android version" }
        require(!identity.hasSplits) { "Select a standalone APK, not a split APK" }
    }

    fun copy(
        input: InputStream,
        output: OutputStream,
        maxBytes: Long = MAX_APK_BYTES,
        checkCancelled: () -> Unit = {},
    ): CopiedPluginApk {
        require(maxBytes in 1..MAX_APK_BYTES) { "APK byte limit is invalid" }
        val digest = MessageDigest.getInstance("SHA-256")
        val buffer = ByteArray(DEFAULT_BUFFER_SIZE)
        var total = 0L
        while (true) {
            checkCancelled()
            val count = input.read(buffer)
            if (count < 0) break
            if (count == 0) throw IOException("APK provider returned no data")
            require(count.toLong() <= maxBytes - total) { "APK exceeds the size limit" }
            output.write(buffer, 0, count)
            digest.update(buffer, 0, count)
            total += count
        }
        require(total > 0) { "APK is empty" }
        return CopiedPluginApk(total, hex(digest.digest()))
    }

    fun certificateDigest(bytes: ByteArray): String =
        hex(MessageDigest.getInstance("SHA-256").digest(bytes))

    private fun hex(bytes: ByteArray): String = bytes.joinToString("") { "%02x".format(it) }
}
