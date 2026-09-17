package ai.zara.app.update

import java.io.File
import java.security.MessageDigest

data class UpdateRelease(
    val version: String,
    val sourceSha: String,
    val apkUrl: String,
    val sha256: String,
)

object UpdateSecurity {
    private val sha = Regex("[0-9a-f]{40}")
    private val digest = Regex("[0-9a-f]{64}")
    private val version = Regex("^v?(\\d+)\\.(\\d+)\\.(\\d+)(?:-([0-9A-Za-z.-]+))?$")

    fun validate(release: UpdateRelease): Result<UpdateRelease> = runCatching {
        require(isVersion(release.version)) { "Release version is invalid" }
        require(release.sourceSha.matches(sha)) { "Release source SHA is invalid" }
        require(release.sha256.matches(digest)) { "Release checksum is invalid" }
        require(release.apkUrl.startsWith("https://")) { "Update URL must use HTTPS" }
        require(release.apkUrl.substringBefore('?').endsWith(".apk")) { "Update asset must be an APK" }
        release
    }

    fun isNewer(candidate: String, current: String): Boolean {
        val next = parseVersion(candidate) ?: return false
        val installed = parseVersion(current) ?: return false
        for (index in 0..2) {
            if (next.numbers[index] != installed.numbers[index]) {
                return next.numbers[index] > installed.numbers[index]
            }
        }
        if (next.prerelease == installed.prerelease) return false
        if (next.prerelease == null) return true
        if (installed.prerelease == null) return false
        return comparePrerelease(next.prerelease, installed.prerelease) > 0
    }

    fun verifySha256(file: File, expected: String): Boolean {
        if (!file.isFile || !expected.matches(digest)) return false
        val checksum = MessageDigest.getInstance("SHA-256")
        file.inputStream().use { input ->
            val buffer = ByteArray(DEFAULT_BUFFER_SIZE)
            while (true) {
                val read = input.read(buffer)
                if (read < 0) break
                checksum.update(buffer, 0, read)
            }
        }
        val actual = checksum.digest().joinToString("") { byte ->
            "%02x".format(byte.toInt() and 0xff)
        }
        return MessageDigest.isEqual(actual.encodeToByteArray(), expected.encodeToByteArray())
    }

    private data class ParsedVersion(
        val numbers: List<Int>,
        val prerelease: String?,
    )

    private fun isVersion(value: String): Boolean = parseVersion(value) != null

    private fun parseVersion(value: String): ParsedVersion? {
        val match = version.matchEntire(value.trim()) ?: return null
        val numbers = (1..3).map { match.groupValues[it].toIntOrNull() ?: return null }
        return ParsedVersion(numbers, match.groupValues[4].ifBlank { null })
    }

    private fun comparePrerelease(left: String, right: String): Int {
        val leftParts = left.split('.')
        val rightParts = right.split('.')
        val length = maxOf(leftParts.size, rightParts.size)
        for (index in 0 until length) {
            val a = leftParts.getOrNull(index) ?: return -1
            val b = rightParts.getOrNull(index) ?: return 1
            val aNumber = a.toIntOrNull()
            val bNumber = b.toIntOrNull()
            val comparison = when {
                aNumber != null && bNumber != null -> aNumber.compareTo(bNumber)
                aNumber != null -> -1
                bNumber != null -> 1
                else -> a.compareTo(b)
            }
            if (comparison != 0) return comparison
        }
        return 0
    }
}
