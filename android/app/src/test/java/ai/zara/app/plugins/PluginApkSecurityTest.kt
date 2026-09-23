package ai.zara.app.plugins

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.IOException
import java.io.InputStream
import java.security.MessageDigest
import org.junit.Test

class PluginApkSecurityTest {
    private val payload = "a selected plugin APK".toByteArray()
    private val digest = MessageDigest.getInstance("SHA-256")
        .digest(payload).joinToString("") { "%02x".format(it) }

    @Test
    fun acceptsPublishedDigestAndExactSizeBoundary() {
        val output = ByteArrayOutputStream()
        val copied = PluginApkSecurity.copyVerified(
            ByteArrayInputStream(payload), output, " ${digest.uppercase()} ", payload.size.toLong(),
        )
        check(copied == payload.size.toLong())
        check(output.toByteArray().contentEquals(payload))
    }

    @Test
    fun rejectsMalformedDigestsBeforeReading() {
        listOf("", "a".repeat(63), "g".repeat(64), "a".repeat(65), "a ".repeat(32)).forEach { hash ->
            val input = object : InputStream() {
                override fun read(): Int = error("Malformed digests must not read the APK")
            }
            expectFailure<IllegalArgumentException> {
                PluginApkSecurity.copyVerified(input, ByteArrayOutputStream(), hash)
            }
        }
    }

    @Test
    fun rejectsEmptyApk() {
        val emptyHash = MessageDigest.getInstance("SHA-256")
            .digest(byteArrayOf()).joinToString("") { "%02x".format(it) }
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.copyVerified(ByteArrayInputStream(byteArrayOf()), ByteArrayOutputStream(), emptyHash)
        }
    }

    @Test
    fun rejectsChecksumMismatch() {
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.copyVerified(ByteArrayInputStream(payload), ByteArrayOutputStream(), "0".repeat(64))
        }
    }

    @Test
    fun rejectsOversizedApkWithoutWritingBeyondLimit() {
        val output = ByteArrayOutputStream()
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.copyVerified(ByteArrayInputStream(payload), output, digest, payload.size.toLong() - 1)
        }
        check(output.size() < payload.size)
    }

    @Test
    fun rejectsInvalidSizeLimit() {
        listOf(0L, -1L, PluginApkSecurity.MAX_APK_BYTES + 1).forEach { limit ->
            expectFailure<IllegalArgumentException> {
                PluginApkSecurity.copyVerified(ByteArrayInputStream(payload), ByteArrayOutputStream(), digest, limit)
            }
        }
    }

    @Test
    fun cancellationStopsBeforeReadingOrWriting() {
        val output = ByteArrayOutputStream()
        expectFailure<IOException> {
            PluginApkSecurity.copyVerified(ByteArrayInputStream(payload), output, digest, cancelled = { true })
        }
        check(output.size() == 0)
    }

    @Test
    fun doesNotSpinOnAStalledStream() {
        val input = object : InputStream() {
            override fun read(): Int = 0
            override fun read(buffer: ByteArray, offset: Int, length: Int): Int = 0
        }
        expectFailure<IOException> {
            PluginApkSecurity.copyVerified(input, ByteArrayOutputStream(), digest)
        }
    }

    @Test
    fun propagatesReadFailure() {
        val input = object : InputStream() {
            override fun read(): Int = throw IOException("provider disconnected")
        }
        expectFailure<IOException> {
            PluginApkSecurity.copyVerified(input, ByteArrayOutputStream(), digest)
        }
    }

    @Test
    fun validatesPackageIdentityWithoutGrantingTrust() {
        PluginApkSecurity.validateIdentity("example.plugin", "ai.zara.app", listOf(digest))
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.validateIdentity("ai.zara.app", "ai.zara.app", listOf(digest))
        }
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.validateIdentity("example.plugin", "ai.zara.app", emptyList())
        }
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.validateIdentity("example.plugin", "ai.zara.app", listOf("invalid"))
        }
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.validateIdentity("", "ai.zara.app", listOf(digest))
        }
    }

    @Test
    fun onlyAcceptsTheCurrentInstallationCallback() {
        check(PluginApkSecurity.matchesCallback(42, "nonce", 42, "nonce"))
        check(!PluginApkSecurity.matchesCallback(42, "nonce", 41, "nonce"))
        check(!PluginApkSecurity.matchesCallback(42, "nonce", 42, "old"))
        check(!PluginApkSecurity.matchesCallback(42, "nonce", 42, null))
        check(!PluginApkSecurity.matchesCallback(-1, "nonce", -1, "nonce"))
        check(!PluginApkSecurity.matchesCallback(42, "", 42, ""))
    }

    private inline fun <reified T : Throwable> expectFailure(block: () -> Unit) {
        try {
            block()
        } catch (failure: Throwable) {
            check(failure is T) { "Expected ${T::class.java.name}, got $failure" }
            return
        }
        error("Expected ${T::class.java.name}")
    }
}
