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
    fun rejectsChecksumMismatchAndOversize() {
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.copyVerified(
                ByteArrayInputStream(payload),
                ByteArrayOutputStream(),
                "0".repeat(64),
            )
        }
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.copyVerified(
                ByteArrayInputStream(payload),
                ByteArrayOutputStream(),
                digest,
                payload.size.toLong() - 1,
            )
        }
    }

    @Test
    fun cancellationStopsBeforeReadingOrWriting() {
        val output = ByteArrayOutputStream()
        expectFailure<IOException> {
            PluginApkSecurity.copyVerified(
                ByteArrayInputStream(payload),
                output,
                digest,
                cancelled = { true },
            )
        }
        check(output.size() == 0)
    }

    @Test
    fun validatesIdentityWithoutGrantingTrust() {
        PluginApkSecurity.validateIdentity("example.plugin", "ai.zara.app", listOf(digest))
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.validateIdentity("ai.zara.app", "ai.zara.app", listOf(digest))
        }
        expectFailure<IllegalArgumentException> {
            PluginApkSecurity.validateIdentity("example.plugin", "ai.zara.app", emptyList())
        }
    }

    @Test
    fun onlyAcceptsCurrentInstallationCallback() {
        check(PluginApkSecurity.matchesCallback(42, "nonce", 42, "nonce"))
        check(!PluginApkSecurity.matchesCallback(42, "nonce", 41, "nonce"))
        check(!PluginApkSecurity.matchesCallback(42, "nonce", 42, "old"))
        check(!PluginApkSecurity.matchesCallback(-1, "nonce", -1, "nonce"))
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
