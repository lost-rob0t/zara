package ai.zara.app.plugins

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.IOException
import java.io.InputStream
import java.util.concurrent.CancellationException
import org.junit.Test

class PluginApkPolicyTest {
    private val identity = PluginApkIdentity(
        packageName = "example.plugin",
        versionName = "1.0.0",
        versionCode = 1,
        signerSha256 = listOf("a".repeat(64)),
        minSdk = 29,
        hasSplits = false,
    )

    @Test fun acceptsThirdPartyPluginWithoutGrantingTrust() {
        PluginApkPolicy.validateIdentity(identity, "ai.zara.app", 36)
    }

    @Test fun rejectsHostReplacement() {
        rejects<IllegalArgumentException> {
            PluginApkPolicy.validateIdentity(identity.copy(packageName = "ai.zara.app"), "ai.zara.app", 36)
        }
    }

    @Test fun rejectsInvalidPackageIdentity() {
        listOf("", "../plugin", "example.plugin\nspoof", "example").forEach { name ->
            rejects<IllegalArgumentException> {
                PluginApkPolicy.validateIdentity(identity.copy(packageName = name), "ai.zara.app", 36)
            }
        }
    }

    @Test fun rejectsMissingOrMalformedSigners() {
        listOf(emptyList(), listOf("invalid"), List(9) { "a".repeat(64) }).forEach { signers ->
            rejects<IllegalArgumentException> {
                PluginApkPolicy.validateIdentity(identity.copy(signerSha256 = signers), "ai.zara.app", 36)
            }
        }
    }

    @Test fun rejectsUnsupportedSdkAndSplitApks() {
        listOf(identity.copy(minSdk = 37), identity.copy(hasSplits = true)).forEach { candidate ->
            rejects<IllegalArgumentException> {
                PluginApkPolicy.validateIdentity(candidate, "ai.zara.app", 36)
            }
        }
    }

    @Test fun rejectsUnboundedOrMisleadingVersionNames() {
        listOf("x".repeat(129), "1.0\nTrusted publisher").forEach { name ->
            rejects<IllegalArgumentException> {
                PluginApkPolicy.validateIdentity(identity.copy(versionName = name), "ai.zara.app", 36)
            }
        }
    }

    @Test fun copiesExactlyTheLimitAndHashesExactBytes() {
        val output = ByteArrayOutputStream()
        val result = PluginApkPolicy.copy(ByteArrayInputStream("abc".toByteArray()), output, 3)
        check(output.toString("UTF-8") == "abc")
        check(result.bytes == 3L)
        check(result.sha256 == "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad")
    }

    @Test fun refusesOverflowBeforeWritingExcessBytes() {
        val output = ByteArrayOutputStream()
        rejects<IllegalArgumentException> {
            PluginApkPolicy.copy(ByteArrayInputStream(ByteArray(4)), output, 3)
        }
        check(output.size() <= 3)
    }

    @Test fun rejectsEmptyApkAndInvalidLimits() {
        rejects<IllegalArgumentException> {
            PluginApkPolicy.copy(ByteArrayInputStream(byteArrayOf()), ByteArrayOutputStream())
        }
        rejects<IllegalArgumentException> {
            PluginApkPolicy.copy(ByteArrayInputStream(byteArrayOf(1)), ByteArrayOutputStream(), 0)
        }
    }

    @Test fun propagatesReadErrorsAndCancellation() {
        rejects<IOException> {
            PluginApkPolicy.copy(object : InputStream() {
                override fun read(): Int = throw IOException("fixture failure")
            }, ByteArrayOutputStream())
        }
        val output = ByteArrayOutputStream()
        rejects<CancellationException> {
            PluginApkPolicy.copy(ByteArrayInputStream(byteArrayOf(1)), output) {
                throw CancellationException("fixture cancellation")
            }
        }
        check(output.size() == 0)
    }

    @Test fun failsInsteadOfSpinningOnZeroByteReads() {
        rejects<IOException> {
            PluginApkPolicy.copy(object : InputStream() {
                override fun read(): Int = 0
                override fun read(buffer: ByteArray, offset: Int, length: Int): Int = 0
            }, ByteArrayOutputStream())
        }
    }

    @Test fun changedBytesDoNotMatchReviewedDigest() {
        val first = PluginApkPolicy.copy(ByteArrayInputStream(byteArrayOf(1)), ByteArrayOutputStream())
        val changed = PluginApkPolicy.copy(ByteArrayInputStream(byteArrayOf(2)), ByteArrayOutputStream())
        check(first.sha256 != changed.sha256)
    }

    private inline fun <reified T : Exception> rejects(block: () -> Unit) {
        try {
            block()
        } catch (error: Exception) {
            check(error is T) { "Expected ${T::class.java.name}, got ${error::class.java.name}" }
            return
        }
        error("Expected ${T::class.java.name}")
    }
}
