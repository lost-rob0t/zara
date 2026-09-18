package ai.zara.app.update

import java.io.File
import java.security.MessageDigest
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class UpdateSecurityTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun semanticVersionComparisonDoesNotDowngradeOrReinstall() {
        assertTrue(UpdateSecurity.isNewer("0.2.0", "0.1.2-alpha"))
        assertTrue(UpdateSecurity.isNewer("0.1.2", "0.1.2-alpha"))
        assertFalse(UpdateSecurity.isNewer("0.1.2-alpha", "0.1.2-alpha"))
        assertFalse(UpdateSecurity.isNewer("0.1.1", "0.1.2-alpha"))
        assertFalse(UpdateSecurity.isNewer("garbage", "0.1.2-alpha"))
    }

    @Test
    fun rollingMasterCandidateUsesExactSourceShaInsteadOfSemver() {
        val rolling = UpdateRelease(
            version = "master",
            sourceSha = "a".repeat(40),
            apkUrl = "https://github.com/lost-rob0t/zara/releases/download/android-latest/zara-latest.apk",
            sha256 = "b".repeat(64),
            channel = UpdateChannel.Master,
        )

        assertTrue(UpdateSecurity.validate(rolling).isSuccess)
        assertTrue(UpdateSecurity.isInstallCandidate(rolling, "0.1.2-alpha", "c".repeat(40)))
        assertFalse(UpdateSecurity.isInstallCandidate(rolling, "0.1.2-alpha", "a".repeat(40)))
    }

    @Test
    fun downloadedApkMustMatchReleaseSha256() {
        val apk = temporary.newFile("zara.apk")
        apk.writeBytes("signed-apk-placeholder".encodeToByteArray())
        val digest = MessageDigest.getInstance("SHA-256")
            .digest(apk.readBytes())
            .joinToString("") { "%02x".format(it.toInt() and 0xff) }

        assertTrue(UpdateSecurity.verifySha256(apk, digest))
        assertFalse(UpdateSecurity.verifySha256(apk, "00".repeat(32)))
        assertFalse(UpdateSecurity.verifySha256(File(apk.parentFile, "missing.apk"), digest))
    }

    @Test
    fun releaseMetadataRequiresTrustedGithubHttpsApkAndChecksum() {
        val valid = UpdateRelease(
            version = "0.2.0",
            sourceSha = "a".repeat(40),
            apkUrl = "https://github.com/lost-rob0t/zara/releases/download/v0.2.0/zara.apk",
            sha256 = "b".repeat(64),
        )
        assertTrue(UpdateSecurity.validate(valid).isSuccess)
        assertTrue(UpdateSecurity.validate(valid.copy(apkUrl = "http://github.com/zara.apk")).isFailure)
        assertTrue(UpdateSecurity.validate(valid.copy(apkUrl = "https://example.test/zara.apk")).isFailure)
        assertTrue(UpdateSecurity.validate(valid.copy(apkUrl = "https://github.com:444/zara.apk")).isFailure)
        assertTrue(UpdateSecurity.validate(valid.copy(apkUrl = "https://user@github.com/zara.apk")).isFailure)
        assertTrue(UpdateSecurity.validate(valid.copy(apkUrl = "https://github.com/zara.zip")).isFailure)
        assertTrue(UpdateSecurity.validate(valid.copy(sourceSha = "main")).isFailure)
    }

    @Test
    fun githubReleaseRedirectHostsAreExplicitlyBounded() {
        assertTrue(runCatching {
            UpdateSecurity.requireTrustedTransport("https://api.github.com/repos/lost-rob0t/zara/releases")
        }.isSuccess)
        assertTrue(runCatching {
            UpdateSecurity.requireTrustedTransport(
                "https://release-assets.githubusercontent.com/github-production-release-asset/zara.apk"
            )
        }.isSuccess)
        assertTrue(runCatching {
            UpdateSecurity.requireTrustedTransport("https://evil.example/zara.apk")
        }.isFailure)
    }
}
