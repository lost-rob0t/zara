package ai.zara.app.update

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class VersionedUpdateManifestTest {
    private val version = "0.2.3-alpha"
    private val apkName = "zara-android-$version.apk"
    private val apkUrl =
        "https://github.com/lost-rob0t/zara/releases/download/v$version/$apkName"

    private fun manifest(
        sourceSha: String = "a".repeat(40),
        versionName: String = version,
        versionCode: Long = 5,
        apk: String = apkName,
        apkSha256: String = "b".repeat(64),
    ): String = """
        schema=1
        source_sha=$sourceSha
        version_name=$versionName
        version_code=$versionCode
        apk=$apk
        apk_sha256=$apkSha256
    """.trimIndent()

    @Test
    fun `canonical versioned manifest owns immutable source and phone identity`() {
        val parsed = VersionedUpdateManifest.parse(
            manifest(),
            expectedVersion = version,
            apkUrl = apkUrl,
        ).getOrThrow()

        assertEquals(UpdateChannel.Versioned, parsed.release.channel)
        assertEquals("a".repeat(40), parsed.release.sourceSha)
        assertEquals(version, parsed.release.version)
        assertEquals(apkName, parsed.apkName)
        assertEquals(5L, parsed.versionCode)
        assertEquals("b".repeat(64), parsed.release.sha256)
    }

    @Test
    fun `rejects duplicate foreign and mismatched versioned provenance`() {
        assertTrue(
            VersionedUpdateManifest.parse(
                manifest() + "\nsource_sha=dddddddddddddddddddddddddddddddddddddddd",
                expectedVersion = version,
                apkUrl = apkUrl,
            ).isFailure
        )
        assertTrue(
            VersionedUpdateManifest.parse(
                manifest(apk = "zara-code-editor-$version.apk"),
                expectedVersion = version,
                apkUrl = apkUrl,
            ).isFailure
        )
        assertTrue(
            VersionedUpdateManifest.parse(
                manifest(versionName = "0.2.4-alpha"),
                expectedVersion = version,
                apkUrl = apkUrl,
            ).isFailure
        )
        assertTrue(
            VersionedUpdateManifest.parse(
                manifest(apkSha256 = "not-a-digest"),
                expectedVersion = version,
                apkUrl = apkUrl,
            ).isFailure
        )
    }
}
