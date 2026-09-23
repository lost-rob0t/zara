package ai.zara.app.update

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class MasterUpdateManifestTest {
    private val apkUrl =
        "https://github.com/lost-rob0t/zara/releases/download/android-latest/zara-latest.apk"

    private fun manifest(
        sourceSha: String = "a".repeat(40),
        versionName: String = "0.2.2-alpha",
        versionCode: Long = 4,
    ): String = """
        schema=1
        channel=android-latest
        mutable=true
        source_sha=$sourceSha
        version_name=$versionName
        version_code=$versionCode
        phone_apk=zara-latest.apk
        phone_sha256=bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb
        wear_apk=zara-wear-latest.apk
        wear_sha256=cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
    """.trimIndent()

    @Test
    fun parsesExactMasterProvenance() {
        val parsed = MasterUpdateManifest.parse(manifest(), apkUrl).getOrThrow()
        assertEquals(UpdateChannel.Master, parsed.release.channel)
        assertEquals("master", parsed.release.version)
        assertEquals("0.2.2-alpha", parsed.versionName)
        assertEquals(4L, parsed.versionCode)
    }

    @Test
    fun rejectsDuplicateAndForeignManifestData() {
        assertTrue(
            MasterUpdateManifest.parse(
                manifest() + "\nsource_sha=dddddddddddddddddddddddddddddddddddddddd",
                apkUrl,
            ).isFailure
        )
        assertTrue(
            MasterUpdateManifest.parse(
                manifest(),
                "https://example.com/zara-latest.apk",
            ).isFailure
        )
    }

    @Test
    fun sameVersionCodeMayAdvanceMasterButNeverDowngrades() {
        val parsed = MasterUpdateManifest.parse(manifest(), apkUrl).getOrThrow()
        assertTrue(
            parsed.isUpdateFor(
                currentSourceSha = "d".repeat(40),
                currentVersion = "0.2.2-alpha",
                currentVersionCode = 4,
            )
        )
        assertFalse(
            parsed.isUpdateFor(
                currentSourceSha = "a".repeat(40),
                currentVersion = "0.2.2-alpha",
                currentVersionCode = 4,
            )
        )
        assertFalse(
            parsed.isUpdateFor(
                currentSourceSha = "d".repeat(40),
                currentVersion = "0.2.3-alpha",
                currentVersionCode = 5,
            )
        )
    }
}
