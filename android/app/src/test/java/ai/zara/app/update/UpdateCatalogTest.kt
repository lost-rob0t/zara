package ai.zara.app.update

import org.json.JSONArray
import org.json.JSONObject
import org.junit.Assert.*
import org.junit.Test

class UpdateCatalogTest {
    private val source = "a".repeat(40)
    private val checksum = "b".repeat(64)
    private val base = "https://github.com/lost-rob0t/zara/releases/download/v0.4.0-alpha"

    @Test fun phoneManifestWinsOverCompanionOrderingAndBranchTarget() {
        val json = release()
        val requested = mutableListOf<String>()
        val result = UpdateCatalog.versioned(json) { url ->
            requested.add(url)
            manifest()
        }!!
        assertEquals("$base/zara-android-0.4.0-alpha.apk", result.apkUrl)
        assertEquals(source, result.sourceSha)
        assertEquals(7, result.versionCode)
        assertEquals(listOf("$base/zara-android-0.4.0-alpha.manifest.txt"), requested)
    }

    @Test fun invalidOrCrossProductManifestsAreRejected() {
        for (bad in listOf(
            manifest().replace("apk=zara-android", "apk=zara-wear"),
            manifest().replace("version_name=0.4.0-alpha", "version_name=0.5.0"),
            manifest().replace("version_code=7", "version_code=-1"),
            manifest() + "\nsource_sha=${"c".repeat(40)}",
            manifest().replace(source, "master"),
        )) assertThrows(IllegalArgumentException::class.java) {
            UpdateCatalog.versioned(release()) { bad }
        }
    }

    @Test fun draftsAndNonVersionTagsNeverFetchAssets() {
        for (json in listOf(release().put("draft", true), release().put("tag_name", "android-latest"))) {
            assertNull(UpdateCatalog.versioned(json) { error("must not fetch") })
        }
    }

    @Test fun catalogSelectionStaysInChosenChannelAndRetainsVersion() {
        val older = UpdateRelease("0.3.1-alpha", source, "$base/phone.apk", checksum)
        val newer = older.copy(version = "0.4.0-alpha", sourceSha = "c".repeat(40))
        val master = older.copy(version = "master", channel = UpdateChannel.Master)
        val choices = listOf(master, newer, older)
        assertEquals(older, UpdateCatalog.select(choices, UpdateChannel.Versioned, older.selectionId))
        assertEquals(newer, UpdateCatalog.select(choices, UpdateChannel.Versioned, "missing"))
        assertNull(UpdateCatalog.select(listOf(master), UpdateChannel.Versioned, null))
        assertEquals(master, UpdateCatalog.select(choices, UpdateChannel.Master, older.selectionId))
    }

    @Test fun masterManifestKeepsActualVersionAndRejectsDowngrade() {
        val text = "schema=1\nchannel=android-latest\nmutable=true\nsource_sha=$source\nversion_name=0.3.1-alpha\nversion_code=6\nphone_apk=zara-latest.apk\nphone_sha256=$checksum"
        val result = UpdateCatalog.master(text, "https://github.com/lost-rob0t/zara/releases/download/android-latest/zara-latest.manifest.txt")
        assertEquals("0.3.1-alpha", result.productVersion)
        assertFalse(UpdateSecurity.isInstallCandidate(result, "0.4.0-alpha", "c".repeat(40), 7))
        assertTrue(UpdateSecurity.isInstallCandidate(result, "0.3.1-alpha", "c".repeat(40), 6))
    }

    private fun manifest() = "schema=1\nsource_sha=$source\nversion_name=0.4.0-alpha\nversion_code=7\napk=zara-android-0.4.0-alpha.apk\napk_sha256=$checksum"

    private fun release(): JSONObject {
        val names = listOf("zara-code-editor-0.4.0-alpha.apk", "zara-android-0.4.0-alpha.apk", "zara-android-0.4.0-alpha.manifest.txt")
        return JSONObject().put("tag_name", "v0.4.0-alpha").put("target_commitish", "master")
            .put("assets", JSONArray(names.map { JSONObject().put("name", it).put("browser_download_url", "$base/$it") }))
    }
}
