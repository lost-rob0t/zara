package ai.zara.app.update

import android.content.Context
import androidx.test.platform.app.InstrumentationRegistry
import java.util.concurrent.TimeUnit
import org.junit.Assert.*
import org.junit.Test

class UpdateSelectionInstrumentedTest {
    @Test fun selectedChannelAndVersionSurviveManagerRecreationAndRefresh() {
        val context = InstrumentationRegistry.getInstrumentation().targetContext
        val preferences = context.getSharedPreferences("zara-updates", Context.MODE_PRIVATE)
        val previous = preferences.all.toMap()
        preferences.edit().clear().commit()
        try {
            manager(context).use { manager ->
                manager.check().get(5, TimeUnit.SECONDS)
                assertEquals(UpdateChannel.Master, manager.state().channel)
                manager.selectChannel(UpdateChannel.Versioned).get(5, TimeUnit.SECONDS)
                val older = manager.state().choices.single { it.version == "0.3.1-alpha" }
                manager.select(older.selectionId).get(5, TimeUnit.SECONDS)
            }
            manager(context).use { manager ->
                assertEquals(UpdateChannel.Versioned, manager.state().channel)
                val refreshed = manager.check().get(5, TimeUnit.SECONDS)
                assertEquals("0.3.1-alpha", refreshed.release!!.version)
                assertEquals(UpdatePhase.AVAILABLE, refreshed.phase)
                assertTrue(refreshed.release!!.apkUrl.endsWith("zara-android-0.3.1-alpha.apk"))
            }
        } finally {
            val editor = preferences.edit().clear()
            previous.forEach { (key, value) -> if (value is String) editor.putString(key, value) }
            editor.commit()
        }
    }

    private fun manager(context: Context) = AndroidUpdateManager(
        context, "0.2.2-alpha", "f".repeat(40), currentVersionCode = 4,
        metadataReader = { url, _ ->
            when {
                url.contains("api.github.com") -> "[${release("0.4.0-alpha")},${release("0.3.1-alpha")}]"
                url.endsWith("zara-latest.manifest.txt") ->
                    "schema=1\nchannel=android-latest\nmutable=true\nsource_sha=${"c".repeat(40)}\nversion_name=0.4.0-alpha\nversion_code=7\nphone_apk=zara-latest.apk\nphone_sha256=${"b".repeat(64)}"
                else -> {
                    val version = if (url.contains("0.4.0-alpha")) "0.4.0-alpha" else "0.3.1-alpha"
                    val source = if (version == "0.4.0-alpha") "a" else "d"
                    "schema=1\nsource_sha=${source.repeat(40)}\nversion_name=$version\nversion_code=6\napk=zara-android-$version.apk\napk_sha256=${"b".repeat(64)}"
                }
            }
        },
    )

    private fun release(version: String): String {
        val base = "https://github.com/lost-rob0t/zara/releases/download/v$version"
        return """{"tag_name":"v$version","target_commitish":"master","assets":[
            {"name":"zara-code-editor-$version.apk","browser_download_url":"$base/zara-code-editor-$version.apk"},
            {"name":"zara-android-$version.apk","browser_download_url":"$base/zara-android-$version.apk"},
            {"name":"zara-android-$version.manifest.txt","browser_download_url":"$base/zara-android-$version.manifest.txt"}]}"""
    }
}
