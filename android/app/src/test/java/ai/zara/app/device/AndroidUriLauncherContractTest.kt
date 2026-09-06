package ai.zara.app.device

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidUriLauncherContractTest {
    @Test
    fun `reviewed URI launcher cannot deserialize arbitrary Android intents`() {
        val source = projectFile("app/src/main/java/ai/zara/app/device/AndroidUriLauncher.kt").readText()

        assertTrue(source.contains("Intent(Intent.ACTION_VIEW, Uri.parse(uri))"))
        assertTrue(source.contains("Intent.CATEGORY_BROWSABLE"))
        assertTrue(source.contains("Intent.FLAG_ACTIVITY_NEW_TASK"))
        assertTrue(source.contains("OpenUriPolicy.normalize(uri)"))

        listOf(
            "Intent.parseUri",
            "setClassName(",
            "setComponent(",
            "ComponentName(",
            "putExtra(",
            "setPackage(",
        ).forEach { forbidden ->
            assertFalse("arbitrary intent escape surface must stay absent: $forbidden", source.contains(forbidden))
        }
    }

    private fun projectFile(relativePath: String): File {
        val cwd = File(System.getProperty("user.dir"))
        val candidates = listOf(
            File(cwd, relativePath),
            File(cwd, "android/$relativePath"),
            File(cwd.parentFile ?: cwd, relativePath),
            File(cwd.parentFile ?: cwd, "android/$relativePath"),
        )
        return candidates.firstOrNull(File::exists) ?: candidates.first()
    }
}
