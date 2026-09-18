package ai.zara.app.device

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class OpenAppWiringContractTest {
    @Test
    fun `application session registers executable app_search open_app and open_uri`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("AppSearchAdapter(AndroidAppSearchLauncher(context))"))
        assertTrue(session.contains("OpenUriAdapter(AndroidUriLauncher(context))"))
        assertTrue(session.contains("OpenAppAdapter(AndroidAppLauncher(context))"))
    }
}
