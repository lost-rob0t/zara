package ai.zara.app.device

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class OpenAppWiringContractTest {
    @Test
    fun `application session registers executable open_app beside open_uri`() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()

        assertTrue(session.contains("OpenUriAdapter(AndroidUriLauncher(context))"))
        assertTrue(session.contains("OpenAppAdapter(AndroidAppLauncher(context))"))
    }
}
