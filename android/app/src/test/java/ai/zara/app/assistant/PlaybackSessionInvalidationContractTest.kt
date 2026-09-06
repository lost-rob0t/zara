package ai.zara.app.assistant

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class PlaybackSessionInvalidationContractTest {
    @Test
    fun `authenticated session loss invalidates playback before runtime publication`() {
        val source = projectFile("app/src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        val observer = source.substringAfter("private fun observeRuntimeState(state: RuntimeState) {")
            .substringBefore("private fun clearVoiceOwnershipIfIdle")

        assertTrue(observer.contains("ServerConnection.Connected"))
        assertTrue(observer.contains("voiceStreamSink.reset()"))
        assertTrue(
            "playback invalidation must be requested before runtime state reaches UI observers",
            observer.indexOf("voiceStreamSink.reset()") < observer.indexOf("runtimeStateObserver?.invoke(state)"),
        )
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
