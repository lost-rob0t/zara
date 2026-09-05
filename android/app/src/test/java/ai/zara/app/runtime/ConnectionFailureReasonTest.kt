package ai.zara.app.runtime

import java.io.File
import java.io.IOException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ConnectionFailureReasonTest {
    @Test
    fun `connection exceptions never expose raw private messages`() {
        val error = IllegalStateException(
            "tcp://10.0.0.8:5555 token=private /data/user/0/ai.zara/curve.key"
        )

        val reason = ConnectionFailureReason.summarize(error)

        assertEquals("connection_failed", reason)
        assertFalse(reason.contains("10.0.0.8"))
        assertFalse(reason.contains("private"))
        assertFalse(reason.contains("curve.key"))
    }

    @Test
    fun `connection failure categories are stable and message free`() {
        assertEquals("permission_denied", ConnectionFailureReason.summarize(SecurityException("secret")))
        assertEquals("network_error", ConnectionFailureReason.summarize(IOException("private.internal")))
    }

    @Test
    fun `nested network failure is classified with bounded cause traversal`() {
        val outer = RuntimeException("provider wrapper", IOException("private.internal"))
        assertEquals("network_error", ConnectionFailureReason.summarize(outer))
    }

    @Test
    fun `controller never injects throwable message into reducer state`() {
        val source = File("src/main/java/ai/zara/app/runtime/AndroidTextSessionController.kt").readText()

        assertTrue(source.contains("ConnectionFailureReason.summarize(error)"))
        assertFalse(source.contains("error?.message ?: \"connection failed\""))
    }
}
