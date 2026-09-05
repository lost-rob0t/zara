package ai.zara.app.ui

import java.io.IOException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test

class UiOperationFailureTest {
    @Test
    fun `raw secret bearing messages never escape`() {
        val failure = IllegalStateException(
            "tcp://10.0.0.5:5555 token=super-secret /data/user/0/ai.zara/secret.key"
        )

        val rendered = UiOperationFailure.summarize(failure)

        assertEquals("operation_failed", rendered)
        assertFalse(rendered.contains("10.0.0.5"))
        assertFalse(rendered.contains("super-secret"))
        assertFalse(rendered.contains("secret.key"))
    }

    @Test
    fun `stable categories preserve useful failure shape without messages`() {
        assertEquals("permission_denied", UiOperationFailure.summarize(SecurityException("camera denied")))
        assertEquals("invalid_input", UiOperationFailure.summarize(IllegalArgumentException("bad endpoint")))
        assertEquals("network_error", UiOperationFailure.summarize(IOException("host=private.internal")))
    }

    @Test
    fun `cause traversal is bounded and cycle safe`() {
        val outer = RuntimeException("outer")
        val inner = IOException("private endpoint")
        outer.initCause(inner)

        assertEquals("network_error", UiOperationFailure.summarize(outer))
    }
}
