package ai.zara.app.ui

import ai.zara.app.auth.AuthenticationException
import ai.zara.app.runtime.TextRequestTimeoutException
import ai.zara.app.runtime.ZaraWireException
import java.io.IOException
import java.util.concurrent.CompletionException
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

    @Test
    fun `connection failures identify the handshake stage without exposing messages`() {
        assertEquals(
            "server_hello_timeout",
            UiOperationFailure.summarize(
                CompletionException(TextRequestTimeoutException("ZARA/1 voice hello timed out"))
            ),
        )
        assertEquals(
            "capability_negotiation_timeout",
            UiOperationFailure.summarize(
                TextRequestTimeoutException("ZARA/1 capability negotiation timed out")
            ),
        )
        assertEquals(
            "request_timeout",
            UiOperationFailure.summarize(TextRequestTimeoutException("token=private")),
        )
        assertEquals(
            "authentication_failed",
            UiOperationFailure.summarize(AuthenticationException("secret=private")),
        )
        assertEquals(
            "protocol_error",
            UiOperationFailure.summarize(ZaraWireException("secret=private")),
        )
    }
}
