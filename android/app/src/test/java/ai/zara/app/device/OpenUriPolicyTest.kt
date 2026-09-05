package ai.zara.app.device

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class OpenUriPolicyTest {
    @Test
    fun `https and http URIs normalize without exposing credentials`() {
        assertEquals(
            "https://example.com/path?q=1",
            OpenUriPolicy.normalize("https://example.com/path?q=1"),
        )
        assertEquals(
            "http://example.com/",
            OpenUriPolicy.normalize("http://example.com"),
        )
    }

    @Test
    fun `normalization preserves valid percent encoding instead of double escaping it`() {
        assertEquals(
            "https://example.com/a%20b?q=x%2Fy",
            OpenUriPolicy.normalize("https://example.com/a%20b?q=x%2Fy"),
        )
    }

    @Test
    fun `unsupported or authority-free schemes fail closed`() {
        listOf(
            "file:///sdcard/private.txt",
            "javascript:alert(1)",
            "intent://example.com/#Intent;scheme=https;end",
            "data:text/plain,secret",
            "https:/missing-host",
            "https://",
        ).forEach { value ->
            assertThrows(IllegalArgumentException::class.java) {
                OpenUriPolicy.normalize(value)
            }
        }
    }

    @Test
    fun `userinfo control characters fragments and oversized URIs fail closed`() {
        listOf(
            "https://user:password@example.com/",
            "https://example.com/a\nheader",
            "https://example.com/#fragment",
            "https://example.com/${"a".repeat(OpenUriPolicy.MAX_URI_BYTES)}",
        ).forEach { value ->
            assertThrows(IllegalArgumentException::class.java) {
                OpenUriPolicy.normalize(value)
            }
        }
    }

    @Test
    fun `blank URI fails closed`() {
        assertThrows(IllegalArgumentException::class.java) {
            OpenUriPolicy.normalize("   ")
        }
    }
}
