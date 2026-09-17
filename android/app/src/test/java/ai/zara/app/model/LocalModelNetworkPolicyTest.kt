package ai.zara.app.model

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalModelNetworkPolicyTest {
    @Test
    fun `cleartext is disabled globally and enabled only for loopback model traffic`() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        val policy = File("src/main/res/xml/network_security_config.xml").readText()

        assertTrue(manifest.contains("android:networkSecurityConfig=\"@xml/network_security_config\""))
        assertTrue(policy.contains("<base-config cleartextTrafficPermitted=\"false\""))
        assertTrue(policy.contains(">localhost</domain>"))
        assertTrue(policy.contains(">127.0.0.1</domain>"))
        assertTrue(policy.contains(">[::1]</domain>"))
        assertFalse(policy.contains("includeSubdomains=\"true\""))
    }

    @Test
    fun `loopback backend never follows redirects or sends tool definitions`() {
        val backend = File(
            "src/main/java/ai/zara/app/model/LoopbackOpenAiLocalModelBackend.kt"
        ).readText()

        assertTrue(backend.contains("instanceFollowRedirects = false"))
        assertTrue(backend.contains("/v1/chat/completions"))
        assertFalse(backend.contains("put(\"tools\""))
        assertFalse(backend.contains("tool_choice"))
    }
}
