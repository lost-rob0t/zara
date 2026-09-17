package ai.zara.app.email

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidEmailPluginContractTest {
    @Test
    fun modelContextPublishesToolsAndPrologSymbols() {
        val context = AndroidEmailPlugin.MODEL_CONTEXT

        assertTrue(context.contains("email_search"))
        assertTrue(context.contains("email_send"))
        assertTrue(context.contains("email_before_send_rule/6"))
        assertTrue(context.contains("email_spam_rule/6"))
        assertTrue(context.contains("untrusted data"))
    }

    @Test
    fun providerContractIncludesGmailImapAndPop3() {
        assertEquals(
            setOf(EmailProviderKind.GMAIL, EmailProviderKind.IMAP, EmailProviderKind.POP3),
            EmailProviderKind.entries.toSet(),
        )
    }

    @Test
    fun accountRejectsUnknownPort() {
        try {
            AndroidEmailAccount(
                id = "mail",
                provider = EmailProviderKind.IMAP,
                address = "a@example.test",
                username = "a@example.test",
                port = 70_000,
            )
            throw AssertionError("expected invalid port")
        } catch (_: IllegalArgumentException) {
            // expected
        }
    }
}
