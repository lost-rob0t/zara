package ai.zara.app.email

import ai.zara.app.prolog.TreallaBridge
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
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
        assertTrue(context.contains("email_tool/3"))
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

    @Test
    fun treallaRuleQueriesAlwaysBindJniResultVariable() {
        val bridge = RecordingBridge()
        val engine = TreallaEmailRuleEngine(bridge)

        bridge.results = listOf("deny")
        val (allowed, _) = engine.beforeSend("work", "x@example.test", "subject", "body")
        assertFalse(allowed)
        assertTrue(bridge.queries.last().contains("Result = Decision"))

        bridge.results = listOf("20", "35")
        val (score, reasons) = engine.spamScore("x@bad.example", "bad.example", "subject", "body")
        assertEquals(55, score)
        assertTrue(reasons.isEmpty())
        assertTrue(bridge.queries.last().contains("Result = Score"))

        bridge.results = listOf("archive")
        val action = engine.afterReceive(
            "work",
            AndroidEmailMessage("1", "x@example.test", "me@example.test", "subject", "today"),
            55,
        )
        assertEquals("archive", action)
        assertTrue(bridge.queries.last().contains("Result = Action"))
    }

    private class RecordingBridge : TreallaBridge {
        var results: List<String> = emptyList()
        val queries = mutableListOf<String>()

        override fun initialize(coreAssetPath: String) = Unit
        override fun consult(sourcePath: String) = Unit
        override fun evaluate(query: String): List<String> {
            queries += query
            return results
        }
        override fun shutdown() = Unit
    }
}
