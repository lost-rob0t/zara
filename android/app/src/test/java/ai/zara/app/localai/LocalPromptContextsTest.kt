package ai.zara.app.localai

import ai.zara.app.email.AndroidEmailPlugin
import org.junit.After
import org.junit.Assert.assertTrue
import org.junit.Test

class LocalPromptContextsTest {
    @After
    fun cleanup() {
        LocalPromptContexts.clearForTests()
    }

    @Test
    fun emailPrologSymbolsArePrependedToLocalModelTurns() {
        LocalPromptContexts.register("email-prolog-api", AndroidEmailPlugin.MODEL_CONTEXT)

        val request = LocalPromptContexts.apply(LocalGenerationRequest("summarize my inbox"))

        assertTrue(request.prompt.contains("email_send"))
        assertTrue(request.prompt.contains("email_spam_rule/6"))
        assertTrue(request.prompt.contains("summarize my inbox"))
        assertTrue(request.prompt.indexOf("email_send") < request.prompt.indexOf("summarize my inbox"))
    }

    @Test
    fun registryIsDeterministicAndReplaceableByOwnerName() {
        LocalPromptContexts.register("b", "second")
        LocalPromptContexts.register("a", "first")
        LocalPromptContexts.register("a", "first-updated")

        val prompt = LocalPromptContexts.apply(LocalGenerationRequest("user")).prompt

        assertTrue(prompt.indexOf("first-updated") < prompt.indexOf("second"))
        assertTrue(!prompt.contains("\nfirst\n"))
    }
}
