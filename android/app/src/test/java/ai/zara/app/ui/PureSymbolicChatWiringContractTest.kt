package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PureSymbolicChatWiringContractTest {
    private fun activity(): String = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

    private fun submitChatTextBlock(source: String): String {
        val marker =
            "val submitChatText: (String, ConversationRecord, ProjectContext?) -> Unit = { text, conversation, project ->"
        check(source.contains(marker)) { "MainActivity must define the canonical submitChatText helper" }
        return source.substringAfter(marker)
            .substringBefore("\n\n            ZaraApp(")
    }

    @Test
    fun realChatOwnsPersistedExecutionPolicyAndCanonicalPureSymbolicController() {
        val source = activity()

        assertTrue(source.contains("ConversationExecutionPolicyStore("))
        assertTrue(source.contains("conversation-execution-policy.bin"))
        assertTrue(source.contains("val portableConversationStore = PortableConversationStore(this)"))
        assertTrue(source.contains("AndroidPureSymbolicConversationFactory.create("))
        assertTrue(source.contains("portableConversationStore,"))
        assertTrue(source.contains("ConversationExecutionPolicyController("))
        assertTrue(
            source.contains(
                "onSendText = { text, conversation, project -> submitChatText(text, conversation, project) }"
            )
        )
        assertFalse(source.contains("ConversationStore(File(filesDir, \"conversations.bin\"))"))
    }

    @Test
    fun pureSymbolicRouteUsesCanonicalConversationIdAndFencesProviderSubmitBehindLazySupplier() {
        val source = activity()
        val send = submitChatTextBlock(source)
        val policySubmit = send.substringAfter("executionPolicyController.submit(")

        assertTrue(policySubmit.contains("conversationId = conversation.id"))
        assertTrue(policySubmit.contains("standardTurn = {"))
        assertTrue(policySubmit.contains("appSession.submitText("))
        assertTrue(policySubmit.contains("appSession.submitProjectText("))
        assertFalse(send.substringBefore("executionPolicyController.submit(").contains("appSession.submitText("))
        assertFalse(send.substringBefore("executionPolicyController.submit(").contains("appSession.submitProjectText("))
        assertTrue(send.contains("executionPolicy == ConversationExecutionPolicy.PURE_SYMBOLIC"))
    }

    @Test
    fun chatCanEnableAndDisablePersistedPureSymbolicPolicyWithoutProviderTurn() {
        val source = activity()
        val send = submitChatTextBlock(source)

        assertTrue(send.contains("\"/symbolic on\" -> ConversationExecutionPolicy.PURE_SYMBOLIC"))
        assertTrue(send.contains("\"/symbolic off\" -> ConversationExecutionPolicy.STANDARD"))
        assertTrue(send.contains("executionPolicyController.selectAfterCanonicalCommit(requestedPolicy)"))
        assertTrue(send.contains("max_model_calls=0"))
        assertTrue(send.contains("max_provider_calls=0"))
    }
}
