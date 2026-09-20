package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PureSymbolicChatWiringContractTest {
    private fun activity(): String = File("src/main/java/ai/zara/app/MainActivity.kt").readText()

    @Test
    fun realChatOwnsPersistedExecutionPolicyAndPureSymbolicController() {
        val source = activity()

        assertTrue(source.contains("ConversationExecutionPolicyStore("))
        assertTrue(source.contains("conversation-execution-policy.bin"))
        assertTrue(source.contains("AndroidPureSymbolicConversationFactory.create(appSession)::submit"))
        assertTrue(source.contains("ConversationExecutionPolicyController("))
    }

    @Test
    fun pureSymbolicRouteFencesExistingProviderCapableSubmitPathBehindLazySupplier() {
        val source = activity()
        val send = source.substringAfter("onSendText = { text, conversation, project ->")
            .substringBefore("onCreateProject = { name ->")
        val policySubmit = send.substringAfter("executionPolicyController.submit(")

        assertTrue(policySubmit.contains("standardTurn = {"))
        assertTrue(policySubmit.contains("appSession.submitText("))
        assertTrue(policySubmit.contains("appSession.submitProjectText("))
        assertFalse(send.substringBefore("executionPolicyController.submit(").contains("appSession.submitText("))
        assertFalse(send.substringBefore("executionPolicyController.submit(").contains("appSession.submitProjectText("))
    }

    @Test
    fun chatCanEnableAndDisablePersistedPureSymbolicPolicyWithoutProviderTurn() {
        val source = activity()
        val send = source.substringAfter("onSendText = { text, conversation, project ->")
            .substringBefore("onCreateProject = { name ->")

        assertTrue(send.contains("\"/symbolic on\" -> ConversationExecutionPolicy.PURE_SYMBOLIC"))
        assertTrue(send.contains("\"/symbolic off\" -> ConversationExecutionPolicy.STANDARD"))
        assertTrue(send.contains("executionPolicyController.select(requestedPolicy)"))
        assertTrue(send.contains("max_model_calls=0"))
        assertTrue(send.contains("max_provider_calls=0"))
    }
}
