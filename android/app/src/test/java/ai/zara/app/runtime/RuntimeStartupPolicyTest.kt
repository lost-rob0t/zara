package ai.zara.app.runtime

import ai.zara.app.ui.ConversationExecutionPolicy
import org.junit.Assert.*
import org.junit.Test

class RuntimeStartupPolicyTest {
    @Test
    fun localBootNeverRestoresSavedRemoteSession() {
        val policy = RuntimeStartupPolicy(RuntimeMode.Local, ConversationExecutionPolicy.STANDARD)
        assertFalse(policy.restoreRemote)
        assertTrue(policy.loadLocalModel)
    }

    @Test
    fun symbolicBootDoesNotInitializeAnyModelOrTransportEvenWithRemotePreference() {
        RuntimeMode.entries.forEach { mode ->
            val policy = RuntimeStartupPolicy(mode, ConversationExecutionPolicy.PURE_SYMBOLIC)
            assertFalse(policy.restoreRemote)
            assertFalse(policy.loadLocalModel)
        }
    }

    @Test
    fun explicitRemoteAndAutoRetainSavedConnectionSupport() {
        assertTrue(RuntimeStartupPolicy(RuntimeMode.Remote, ConversationExecutionPolicy.STANDARD).restoreRemote)
        assertFalse(RuntimeStartupPolicy(RuntimeMode.Remote, ConversationExecutionPolicy.STANDARD).loadLocalModel)
        assertTrue(RuntimeStartupPolicy(RuntimeMode.Auto, ConversationExecutionPolicy.STANDARD).restoreRemote)
    }
}
