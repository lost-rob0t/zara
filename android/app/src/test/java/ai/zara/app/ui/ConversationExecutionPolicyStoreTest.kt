package ai.zara.app.ui

import java.io.File
import kotlin.io.path.createTempDirectory
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class ConversationExecutionPolicyStoreTest {
    @Test
    fun freshStoreDefaultsToStandardWithoutInventingAnotherRuntimeMode() {
        val directory = createTempDirectory("zara-execution-policy").toFile()
        val store = ConversationExecutionPolicyStore(File(directory, "conversation-policy.bin"))

        val policy = store.load()

        assertEquals(ConversationExecutionPolicy.STANDARD, policy)
        assertTrue(policy.providersEnabled)
        assertNull(policy.maxModelCalls)
        assertNull(policy.maxProviderCalls)
    }

    @Test
    fun pureSymbolicSelectionSurvivesProcessRecreationWithHardZeroBudgets() {
        val directory = createTempDirectory("zara-execution-policy").toFile()
        val file = File(directory, "conversation-policy.bin")
        ConversationExecutionPolicyStore(file).save(ConversationExecutionPolicy.PURE_SYMBOLIC)

        val restored = ConversationExecutionPolicyStore(file).load()

        assertEquals(ConversationExecutionPolicy.PURE_SYMBOLIC, restored)
        assertFalse(restored.providersEnabled)
        assertEquals(0, restored.maxModelCalls)
        assertEquals(0, restored.maxProviderCalls)
        assertEquals("v1:pure-symbolic", file.readText())
    }

    @Test
    fun corruptPersistedSelectionFailsClosedToPureSymbolic() {
        val directory = createTempDirectory("zara-execution-policy").toFile()
        val file = File(directory, "conversation-policy.bin")
        file.writeText("garbled-provider-enabling-state")

        val restored = ConversationExecutionPolicyStore(file).load()

        assertEquals(ConversationExecutionPolicy.PURE_SYMBOLIC, restored)
        assertFalse(restored.providersEnabled)
        assertEquals(0, restored.maxModelCalls)
        assertEquals(0, restored.maxProviderCalls)
    }

    @Test
    fun explicitStandardSelectionAlsoSurvivesProcessRecreation() {
        val directory = createTempDirectory("zara-execution-policy").toFile()
        val file = File(directory, "conversation-policy.bin")
        val first = ConversationExecutionPolicyStore(file)
        first.save(ConversationExecutionPolicy.PURE_SYMBOLIC)
        first.save(ConversationExecutionPolicy.STANDARD)

        val restored = ConversationExecutionPolicyStore(file).load()

        assertEquals(ConversationExecutionPolicy.STANDARD, restored)
        assertTrue(restored.providersEnabled)
        assertEquals("v1:standard", file.readText())
    }
}
