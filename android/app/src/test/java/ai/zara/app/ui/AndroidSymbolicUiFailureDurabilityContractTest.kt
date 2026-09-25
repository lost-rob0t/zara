package ai.zara.app.ui

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * P0 durability contract for the Activity-side pure-symbolic failure path.
 *
 * The canonical zara.db assistant row must never be left Running while the UI silently accepts a
 * failed terminal write. Factory-side failure fencing cannot cover every Activity-side failure
 * (for example an explicit symbolic route or a failed idempotence settlement), so recordTurnFailure
 * must surface a CanonicalConversationStore.failTurn persistence failure instead of swallowing it.
 */
class AndroidSymbolicUiFailureDurabilityContractTest {
    @Test
    fun `activity must not swallow canonical failure terminalization errors`() {
        val source = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        val failureHandler = source
            .substringAfter("private fun recordTurnFailure(", "")
            .substringBefore("\n    private fun ", "")

        assertTrue("recordTurnFailure disappeared", failureHandler.isNotEmpty())
        assertTrue(
            "Activity failure handling must terminalize the exact canonical turn",
            failureHandler.contains("conversationStore.failTurn(") &&
                failureHandler.contains("expectedTurnId = expectedTurnId"),
        )
        assertFalse(
            "canonical failTurn persistence failure must not be silently swallowed",
            failureHandler.contains("catch (_: Exception) {\n        }"),
        )
        assertTrue(
            "a failed canonical terminal write must be surfaced to the UI instead of false-green completion",
            failureHandler.contains("catch (storeError: Exception)") &&
                failureHandler.contains("operationError = UiOperationFailure.summarize(storeError)"),
        )
    }
}
