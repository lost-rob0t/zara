package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * P0 cancellation-durability contract for pure-symbolic Android turns.
 *
 * A caller-visible cancelled future is a terminal claim. The canonical zara.db projection/history
 * fence must therefore win (or restart recovery must be proven to have already taken ownership)
 * before the future is marked cancelled. Swallowing an arbitrary persistence failure can leave the
 * canonical assistant row/projection pending while the UI believes the turn is gone.
 */
class AndroidSymbolicCancellationDurabilityContractTest {
    @Test
    fun `cancellation persists canonical terminal state before publishing cancelled future`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()
        val startMarker =
            "override fun cancel(mayInterruptIfRunning: Boolean): Boolean = synchronized(fence) {"
        val cancelStart = factory.indexOf(startMarker)
        val nextMethod = factory.indexOf(
            "\n    internal fun dialogueTurnQuery(",
            startIndex = cancelStart.coerceAtLeast(0),
        )

        assertTrue("persistence-fenced cancellation implementation disappeared", cancelStart >= 0)
        assertTrue("cancellation contract could not find the next factory method boundary", nextMethod > cancelStart)
        val cancellation = factory.substring(cancelStart, nextMethod)

        assertFalse(
            "cancellation must not swallow arbitrary zara.db terminalization failures",
            cancellation.contains("runCatching(onCancel)"),
        )
        val persistIndex = cancellation.indexOf("onCancel()")
        val publishIndex = cancellation.indexOf("super.cancel(")
        assertTrue("cancellation must invoke the durable terminal fence", persistIndex >= 0)
        assertTrue("cancellation must publish Future cancellation only after durable fencing", publishIndex > persistIndex)
    }

    @Test
    fun `stale restart ownership is handled explicitly instead of broad exception swallowing`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()

        assertTrue(
            "restart-lost cancellation must have an explicit stale-owner path",
            factory.contains("cancelPersistedTurnIfOwned("),
        )
        assertTrue(
            "stale-owner handling must re-read the canonical projection",
            factory.contains("loadSymbolicProjection(pendingProjection.conversationId)"),
        )
        assertTrue(
            "only a proven ownership loss may suppress a stale cancellation write",
            factory.contains("stillOwnsPendingGeneration"),
        )
    }
}
