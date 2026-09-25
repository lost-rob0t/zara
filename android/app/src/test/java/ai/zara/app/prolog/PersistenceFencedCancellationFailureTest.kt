package ai.zara.app.prolog

import ai.zara.app.runtime.LocalQueryResult
import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * Cancellation must reach in-flight symbolic work even when durable cancellation persistence fails.
 *
 * The persistence callback is intentionally failed here. A storage failure must never leave the
 * conversation-facing future live or allow its canonical expert/query upstream to keep running.
 * This is a behavioral regression over the real factory cancellation wrapper, not a source-shape
 * assertion, and it adds no second cancellation or persistence authority.
 */
class PersistenceFencedCancellationFailureTest {
    @Test
    fun persistenceFailureCannotPreventUpstreamCancellationFence() {
        val upstream = CompletableFuture<LocalQueryResult>()
        var persistenceAttempts = 0
        val persistenceFailure = IllegalStateException("injected persistence failure")
        val output = persistenceFencedFuture(
            upstream = upstream,
            onCancel = {
                persistenceAttempts += 1
                throw persistenceFailure
            },
        )

        val thrown = assertThrows(IllegalStateException::class.java) {
            output.cancel(true)
        }

        assertEquals(persistenceFailure, thrown)
        assertEquals(1, persistenceAttempts)
        assertTrue(
            "conversation cancellation must become terminal even if durable cancellation persistence fails",
            output.isCancelled,
        )
        assertTrue(
            "persistence failure must not strand canonical expert/query work after conversation cancellation",
            upstream.isCancelled,
        )
    }

    @Suppress("UNCHECKED_CAST")
    private fun persistenceFencedFuture(
        upstream: CompletableFuture<LocalQueryResult>,
        onCancel: () -> Unit,
    ): CompletableFuture<LocalQueryResult> {
        val type = Class.forName(
            "ai.zara.app.prolog.AndroidPureSymbolicConversationFactory\$PersistenceFencedFuture",
        )
        val constructor = type.declaredConstructors.single { it.parameterCount == 2 }
        constructor.isAccessible = true
        return constructor.newInstance(upstream, onCancel) as CompletableFuture<LocalQueryResult>
    }
}
