package ai.zara.app.peer

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class PeerNodeListenerLifecycleTest {
    private val identity = PeerNodeIdentity(
        nodeId = "phone-01",
        curvePublicKeyZ85 = "A".repeat(40),
        enrollmentGeneration = 7,
    )

    @Test
    fun listenerRestartKeepsIdentityButAdvancesGeneration() {
        val lifecycle = PeerNodeListenerLifecycle(identity)

        val first = lifecycle.requestStart(identity)
        assertTrue(lifecycle.listenerStarted(first, listOf("tcp://192.0.2.10:17865")))
        val stop = lifecycle.requestStop()
        assertTrue(lifecycle.listenerStopped(requireNotNull(stop)))

        val second = lifecycle.requestStart(identity)
        assertTrue(second > first)
        assertTrue(lifecycle.listenerStarted(second, listOf("tcp://192.0.2.11:17865")))

        val snapshot = lifecycle.snapshot()
        assertEquals(PeerListenerPhase.ACTIVE, snapshot.phase)
        assertEquals(identity, snapshot.identity)
        assertEquals(listOf("tcp://192.0.2.11:17865"), snapshot.endpoints)
    }

    @Test
    fun stopFencesLateStartCompletion() {
        val lifecycle = PeerNodeListenerLifecycle(identity)
        val start = lifecycle.requestStart(identity)
        val stop = requireNotNull(lifecycle.requestStop())

        assertFalse(lifecycle.listenerStarted(start, listOf("tcp://192.0.2.10:17865")))
        assertTrue(lifecycle.listenerStopped(stop))
        assertEquals(PeerListenerPhase.STOPPED, lifecycle.snapshot().phase)
        assertTrue(lifecycle.snapshot().endpoints.isEmpty())
    }

    @Test
    fun networkChangeOnlyUpdatesCurrentActiveGeneration() {
        val lifecycle = PeerNodeListenerLifecycle(identity)
        val generation = lifecycle.requestStart(identity)
        assertTrue(lifecycle.listenerStarted(generation, listOf("tcp://192.0.2.10:17865")))

        assertFalse(
            lifecycle.networkChanged(
                generation - 1,
                listOf("tcp://198.51.100.20:17865"),
            ),
        )
        assertTrue(
            lifecycle.networkChanged(
                generation,
                listOf("tcp://198.51.100.20:17865"),
            ),
        )

        val snapshot = lifecycle.snapshot()
        assertEquals(identity, snapshot.identity)
        assertEquals(listOf("tcp://198.51.100.20:17865"), snapshot.endpoints)
    }

    @Test
    fun processRecreationWithPersistedIdentityStartsStopped() {
        val lifecycle = PeerNodeListenerLifecycle(identity)

        val snapshot = lifecycle.snapshot()
        assertEquals(PeerListenerPhase.STOPPED, snapshot.phase)
        assertEquals(identity, snapshot.identity)
        assertTrue(snapshot.endpoints.isEmpty())
        assertNull(snapshot.failure)
    }

    @Test
    fun staleFailureCannotKillNewGeneration() {
        val lifecycle = PeerNodeListenerLifecycle(identity)
        val first = lifecycle.requestStart(identity)
        val stop = requireNotNull(lifecycle.requestStop())
        assertTrue(lifecycle.listenerStopped(stop))
        val second = lifecycle.requestStart(identity)

        assertFalse(lifecycle.listenerFailed(first, "late bind failure"))
        assertTrue(lifecycle.listenerStarted(second, listOf("tcp://192.0.2.10:17865")))
        assertEquals(PeerListenerPhase.ACTIVE, lifecycle.snapshot().phase)
    }

    @Test
    fun listenerRejectsHiddenHttpFallbackAndUnboundedEndpoints() {
        val lifecycle = PeerNodeListenerLifecycle(identity)
        val generation = lifecycle.requestStart(identity)

        assertThrows(IllegalArgumentException::class.java) {
            lifecycle.listenerStarted(generation, listOf("https://example.invalid/zara"))
        }
        assertThrows(IllegalArgumentException::class.java) {
            lifecycle.listenerStarted(
                generation,
                (0..8).map { "tcp://192.0.2.${it + 1}:17865" },
            )
        }
    }

    @Test
    fun identityCannotRotateInsideListenerLifecycle() {
        val lifecycle = PeerNodeListenerLifecycle(identity)
        val other = identity.copy(nodeId = "phone-02")

        assertThrows(IllegalStateException::class.java) {
            lifecycle.requestStart(other)
        }
    }

    @Test
    fun sameActiveStartIsIdempotent() {
        val lifecycle = PeerNodeListenerLifecycle(identity)
        val generation = lifecycle.requestStart(identity)
        assertTrue(lifecycle.listenerStarted(generation, listOf("tcp://192.0.2.10:17865")))

        assertEquals(generation, lifecycle.requestStart(identity))
        assertEquals(PeerListenerPhase.ACTIVE, lifecycle.snapshot().phase)
    }
}
