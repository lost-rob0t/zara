package ai.zara.app.ui

import ai.zara.app.runtime.EnrollmentReadiness
import ai.zara.app.runtime.ServerConnection
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ZaraAppProjectionTest {
    @Test
    fun connectionLabelsRemainHonestAcrossCanonicalReducerStates() {
        assertEquals("disconnected", connectionLabel(ServerConnection.Disconnected))
        assertEquals("connecting", connectionLabel(ServerConnection.Connecting(1)))
        assertEquals("connected", connectionLabel(ServerConnection.Connected(2)))
        assertEquals(
            "reconnecting (attempt 3)",
            connectionLabel(ServerConnection.Reconnecting(4, 3)),
        )
        assertEquals(
            "offline (network unavailable)",
            connectionLabel(ServerConnection.OfflineDegraded(5, "network unavailable")),
        )
    }

    @Test
    fun enrollmentLabelsDoNotInventAuthenticatedState() {
        assertEquals("unenrolled", enrollmentLabel(EnrollmentReadiness.Unenrolled))
        assertEquals(
            "awaiting server pin",
            enrollmentLabel(EnrollmentReadiness.AwaitingServerPin),
        )
        assertEquals("ready", enrollmentLabel(EnrollmentReadiness.Ready))
        assertEquals("corrupt", enrollmentLabel(EnrollmentReadiness.Corrupt))
    }

    @Test
    fun connectControlCannotStartParallelConnectionLifecycle() {
        assertTrue(canRequestConnect(ServerConnection.Disconnected))
        assertTrue(canRequestConnect(ServerConnection.OfflineDegraded(3, "network unavailable")))
        assertFalse(canRequestConnect(ServerConnection.Connecting(4)))
        assertFalse(canRequestConnect(ServerConnection.Connected(4)))
        assertFalse(canRequestConnect(ServerConnection.Reconnecting(5, 2)))
    }
}
