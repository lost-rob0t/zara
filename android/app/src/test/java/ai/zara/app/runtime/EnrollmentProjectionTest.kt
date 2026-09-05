package ai.zara.app.runtime

import ai.zara.app.auth.EnrollmentState
import org.junit.Assert.assertEquals
import org.junit.Test

class EnrollmentProjectionTest {

    @Test fun `runtime exposes enrollment readiness without carrying key material`() {
        assertEquals(EnrollmentReadiness.Unenrolled, EnrollmentState.Unenrolled.toRuntimeReadiness())
        assertEquals(
            EnrollmentReadiness.AwaitingServerPin,
            EnrollmentState.AwaitingServerPin(ByteArray(32) { 1 }).toRuntimeReadiness(),
        )
        assertEquals(
            EnrollmentReadiness.Ready,
            EnrollmentState.Ready(ByteArray(32) { 2 }).toRuntimeReadiness(),
        )
        assertEquals(
            EnrollmentReadiness.Corrupt,
            EnrollmentState.Corrupt("bad credential").toRuntimeReadiness(),
        )
    }

    @Test fun `enrollment observation does not change connection session`() {
        val state = RuntimeState.initial()
        val observed = reduce(state, RuntimeEvent.EnrollmentObserved(EnrollmentReadiness.Ready))
        assertEquals(EnrollmentReadiness.Ready, observed.enrollment)
        assertEquals(ServerConnection.Disconnected, observed.server)
        assertEquals(null, observed.sessionId)
    }
}
