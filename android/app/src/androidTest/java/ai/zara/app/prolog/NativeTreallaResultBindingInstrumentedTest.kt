package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession
import ai.zara.app.runtime.LocalServerPhase
import android.os.SystemClock
import androidx.test.platform.app.InstrumentationRegistry
import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Test

class NativeTreallaResultBindingInstrumentedTest {
    @Test
    fun nativeBridgeCopiesStringSolutionsBeforeRedo() {
        val context = InstrumentationRegistry.getInstrumentation().targetContext
        val session = AndroidAppSession(context)
        try {
            awaitLocalServerReady(session)
            val result = session.queryLocalProlog(
                "(Result = \"first\" ; Result = \"second\")"
            ).get(QUERY_TIMEOUT_SECONDS, TimeUnit.SECONDS)

            assertEquals(listOf("first", "second"), result.terms)
        } finally {
            session.close()
        }
    }

    private fun awaitLocalServerReady(session: AndroidAppSession) {
        val deadline = SystemClock.elapsedRealtime() + SERVER_TIMEOUT_MILLIS
        while (SystemClock.elapsedRealtime() < deadline) {
            val state = session.localServerState()
            if (state.phase == LocalServerPhase.READY) return
            check(state.phase != LocalServerPhase.FAILED) {
                "Local symbolic server failed during instrumentation: ${state.failure}"
            }
            SystemClock.sleep(50)
        }
        error("Timed out waiting for local symbolic server: ${session.localServerState()}")
    }

    private companion object {
        const val QUERY_TIMEOUT_SECONDS = 15L
        const val SERVER_TIMEOUT_MILLIS = 20_000L
    }
}
