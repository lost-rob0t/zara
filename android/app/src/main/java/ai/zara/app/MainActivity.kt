package ai.zara.app

import ai.zara.app.auth.AndroidEnrollmentRepository
import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.ClientStateStore
import ai.zara.app.runtime.RuntimeEvent
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import ai.zara.app.runtime.reduce
import ai.zara.app.runtime.toRuntimeReadiness
import android.app.Activity
import android.os.Bundle
import android.widget.LinearLayout
import android.widget.TextView
import java.io.File

class MainActivity : Activity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        val restored = ClientStateStore(File(noBackupFilesDir, "zara/client-state.bin")).load()
        var state = restored?.let(RuntimeState::fromRestored) ?: RuntimeState.initial()
        val enrollment = AndroidEnrollmentRepository.create(this).state().toRuntimeReadiness()
        state = reduce(state, RuntimeEvent.EnrollmentObserved(enrollment))

        val layout = LinearLayout(this).apply {
            orientation = LinearLayout.VERTICAL
            setPadding(64, 128, 64, 64)
        }

        val title = TextView(this).apply {
            text = "Zara"
            textSize = 28f
        }

        val status = TextView(this).apply {
            text = describe(state)
            textSize = 15f
        }

        val diagnostics = TextView(this).apply {
            text = "source: ${BuildConfig.SOURCE_SHA}"
            textSize = 12f
        }

        layout.addView(title)
        layout.addView(status)
        layout.addView(diagnostics)
        setContentView(layout)
    }

    private fun describe(state: RuntimeState): String {
        val server = when (val connection = state.server) {
            is ServerConnection.Disconnected -> "disconnected"
            is ServerConnection.Connecting -> "connecting"
            is ServerConnection.Connected -> "connected"
            is ServerConnection.Reconnecting -> "reconnecting (attempt ${connection.attempt})"
            is ServerConnection.OfflineDegraded -> "offline degraded (${connection.reason})"
        }
        val role = when (state.assistantRole) {
            is AssistantRole.NotYetAssessed -> "not yet assessed"
            is AssistantRole.Held -> "held"
            is AssistantRole.NotHeld -> "not held"
            is AssistantRole.PlatformUnavailable -> "platform unavailable"
        }
        return "server: $server\nenrollment: ${state.enrollment}\nassistant role: $role"
    }
}
