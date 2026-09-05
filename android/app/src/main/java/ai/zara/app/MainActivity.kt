package ai.zara.app

import ai.zara.app.runtime.AssistantRole
import ai.zara.app.runtime.RuntimeState
import ai.zara.app.runtime.ServerConnection
import android.app.Activity
import android.os.Bundle
import android.widget.LinearLayout
import android.widget.TextView

class MainActivity : Activity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        val state = RuntimeState.initial()

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

        layout.addView(title)
        layout.addView(status)
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
        return "server: $server\nassistant role: $role"
    }
}
