package ai.zara.app.ui

import ai.zara.app.ZaraApplication
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.ServerConnection
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.OutlinedTextFieldDefaults
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp

@Composable
fun ScheduledSurface(
    lastTurn: RenderedTextTurn?,
    operationError: String?,
    operationBusy: Boolean,
    onSendText: (String) -> Unit,
    padding: PaddingValues,
) {
    val tokens = LocalZaraTokens.current
    val appSession = (LocalContext.current.applicationContext as? ZaraApplication)?.appSession
    val runtimeMode = appSession?.runtimeMode() ?: RuntimeMode.Local
    val remoteConnected = appSession?.state()?.server is ServerConnection.Connected
    val schedulerAvailable = runtimeMode != RuntimeMode.Local && remoteConnected
    var cron by rememberSaveable { mutableStateOf("0 9 * * 1-5") }
    var goal by rememberSaveable { mutableStateOf("") }
    var scheduleId by rememberSaveable { mutableStateOf("") }

    val fieldColors = OutlinedTextFieldDefaults.colors(
        focusedBorderColor = tokens.borderActive,
        unfocusedBorderColor = tokens.border,
        focusedTextColor = tokens.text,
        unfocusedTextColor = tokens.text,
        focusedLabelColor = tokens.primary,
        unfocusedLabelColor = tokens.textMuted,
        cursorColor = tokens.primary,
    )

    Column(
        modifier = Modifier
            .fillMaxSize()
            .padding(padding)
            .padding(horizontal = 18.dp, vertical = 12.dp)
            .verticalScroll(rememberScrollState()),
        verticalArrangement = Arrangement.spacedBy(10.dp),
    ) {
        Text("Scheduled", color = tokens.text)
        Text(
            "Cron / interval → Prolog → LLM escalation. Runs are owned by the canonical Zara runtime.",
            color = tokens.textMuted,
        )
        Text(
            when {
                runtimeMode == RuntimeMode.Local ->
                    "Local mode: schedule controls are disabled. Switch to Auto or Remote and connect to the canonical Zara runtime."
                !remoteConnected ->
                    "Degraded: no authenticated Zara server is connected. Schedules are not executed on Android."
                else ->
                    "Canonical runtime connected. Schedule tools use its configured [tasks] service."
            },
            color = if (schedulerAvailable) tokens.secondary else tokens.error,
        )

        OutlinedTextField(
            value = cron,
            onValueChange = { cron = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Cron / interval") },
            placeholder = { Text("0 9 * * 1-5  or  @every 6h") },
            singleLine = true,
            colors = fieldColors,
            textStyle = androidx.compose.ui.text.TextStyle(fontFamily = FontFamily.Monospace),
        )
        OutlinedTextField(
            value = goal,
            onValueChange = { goal = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Task") },
            minLines = 2,
            maxLines = 5,
            colors = fieldColors,
        )
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            Button(
                enabled = schedulerAvailable && !operationBusy && cron.isNotBlank() && goal.isNotBlank(),
                onClick = {
                    onSendText(
                        "Use schedule_create with cron \"${cron.trim()}\", mode \"auto\", " +
                            "and this goal: ${goal.trim()}"
                    )
                },
                colors = ButtonDefaults.buttonColors(containerColor = tokens.primary),
            ) {
                Text("Create schedule")
            }
            TextButton(
                enabled = schedulerAvailable && !operationBusy,
                onClick = { onSendText("Use schedule_list. List schedules concisely with id, recurrence, state, next run, and last outcome.") },
            ) {
                Text("List schedules", color = tokens.secondary)
            }
        }

        OutlinedTextField(
            value = scheduleId,
            onValueChange = { scheduleId = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Schedule ID") },
            singleLine = true,
            colors = fieldColors,
            textStyle = androidx.compose.ui.text.TextStyle(fontFamily = FontFamily.Monospace),
        )
        Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
            TextButton(
                enabled = schedulerAvailable && !operationBusy && scheduleId.isNotBlank(),
                onClick = { onSendText("Use schedule_pause for schedule id \"${scheduleId.trim()}\".") },
            ) { Text("Pause", color = tokens.text) }
            TextButton(
                enabled = schedulerAvailable && !operationBusy && scheduleId.isNotBlank(),
                onClick = { onSendText("Use schedule_resume for schedule id \"${scheduleId.trim()}\".") },
            ) { Text("Resume", color = tokens.text) }
            TextButton(
                enabled = schedulerAvailable && !operationBusy && scheduleId.isNotBlank(),
                onClick = { onSendText("Use schedule_cancel for schedule id \"${scheduleId.trim()}\".") },
            ) { Text("Cancel", color = tokens.error) }
        }

        if (lastTurn != null) {
            Surface(
                modifier = Modifier.fillMaxWidth(),
                color = tokens.surface,
                border = androidx.compose.foundation.BorderStroke(1.dp, tokens.border),
            ) {
                Text(
                    lastTurn.assistantText,
                    modifier = Modifier.padding(12.dp),
                    color = tokens.text,
                )
            }
        }
        if (operationError != null) {
            Text(operationError, color = tokens.error)
        }
    }
}
