package ai.zara.app.ui

import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.shape.RoundedCornerShape
import androidx.compose.material3.AssistChip
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp

/** Existing chat failure presentation, kept separate from settings integration. */
@Composable
internal fun TurnFailureCard(
    failure: TurnFailure,
    userText: String,
    onRetry: (String) -> Unit,
    onReconnect: () -> Unit,
    onOpenDiagnostics: () -> Unit,
) {
    val tokens = LocalZaraTokens.current
    Surface(
        color = tokens.surface,
        shape = RoundedCornerShape(18.dp),
        border = BorderStroke(1.dp, tokens.error),
        modifier = Modifier.fillMaxWidth(),
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(6.dp),
        ) {
            Text(
                failure.title,
                color = tokens.error,
                fontFamily = FontFamily.Monospace,
                fontWeight = FontWeight.SemiBold,
                fontSize = 12.sp,
                letterSpacing = 1.sp,
            )
            Text(
                failure.explanation,
                color = tokens.text,
                fontFamily = FontFamily.Monospace,
                fontSize = 11.sp,
            )
            Text(
                "Code: ${failure.code}",
                color = tokens.textMuted,
                fontFamily = FontFamily.Monospace,
                fontSize = 10.sp,
            )
            Text(
                "Connection: ${failure.connectionState}  •  Recovery: ${failure.recovery}" +
                    (failure.incidentId?.let { "  •  $it" } ?: ""),
                color = tokens.textMuted,
                fontFamily = FontFamily.Monospace,
                fontSize = 10.sp,
            )
            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                if (failure.retryPossible) {
                    AssistChip(onClick = { onRetry(userText) }, label = { Text("Retry") })
                }
                if (failure.reconnectPossible) {
                    AssistChip(onClick = onReconnect, label = { Text("Reconnect") })
                }
                AssistChip(onClick = onOpenDiagnostics, label = { Text("Diagnostics") })
            }
        }
    }
}
