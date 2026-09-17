package ai.zara.app.ui

import ai.zara.app.watch.NearbyWatch
import ai.zara.app.watch.WatchDebugEndpoint
import ai.zara.app.watch.WatchInstallPolicy
import ai.zara.app.watch.WatchSetupPhase
import ai.zara.app.watch.WatchSetupState
import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.LinearProgressIndicator
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedButton
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.darkColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp

@Composable
fun WatchSetupSurface(
    state: WatchSetupState,
    onScan: () -> Unit,
    onHostChanged: (String) -> Unit,
    onPairPortChanged: (String) -> Unit,
    onConnectPortChanged: (String) -> Unit,
    onPairCodeChanged: (String) -> Unit,
    onUseEndpoint: (WatchDebugEndpoint) -> Unit,
    onPair: () -> Unit,
    onConnect: () -> Unit,
    onInstall: () -> Unit,
    onUninstall: () -> Unit,
    onDisconnect: () -> Unit,
) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    val colors = darkColorScheme(
        primary = tokens.primary,
        secondary = tokens.secondary,
        background = tokens.background,
        surface = tokens.surface,
        surfaceVariant = tokens.surfaceElevated,
        onPrimary = Color(0xFF160018),
        onSecondary = Color(0xFF00161A),
        onBackground = tokens.text,
        onSurface = tokens.text,
        onSurfaceVariant = tokens.textMuted,
        error = tokens.error,
    )

    MaterialTheme(colorScheme = colors) {
        Surface(
            modifier = Modifier.fillMaxSize(),
            color = tokens.background,
        ) {
            Column(
                modifier = Modifier
                    .fillMaxSize()
                    .verticalScroll(rememberScrollState())
                    .padding(18.dp),
                verticalArrangement = Arrangement.spacedBy(12.dp),
            ) {
                Text(
                    "ZARA WATCH SETUP",
                    color = tokens.accentCyan,
                    fontFamily = FontFamily.Monospace,
                    fontWeight = FontWeight.Bold,
                    style = MaterialTheme.typography.titleLarge,
                )
                Text(
                    WatchInstallPolicy.transportNotice,
                    color = tokens.textMuted,
                    style = MaterialTheme.typography.bodyMedium,
                )

                StatusCard(state)

                Button(
                    onClick = onScan,
                    enabled = state.phase != WatchSetupPhase.SCANNING,
                    modifier = Modifier.fillMaxWidth(),
                    colors = ButtonDefaults.buttonColors(containerColor = tokens.primary),
                ) {
                    Text(if (state.phase == WatchSetupPhase.SCANNING) "Scanning…" else "Scan paired watch + ADB")
                }

                if (state.watches.isNotEmpty()) {
                    SectionTitle("PAIRED WEAR DEVICES")
                    state.watches.forEach { watch -> WatchRow(watch) }
                }

                if (state.debugEndpoints.isNotEmpty()) {
                    SectionTitle("WIRELESS DEBUGGING")
                    state.debugEndpoints.forEach { endpoint ->
                        OutlinedButton(
                            onClick = { onUseEndpoint(endpoint) },
                            modifier = Modifier.fillMaxWidth(),
                        ) {
                            Text(
                                if (endpoint.pairing) {
                                    "Use pairing · ${endpoint.host}:${endpoint.port}"
                                } else {
                                    "Use connection · ${endpoint.host}:${endpoint.port}"
                                }
                            )
                        }
                    }
                }

                SectionTitle("ONE-TIME PAIRING")
                Text(
                    "On the watch: Settings → Developer options → Wireless debugging → Pair new device.",
                    color = tokens.textMuted,
                    style = MaterialTheme.typography.bodySmall,
                )
                OutlinedTextField(
                    value = state.host,
                    onValueChange = onHostChanged,
                    modifier = Modifier.fillMaxWidth(),
                    singleLine = true,
                    label = { Text("Watch IP") },
                    placeholder = { Text("192.168.1.25") },
                )
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.spacedBy(10.dp),
                ) {
                    OutlinedTextField(
                        value = state.pairPort,
                        onValueChange = onPairPortChanged,
                        modifier = Modifier.weight(1f),
                        singleLine = true,
                        label = { Text("Pair port") },
                    )
                    OutlinedTextField(
                        value = state.pairCode,
                        onValueChange = onPairCodeChanged,
                        modifier = Modifier.weight(1f),
                        singleLine = true,
                        label = { Text("6-digit code") },
                    )
                }
                Button(
                    onClick = onPair,
                    modifier = Modifier.fillMaxWidth(),
                    enabled = state.phase !in setOf(
                        WatchSetupPhase.PAIRING,
                        WatchSetupPhase.CONNECTING,
                        WatchSetupPhase.DOWNLOADING,
                        WatchSetupPhase.INSTALLING,
                        WatchSetupPhase.REMOVING,
                    ),
                ) {
                    Text("Pair Zara with watch")
                }

                SectionTitle("CONNECT + INSTALL")
                Text(
                    "After pairing, return to the main Wireless debugging screen. Its connection port is usually different from the pairing port.",
                    color = tokens.textMuted,
                    style = MaterialTheme.typography.bodySmall,
                )
                OutlinedTextField(
                    value = state.connectPort,
                    onValueChange = onConnectPortChanged,
                    modifier = Modifier.fillMaxWidth(),
                    singleLine = true,
                    label = { Text("Connection port") },
                )
                Row(
                    modifier = Modifier.fillMaxWidth(),
                    horizontalArrangement = Arrangement.spacedBy(10.dp),
                ) {
                    Button(
                        onClick = onConnect,
                        modifier = Modifier.weight(1f),
                    ) {
                        Text("Connect")
                    }
                    OutlinedButton(
                        onClick = onDisconnect,
                        modifier = Modifier.weight(1f),
                    ) {
                        Text("Disconnect")
                    }
                }
                Button(
                    onClick = onInstall,
                    modifier = Modifier.fillMaxWidth(),
                    enabled = state.phase == WatchSetupPhase.CONNECTED || state.phase == WatchSetupPhase.INSTALLED,
                    colors = ButtonDefaults.buttonColors(containerColor = tokens.secondary),
                ) {
                    Text("Install Zara + Agenda")
                }
                OutlinedButton(
                    onClick = onUninstall,
                    modifier = Modifier.fillMaxWidth(),
                    enabled = state.phase == WatchSetupPhase.CONNECTED || state.phase == WatchSetupPhase.INSTALLED,
                ) {
                    Text("Uninstall Zara")
                }

                Spacer(Modifier.height(10.dp))
                Text(
                    "Zara Wear and the Zara Agenda watch face install as a pair. After installation, phone↔watch data uses the normal Wear Data Layer path; removing Zara from this screen removes both watch packages.",
                    color = tokens.textMuted,
                    style = MaterialTheme.typography.bodySmall,
                )
            }
        }
    }
}

@Composable
private fun StatusCard(state: WatchSetupState) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    Surface(
        modifier = Modifier.fillMaxWidth(),
        color = tokens.surfaceElevated,
        border = BorderStroke(1.dp, tokens.border),
        shape = MaterialTheme.shapes.medium,
    ) {
        Column(
            modifier = Modifier.padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text(
                state.phase.name,
                color = if (state.phase == WatchSetupPhase.ERROR) tokens.error else tokens.success,
                fontFamily = FontFamily.Monospace,
                fontWeight = FontWeight.Bold,
            )
            Text(state.status, color = tokens.text)
            state.connectedDevice?.let {
                Text("connected · $it", color = tokens.accentCyan)
            }
            state.progress?.let { progress ->
                LinearProgressIndicator(
                    progress = { progress.coerceIn(0f, 1f) },
                    modifier = Modifier.fillMaxWidth(),
                )
            }
        }
    }
}

@Composable
private fun WatchRow(watch: NearbyWatch) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    Surface(
        modifier = Modifier.fillMaxWidth(),
        color = tokens.surface,
        border = BorderStroke(1.dp, tokens.border),
        shape = MaterialTheme.shapes.small,
    ) {
        Column(Modifier.padding(12.dp)) {
            Text(watch.name, color = tokens.text, fontWeight = FontWeight.SemiBold)
            Text(
                buildString {
                    append(if (watch.nearby) "nearby" else "connected")
                    append(" · ")
                    append(if (watch.zaraInstalled) "Zara installed" else "Zara not detected")
                },
                color = tokens.textMuted,
                style = MaterialTheme.typography.bodySmall,
            )
        }
    }
}

@Composable
private fun SectionTitle(text: String) {
    val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
    Text(
        text,
        color = tokens.accentCyan,
        fontFamily = FontFamily.Monospace,
        fontWeight = FontWeight.Bold,
        style = MaterialTheme.typography.labelLarge,
    )
}
