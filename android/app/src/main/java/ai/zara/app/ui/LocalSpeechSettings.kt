package ai.zara.app.ui

import ai.zara.app.localai.LocalTtsPhase
import ai.zara.app.localai.LocalTtsState
import androidx.compose.runtime.Composable

@Composable
internal fun LocalSpeechSettingsCard(
    state: LocalTtsState,
    onTest: () -> Unit,
    onStop: () -> Unit,
    onRefresh: () -> Unit,
    onInstallVoices: () -> Unit,
    onOpenSettings: () -> Unit,
) {
    SectionCard("OFFLINE SPEECH") {
        KeyValueRow("state", state.phase.name.lowercase())
        KeyValueRow("voice", state.voiceId ?: "none installed")
        state.locale?.let { KeyValueRow("language", it) }
        state.failure?.let { ErrorBanner(it) }
        MutedNotice("Uses an installed Android offline voice on this device. No Zara server or language model is required.")
        PrimaryAction("Test offline speech", state.phase == LocalTtsPhase.READY, onTest)
        SecondaryAction("Stop speech", state.phase == LocalTtsPhase.SPEAKING, onStop)
        SecondaryAction("Refresh voices", state.phase != LocalTtsPhase.STARTING, onRefresh)
        SecondaryAction("Install voice data", true, onInstallVoices)
        SecondaryAction("Android speech settings", true, onOpenSettings)
    }
}
