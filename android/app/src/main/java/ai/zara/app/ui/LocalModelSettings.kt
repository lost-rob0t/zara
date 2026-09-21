package ai.zara.app.ui

import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalModelBackend
import ai.zara.app.localai.LocalModelQuantization
import ai.zara.app.localai.LocalModelSpec
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.DropdownMenu
import androidx.compose.material3.DropdownMenuItem
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp

@Composable
internal fun LocalModelSettingsCard(
    state: LocalAiState,
    models: List<LocalModelSpec>,
    busy: Boolean,
    onImport: (String, String, LocalModelQuantization, Int, LocalModelBackend) -> Unit,
    onSelect: (String, String) -> Unit,
    onUnload: () -> Unit,
) {
    var modelId by rememberSaveable { mutableStateOf("local-model") }
    var modelVersion by rememberSaveable { mutableStateOf("1") }
    var contextTokens by rememberSaveable { mutableStateOf("4096") }
    var quantizationName by rememberSaveable { mutableStateOf(LocalModelQuantization.DYNAMIC_INT4.name) }
    var backendName by rememberSaveable { mutableStateOf(LocalModelBackend.CPU.name) }
    var quantizationMenu by rememberSaveable { mutableStateOf(false) }
    var backendMenu by rememberSaveable { mutableStateOf(false) }

    val quantization = runCatching { LocalModelQuantization.valueOf(quantizationName) }
        .getOrDefault(LocalModelQuantization.DYNAMIC_INT4)
    val backend = runCatching { LocalModelBackend.valueOf(backendName) }
        .getOrDefault(LocalModelBackend.CPU)
    val context = contextTokens.toIntOrNull()
    val importReady =
        modelId.isNotBlank() &&
            modelVersion.isNotBlank() &&
            context != null &&
            context in 128..131_072 &&
            !busy

    SectionCard("LOCAL LANGUAGE MODEL") {
        KeyValueRow("state", state.phase.name.lowercase())
        KeyValueRow(
            "active",
            state.model?.let { "${it.id}@${it.version}" } ?: "none",
        )
        state.model?.let { active ->
            KeyValueRow("format", active.format.wireName)
            KeyValueRow("quantization", active.quantization.wireName)
            KeyValueRow("backend", active.backend.name.lowercase())
            KeyValueRow("context", active.maxContextTokens.toString())
        }
        state.failure?.let { failure -> ErrorBanner(failure) }

        Text(
            "INSTALLED MODELS",
            color = LocalZaraTokens.current.accentCyan,
            style = MaterialTheme.typography.labelSmall,
        )
        if (models.isEmpty()) {
            MutedNotice("No verified on-device language model is installed yet.")
        } else {
            models.forEach { model ->
                LocalModelRow(
                    model = model,
                    active = state.model?.let { it.id == model.id && it.version == model.version } == true,
                    busy = busy,
                    onSelect = onSelect,
                )
            }
        }

        if (state.model != null) {
            SecondaryAction(
                label = "Unload from memory",
                enabled = !busy,
                onClick = onUnload,
            )
        }

        Text(
            "IMPORT .LITERTLM",
            color = LocalZaraTokens.current.accentCyan,
            style = MaterialTheme.typography.labelSmall,
        )
        OutlinedTextField(
            value = modelId,
            onValueChange = { modelId = it.take(96) },
            modifier = Modifier.fillMaxWidth(),
            enabled = !busy,
            singleLine = true,
            label = { Text("Model id") },
        )
        OutlinedTextField(
            value = modelVersion,
            onValueChange = { modelVersion = it.take(64) },
            modifier = Modifier.fillMaxWidth(),
            enabled = !busy,
            singleLine = true,
            label = { Text("Version") },
        )
        OutlinedTextField(
            value = contextTokens,
            onValueChange = { contextTokens = it.filter(Char::isDigit).take(6) },
            modifier = Modifier.fillMaxWidth(),
            enabled = !busy,
            singleLine = true,
            label = { Text("Context tokens") },
        )
        Column(
            modifier = Modifier.fillMaxWidth(),
            verticalArrangement = Arrangement.spacedBy(4.dp),
        ) {
            Box {
                SecondaryAction(
                    label = "Quantization: ${quantization.wireName}",
                    enabled = !busy,
                ) { quantizationMenu = true }
                DropdownMenu(
                    expanded = quantizationMenu,
                    onDismissRequest = { quantizationMenu = false },
                ) {
                    LocalModelQuantization.entries.forEach { choice ->
                        DropdownMenuItem(
                            text = { Text(choice.wireName) },
                            onClick = {
                                quantizationName = choice.name
                                quantizationMenu = false
                            },
                        )
                    }
                }
            }
            Box {
                SecondaryAction(
                    label = "Backend: ${backend.name}",
                    enabled = !busy,
                ) { backendMenu = true }
                DropdownMenu(
                    expanded = backendMenu,
                    onDismissRequest = { backendMenu = false },
                ) {
                    LocalModelBackend.entries.forEach { choice ->
                        DropdownMenuItem(
                            text = { Text(choice.name) },
                            onClick = {
                                backendName = choice.name
                                backendMenu = false
                            },
                        )
                    }
                }
            }
        }
        PrimaryAction(
            label = if (busy) "Working…" else "Choose .litertlm file",
            enabled = importReady,
        ) {
            onImport(
                modelId.trim(),
                modelVersion.trim(),
                quantization,
                checkNotNull(context),
                backend,
            )
        }
        MutedNotice(
            "The selected file is copied into app-private storage, SHA-256 verified, and loaded entirely on-device. " +
                "Local mode never falls back to a network model."
        )
    }
}

@Composable
private fun LocalModelRow(
    model: LocalModelSpec,
    active: Boolean,
    busy: Boolean,
    onSelect: (String, String) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    Surface(
        modifier = Modifier.fillMaxWidth(),
        color = if (active) tokens.surfaceElevated else tokens.surfaceInput,
        shape = MaterialTheme.shapes.medium,
    ) {
        Row(
            modifier = Modifier.fillMaxWidth().padding(horizontal = 12.dp, vertical = 10.dp),
            verticalAlignment = Alignment.CenterVertically,
            horizontalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Column(modifier = Modifier.weight(1f)) {
                Text(
                    "${model.id}@${model.version}",
                    color = tokens.text,
                    fontFamily = FontFamily.Monospace,
                    style = MaterialTheme.typography.bodyMedium,
                )
                Text(
                    "${model.quantization.wireName} · ${model.backend.name.lowercase()} · ${model.maxContextTokens} ctx",
                    color = tokens.textMuted,
                    style = MaterialTheme.typography.bodySmall,
                )
            }
            SecondaryAction(
                label = if (active) "Active" else "Use",
                enabled = !active && !busy,
            ) {
                onSelect(model.id, model.version)
            }
        }
    }
}
