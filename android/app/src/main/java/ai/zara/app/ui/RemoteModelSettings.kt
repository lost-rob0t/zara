package ai.zara.app.ui

import ai.zara.app.model.CloudModelConfig
import ai.zara.app.model.CloudModelProvider
import ai.zara.app.model.CloudModelState
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.selection.selectable
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.RadioButton
import androidx.compose.material3.Switch
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.input.PasswordVisualTransformation
import androidx.compose.ui.unit.dp

@Composable
internal fun RemoteModelSettingsCard(
    state: CloudModelState,
    busy: Boolean,
    onConfigure: (CloudModelConfig, String?) -> Unit,
    onClearApiKey: () -> Unit,
) {
    val config = state.config
    var enabled by rememberSaveable(config) { mutableStateOf(config.enabled) }
    var providerName by rememberSaveable(config) { mutableStateOf(config.provider.name) }
    var endpoint by rememberSaveable(config) { mutableStateOf(config.endpoint) }
    var model by rememberSaveable(config) { mutableStateOf(config.model) }
    var apiKey by rememberSaveable { mutableStateOf("") }
    val provider = runCatching { CloudModelProvider.valueOf(providerName) }
        .getOrDefault(CloudModelProvider.OPENAI_COMPATIBLE)

    SectionCard("REMOTE MODEL API") {
        KeyValueRow("state", state.phase.name.lowercase())
        KeyValueRow("credential", if (state.apiKeyConfigured) "stored in Android Keystore" else "not configured")
        state.lastFailure?.let { KeyValueRow("last failure", it.name.lowercase()) }

        Text("Provider", color = LocalZaraTokens.current.text)
        listOf(
            CloudModelProvider.OPENROUTER to "OpenRouter",
            CloudModelProvider.OPENAI_COMPATIBLE to "OpenAI-compatible",
        ).forEach { (choice, label) ->
            Row(
                modifier = Modifier
                    .fillMaxWidth()
                    .selectable(
                        selected = provider == choice,
                        onClick = {
                            providerName = choice.name
                            if (choice == CloudModelProvider.OPENROUTER) {
                                endpoint = CloudModelConfig.OPENROUTER_ENDPOINT
                            }
                        },
                    )
                    .padding(vertical = 2.dp),
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(8.dp),
            ) {
                RadioButton(
                    selected = provider == choice,
                    onClick = {
                        providerName = choice.name
                        if (choice == CloudModelProvider.OPENROUTER) {
                            endpoint = CloudModelConfig.OPENROUTER_ENDPOINT
                        }
                    },
                )
                Text(label, color = LocalZaraTokens.current.text)
            }
        }

        OutlinedTextField(
            value = endpoint,
            onValueChange = { endpoint = it },
            modifier = Modifier.fillMaxWidth(),
            enabled = !busy,
            singleLine = true,
            label = { Text("API base URL") },
        )
        OutlinedTextField(
            value = model,
            onValueChange = { model = it },
            modifier = Modifier.fillMaxWidth(),
            enabled = !busy,
            singleLine = true,
            label = { Text("Model") },
        )
        OutlinedTextField(
            value = apiKey,
            onValueChange = { apiKey = it },
            modifier = Modifier.fillMaxWidth(),
            enabled = !busy,
            singleLine = true,
            visualTransformation = PasswordVisualTransformation(),
            label = {
                Text(if (state.apiKeyConfigured) "Replace API key (optional)" else "API key")
            },
        )

        Row(
            modifier = Modifier.fillMaxWidth(),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Column(modifier = Modifier.weight(1f)) {
                Text("Enable remote model API", color = LocalZaraTokens.current.text)
                MutedNotice("Used only by explicit Remote routing; Symbolic and Local stay network-closed.")
            }
            Switch(
                checked = enabled,
                onCheckedChange = { enabled = it },
                enabled = !busy,
            )
        }

        SecondaryAction(
            label = "OpenRouter preset",
            enabled = !busy,
        ) {
            providerName = CloudModelProvider.OPENROUTER.name
            endpoint = CloudModelConfig.OPENROUTER_ENDPOINT
        }
        SecondaryAction(
            label = "StarIntel OpenAI-compatible preset",
            enabled = !busy,
        ) {
            providerName = CloudModelProvider.OPENAI_COMPATIBLE.name
            endpoint = CloudModelConfig.DEFAULT_STARINTEL_ENDPOINT
        }

        PrimaryAction(
            label = if (busy) "Saving…" else "Save remote model",
            enabled = !busy && (!enabled || model.isNotBlank()),
        ) {
            onConfigure(
                config.copy(
                    enabled = enabled,
                    provider = provider,
                    endpoint = endpoint.trim(),
                    model = model.trim(),
                ),
                apiKey.trim().takeIf(String::isNotEmpty),
            )
            apiKey = ""
        }

        if (state.apiKeyConfigured) {
            SecondaryAction("Clear stored API key", !busy, onClearApiKey)
        }
        MutedNotice(
            "API keys are wrapped by Android Keystore and kept in app-private storage. " +
                "They are never written into provider settings, diagnostics, or the UI."
        )
    }
}
