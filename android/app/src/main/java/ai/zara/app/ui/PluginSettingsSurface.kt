package ai.zara.app.ui

import ai.zara.app.ZaraApplication
import ai.zara.app.plugins.AndroidPluginCatalogHealth
import ai.zara.app.plugins.AndroidPluginCatalogItem
import ai.zara.app.plugins.AndroidPluginCatalogSnapshot
import ai.zara.app.plugins.PluginApkSecurity
import ai.zara.app.plugins.PluginInstallPhase
import android.content.Intent
import android.net.Uri
import android.provider.Settings
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.testTag
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp

@Composable
internal fun PluginSettingsSurface(padding: PaddingValues) {
    val context = LocalContext.current
    val application = context.applicationContext as ZaraApplication
    val projection = remember(application) { application.pluginCatalogProjection }
    var snapshot by remember(projection) { mutableStateOf(projection.snapshot()) }
    var search by rememberSaveable { mutableStateOf("") }

    DisposableEffect(projection) {
        val subscription = projection.observe { snapshot = it }
        onDispose { subscription.close() }
    }

    ScreenBody(padding) {
        ScreenTitle("Plugins", "Extensions, health, and capability visibility")
        PluginHostSummary(snapshot)

        if (snapshot.hostAvailable) {
            val visible = remember(snapshot, search) {
                val query = search.trim().lowercase()
                if (query.isEmpty()) {
                    snapshot.plugins
                } else {
                    snapshot.plugins.filter { plugin ->
                        plugin.displayName.lowercase().contains(query) ||
                            plugin.pluginId.lowercase().contains(query) ||
                            plugin.capabilities.any { it.lowercase().contains(query) }
                    }
                }
            }
            SectionCard("DISCOVERED") {
                KeyValueRow("plugins", snapshot.plugins.size.toString())
                if (snapshot.plugins.size > 4) {
                    OutlinedTextField(
                        value = search,
                        onValueChange = { if (it.length <= 80) search = it },
                        label = { Text("Search plugins") },
                        singleLine = true,
                        modifier = Modifier.fillMaxWidth().testTag("plugin-catalog-search"),
                    )
                }
                if (visible.isEmpty()) {
                    MutedNotice(
                        if (snapshot.plugins.isEmpty()) {
                            "No compatible plugins are currently published by the canonical host."
                        } else {
                            "No plugins match this search."
                        },
                    )
                } else {
                    visible.forEachIndexed { index, plugin ->
                        PluginCatalogRow(plugin)
                        if (index != visible.lastIndex) {
                            HorizontalDivider(color = LocalZaraTokens.current.border)
                        }
                    }
                }
            }
        } else {
            SectionCard("CATALOG UNAVAILABLE") {
                MutedNotice(
                    "The canonical ZARA-ANDROID-PLUGIN/1 host has not published a catalog snapshot. " +
                        "Zara will not infer trust, enablement, health, or capabilities from installed APKs.",
                )
            }
        }

        PluginInstallSettings()

        MutedNotice(
            "Installed, trusted, enabled, and ready are separate states. " +
                "A plugin is actionable only after the canonical host proves current trust, setup, and health.",
        )
    }
}

@Composable
private fun PluginHostSummary(snapshot: AndroidPluginCatalogSnapshot) {
    SectionCard("PLUGIN HOST") {
        KeyValueRow("status", if (snapshot.hostAvailable) "available" else "unavailable")
        if (snapshot.hostAvailable) {
            KeyValueRow("generation", snapshot.generation.toString())
        } else {
            snapshot.reasonCode?.let { KeyValueRow("reason", it) }
        }
        MutedNotice(
            if (snapshot.hostAvailable) {
                "This screen is a read-only projection. Capability visibility does not grant execution authority."
            } else {
                "APK installation remains available below, but installation alone never marks a plugin ready."
            },
        )
    }
}

@Composable
private fun PluginCatalogRow(plugin: AndroidPluginCatalogItem) {
    val tokens = LocalZaraTokens.current
    Column(
        modifier = Modifier
            .fillMaxWidth()
            .padding(vertical = 6.dp)
            .testTag("plugin-${plugin.pluginId}"),
        verticalArrangement = Arrangement.spacedBy(4.dp),
    ) {
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(12.dp),
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(
                plugin.displayName,
                modifier = Modifier.weight(1f),
                color = tokens.text,
                style = MaterialTheme.typography.titleMedium,
            )
            Text(
                plugin.health.label.uppercase(),
                color = when (plugin.health) {
                    AndroidPluginCatalogHealth.READY -> tokens.success
                    AndroidPluginCatalogHealth.PERMISSION_REQUIRED,
                    AndroidPluginCatalogHealth.DEGRADED -> tokens.warning
                    AndroidPluginCatalogHealth.INCOMPATIBLE -> tokens.error
                    else -> tokens.textMuted
                },
                fontFamily = FontFamily.Monospace,
                fontSize = 10.sp,
                letterSpacing = 1.2.sp,
            )
        }
        Text(
            "${plugin.version} · ${plugin.source.label} · ${plugin.pluginId}",
            color = tokens.textMuted,
            style = MaterialTheme.typography.bodySmall,
        )
        Text(
            "Capabilities · ${summarize(plugin.capabilities)}",
            color = tokens.text,
            style = MaterialTheme.typography.bodySmall,
        )
        Text(
            "Permissions · ${summarize(plugin.permissions)}",
            color = tokens.text,
            style = MaterialTheme.typography.bodySmall,
        )
        plugin.diagnosticCode?.let { code ->
            Text(
                "Diagnostic · $code",
                color = tokens.textMuted,
                fontFamily = FontFamily.Monospace,
                style = MaterialTheme.typography.bodySmall,
            )
        }
        if (plugin.health != AndroidPluginCatalogHealth.READY) {
            MutedNotice(healthGuidance(plugin.health))
        }
    }
}

@Composable
private fun PluginInstallSettings() {
    val context = LocalContext.current
    val installer = remember(context.applicationContext) {
        (context.applicationContext as ZaraApplication).pluginInstaller
    }
    var state by remember(installer) { mutableStateOf(installer.state()) }
    var checksum by rememberSaveable { mutableStateOf("") }
    var requestedChecksum by rememberSaveable { mutableStateOf("") }
    var uiError by rememberSaveable { mutableStateOf<String?>(null) }

    DisposableEffect(installer) {
        val subscription = installer.observe { state = it }
        onDispose { subscription.close() }
    }

    val documentPicker = rememberLauncherForActivityResult(ActivityResultContracts.OpenDocument()) { uri ->
        if (uri != null) installer.prepare(uri, requestedChecksum)
    }
    val installPermission = rememberLauncherForActivityResult(ActivityResultContracts.StartActivityForResult()) {
        installer.permissionResult()
    }
    val busy = state.phase == PluginInstallPhase.VERIFYING || state.phase == PluginInstallPhase.INSTALLING

    SectionCard("INSTALL APK") {
        MutedNotice(
            "Advanced: install one standalone Android plugin APK from a trusted publisher. " +
                "Desktop plugins, split APK sets, and bundles are not accepted here.",
        )
        OutlinedTextField(
            value = checksum,
            onValueChange = { if (it.length <= 128) checksum = it },
            label = { Text("Publisher SHA-256") },
            supportingText = { Text("Paste the publisher's 64-character checksum before selecting the APK.") },
            singleLine = true,
            enabled = !busy,
            modifier = Modifier.fillMaxWidth().testTag("plugin-apk-checksum"),
        )
        PrimaryAction("Choose APK", !busy) {
            val normalizedChecksum = normalizedPluginChecksumOrNull(checksum)
            if (normalizedChecksum == null) {
                uiError = "Enter the publisher's 64-character SHA-256."
            } else {
                uiError = null
                requestedChecksum = normalizedChecksum
                try {
                    documentPicker.launch(
                        arrayOf(
                            "application/vnd.android.package-archive",
                            "application/octet-stream",
                        ),
                    )
                } catch (_: RuntimeException) {
                    uiError = "Android's file picker could not open. Check that a document provider is available."
                }
            }
        }
    }

    state.candidate?.let { candidate ->
        SectionCard(if (state.phase == PluginInstallPhase.INSTALLED) "LAST INSTALLATION" else "REVIEW APK") {
            KeyValueRow("package", candidate.packageName)
            KeyValueRow("version", candidate.version)
            PluginDigestRow("APK SHA-256", candidate.sha256)
            candidate.certificates.forEachIndexed { index, certificate ->
                PluginDigestRow("signer ${index + 1} SHA-256", certificate)
            }
            MutedNotice(
                "The checksum proves the selected bytes match the value you supplied. " +
                    "It does not make the publisher trusted; Android still validates the APK signature.",
            )
            when (state.phase) {
                PluginInstallPhase.REVIEW,
                PluginInstallPhase.FAILED -> PrimaryAction("Install plugin", true, installer::requestInstall)
                PluginInstallPhase.PERMISSION_REQUIRED -> PrimaryAction("Allow plugin installs", true) {
                    uiError = null
                    try {
                        installPermission.launch(
                            Intent(
                                Settings.ACTION_MANAGE_UNKNOWN_APP_SOURCES,
                                Uri.parse("package:${context.packageName}"),
                            ),
                        )
                    } catch (_: RuntimeException) {
                        uiError = "Install-source settings could not open. Review Zara's install permission in Android Settings."
                    }
                }
                PluginInstallPhase.INSTALLING -> MutedNotice(
                    "Waiting for Android. Installation is not complete until Android reports success.",
                )
                PluginInstallPhase.INSTALLED -> SecondaryAction("Open Android app settings", true) {
                    uiError = null
                    try {
                        context.startActivity(
                            Intent(
                                Settings.ACTION_APPLICATION_DETAILS_SETTINGS,
                                Uri.parse("package:${candidate.packageName}"),
                            ),
                        )
                    } catch (_: RuntimeException) {
                        uiError = "App settings could not open. Find this package in Android Settings > Apps."
                    }
                }
                else -> Unit
            }
        }
    }

    if (state.phase == PluginInstallPhase.VERIFYING) MutedNotice("Verifying and staging APK…")
    state.message?.let {
        if (state.phase == PluginInstallPhase.FAILED) ErrorBanner(it) else MutedNotice(it)
    }
    uiError?.let { ErrorBanner(it) }
    if (state.phase != PluginInstallPhase.IDLE && state.phase != PluginInstallPhase.VERIFYING) {
        SecondaryAction("Dismiss installation", true, installer::dismiss)
    }
}

@Composable
private fun PluginDigestRow(label: String, value: String) {
    val tokens = LocalZaraTokens.current
    Text(label, color = tokens.textMuted, fontSize = 11.sp)
    SelectionContainer {
        Text(
            value,
            modifier = Modifier.fillMaxWidth(),
            color = tokens.text,
            fontFamily = FontFamily.Monospace,
            fontSize = 10.sp,
        )
    }
}

internal fun normalizedPluginChecksumOrNull(value: String): String? =
    runCatching { PluginApkSecurity.normalizeSha256(value) }.getOrNull()

private fun summarize(values: List<String>): String {
    if (values.isEmpty()) return "none"
    val shown = values.take(4).joinToString(", ")
    return if (values.size > 4) "$shown +${values.size - 4}" else shown
}

private fun yesNo(value: Boolean): String = if (value) "yes" else "no"

private fun healthGuidance(health: AndroidPluginCatalogHealth): String = when (health) {
    AndroidPluginCatalogHealth.INSTALLED ->
        "Installed only. Trust and activation have not been proven."
    AndroidPluginCatalogHealth.DISABLED ->
        "Disabled plugins stay visible but cannot expose executable controls."
    AndroidPluginCatalogHealth.PERMISSION_REQUIRED ->
        "Android setup or permission is required before this plugin can become ready."
    AndroidPluginCatalogHealth.DEGRADED ->
        "The plugin is available only in a degraded state; executable controls remain fail-closed."
    AndroidPluginCatalogHealth.INCOMPATIBLE ->
        "This plugin is incompatible with the current Zara Android plugin protocol."
    AndroidPluginCatalogHealth.DISCONNECTED ->
        "The current plugin session is disconnected; stale controls are not usable."
    AndroidPluginCatalogHealth.READY ->
        ""
}
