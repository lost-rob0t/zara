package ai.zara.app.ui

import ai.zara.app.ZaraApplication
import ai.zara.app.plugins.PluginApkSecurity
import ai.zara.app.plugins.PluginInstallPhase
import android.content.Intent
import android.net.Uri
import android.provider.Settings
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.platform.testTag

@Composable
internal fun PluginInstallSurface(padding: PaddingValues) {
    ScreenBody(padding) {
        ScreenTitle("Plugins", "Install Android plugin APKs")
        PluginInstallSettings()
    }
}

@Composable
internal fun PluginInstallSettings() {
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
    val checksumValid = runCatching { PluginApkSecurity.normalizeSha256(checksum) }.isSuccess

    SectionCard("INSTALL PLUGIN") {
        MutedNotice("Choose a standalone Android plugin APK. Desktop Python plugins and split APK bundles are not supported here.")
        OutlinedTextField(
            value = checksum,
            onValueChange = { if (it.length <= 128) checksum = it },
            label = { Text("Publisher SHA-256") },
            supportingText = { Text("Copy the 64-character checksum from a publisher you trust.") },
            singleLine = true,
            enabled = !busy,
            modifier = Modifier.fillMaxWidth().testTag("plugin-apk-checksum"),
        )
        PrimaryAction("Choose plugin APK", checksumValid && !busy) {
            uiError = null
            requestedChecksum = checksum
            try {
                documentPicker.launch(arrayOf("application/vnd.android.package-archive", "application/octet-stream"))
            } catch (_: RuntimeException) {
                uiError = "Android's file picker could not open. Check that a document provider is available."
            }
        }
    }
    state.candidate?.let { candidate ->
        SectionCard(if (state.phase == PluginInstallPhase.INSTALLED) "LAST INSTALLATION" else "REVIEW APK") {
            KeyValueRow("package", candidate.packageName)
            KeyValueRow("version", candidate.version)
            KeyValueRow("APK SHA-256", candidate.sha256)
            candidate.certificates.forEachIndexed { index, certificate ->
                KeyValueRow("signer ${index + 1} SHA-256", certificate)
            }
            MutedNotice("A matching checksum checks the selected file, not the publisher's trustworthiness. Android validates the APK signature during installation.")
            when (state.phase) {
                PluginInstallPhase.REVIEW, PluginInstallPhase.FAILED ->
                    PrimaryAction("Install plugin", true, installer::requestInstall)
                PluginInstallPhase.PERMISSION_REQUIRED -> PrimaryAction("Allow plugin installs", true) {
                    uiError = null
                    try {
                        installPermission.launch(Intent(
                            Settings.ACTION_MANAGE_UNKNOWN_APP_SOURCES,
                            Uri.parse("package:${context.packageName}"),
                        ))
                    } catch (_: RuntimeException) {
                        uiError = "Install-source settings could not open. Review Zara's install permission in Android Settings."
                    }
                }
                PluginInstallPhase.INSTALLING -> MutedNotice("Waiting for Android. Installation is not complete until Android reports success.")
                PluginInstallPhase.INSTALLED -> SecondaryAction("Open Android app settings", true) {
                    uiError = null
                    try {
                        context.startActivity(Intent(
                            Settings.ACTION_APPLICATION_DETAILS_SETTINGS,
                            Uri.parse("package:${candidate.packageName}"),
                        ))
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
    MutedNotice("Installation does not enable a plugin, grant permissions, or authorize its capabilities. Plugin host activation is separate and is not available in this build.")
}
