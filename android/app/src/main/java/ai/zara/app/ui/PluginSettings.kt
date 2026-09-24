package ai.zara.app.ui

import ai.zara.app.plugins.PluginApkInstaller
import ai.zara.app.plugins.StagedPluginApk
import android.app.Activity
import android.content.Intent
import android.net.Uri
import android.provider.Settings
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberCoroutineScope
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import kotlinx.coroutines.CancellationException
import kotlinx.coroutines.launch

internal data class PluginInstallUi(
    val candidate: StagedPluginApk?,
    val busy: Boolean,
    val installing: Boolean,
    val message: String?,
    val error: String?,
    val chooseApk: () -> Unit,
    val install: () -> Unit,
    val clear: () -> Unit,
)

@Composable
internal fun rememberPluginInstallUi(): PluginInstallUi {
    val context = LocalContext.current.applicationContext
    val installer = remember(context) { PluginApkInstaller(context) }
    val scope = rememberCoroutineScope()
    var candidate by rememberSaveable { mutableStateOf<StagedPluginApk?>(null) }
    var busy by remember { mutableStateOf(false) }
    var installing by rememberSaveable { mutableStateOf(false) }
    var message by rememberSaveable { mutableStateOf<String?>(null) }
    var failure by rememberSaveable { mutableStateOf<String?>(null) }

    fun discard(value: StagedPluginApk?) {
        if (value == null) return
        scope.launch {
            try {
                installer.discard(value)
            } catch (error: CancellationException) {
                throw error
            } catch (_: Exception) {
                failure = "Temporary APK cleanup failed. Android may reclaim the cached file later."
            }
        }
    }

    val installResult = rememberLauncherForActivityResult(ActivityResultContracts.StartActivityForResult()) { result ->
        if (installing) {
            val completed = candidate
            installing = false
            candidate = null
            message = when {
                result.resultCode == Activity.RESULT_OK && completed != null ->
                    "Android installed ${completed.identity.packageName}. This does not enable Zara capabilities."
                result.resultCode == Activity.RESULT_CANCELED -> "Installation cancelled."
                else -> "Android did not confirm installation. Select the APK to try again."
            }
            discard(completed)
        }
    }
    val allowInstalls = rememberLauncherForActivityResult(ActivityResultContracts.StartActivityForResult()) {
        message = if (context.packageManager.canRequestPackageInstalls()) {
            "APK installs are allowed. Tap Install APK to continue."
        } else {
            "Android install permission was not granted. The APK has not been installed."
        }
    }
    val chooseApk = rememberLauncherForActivityResult(ActivityResultContracts.OpenDocument()) { uri ->
        if (uri != null && !busy && !installing) {
            busy = true
            scope.launch {
                failure = null
                message = "Preparing APK…"
                try {
                    val prepared = installer.stage(uri)
                    val previous = candidate
                    candidate = prepared
                    message = "Review the package and signer before installing."
                    discard(previous)
                } catch (error: CancellationException) {
                    throw error
                } catch (_: Exception) {
                    failure = "Cannot prepare this APK. Choose a signed, standalone APK compatible with this Android version (maximum 256 MiB). Zara updates belong under General → Self Update."
                    message = null
                } finally {
                    busy = false
                }
            }
        }
    }
    return PluginInstallUi(
        candidate = candidate,
        busy = busy,
        installing = installing,
        message = message,
        error = failure,
        chooseApk = {
            if (!busy && !installing) {
                failure = null
                try {
                    chooseApk.launch(arrayOf(PluginApkInstaller.APK_MIME, "application/octet-stream"))
                } catch (_: Exception) {
                    failure = "Android's file picker is unavailable."
                }
            }
        },
        install = {
            val reviewed = candidate
            if (reviewed != null && !busy && !installing) {
                failure = null
                if (!context.packageManager.canRequestPackageInstalls()) {
                    try {
                        allowInstalls.launch(Intent(
                            Settings.ACTION_MANAGE_UNKNOWN_APP_SOURCES,
                            Uri.parse("package:${context.packageName}"),
                        ))
                    } catch (_: Exception) {
                        failure = "Open Android Settings → Special app access → Install unknown apps and allow Zara."
                    }
                } else {
                    busy = true
                    scope.launch {
                        try {
                            val intent = installer.installIntent(reviewed)
                            installing = true
                            message = "Waiting for Android installation confirmation…"
                            installResult.launch(intent)
                        } catch (error: CancellationException) {
                            throw error
                        } catch (_: Exception) {
                            installing = false
                            failure = "Could not open Android's installer. Check install permission or select the APK again."
                        } finally {
                            busy = false
                        }
                    }
                }
            }
        },
        clear = {
            if (!busy && !installing) {
                val previous = candidate
                candidate = null
                message = null
                failure = null
                discard(previous)
            }
        },
    )
}

@Composable
internal fun PluginSettingsSurface(state: PluginInstallUi, padding: PaddingValues) {
    ScreenBody(padding) {
        ScreenTitle("Plugins", "Install Android plugin APKs")
        SectionCard("INSTALL") {
            MutedNotice("Choose a standalone APK from a plugin publisher you trust. Desktop Python/Nix plugins and ZIP bundles cannot be installed here.")
            PrimaryAction(
                if (state.busy) "Preparing…" else "Choose APK",
                !state.busy && !state.installing,
                state.chooseApk,
            )
            state.message?.let { MutedNotice(it) }
        }
        state.candidate?.let { candidate ->
            SectionCard("REVIEW APK") {
                KeyValueRow("package", candidate.identity.packageName)
                KeyValueRow("version", "${candidate.identity.versionName} (${candidate.identity.versionCode})")
                KeyValueRow("bytes", candidate.bytes.toString())
                KeyValueRow("APK SHA-256", candidate.sha256)
                candidate.identity.signerSha256.forEach { signer -> KeyValueRow("signer SHA-256", signer) }
                MutedNotice("These fingerprints identify the selected file; they do not certify its publisher or Zara compatibility. Android performs the final package/signature checks.")
                PrimaryAction("Install APK", !state.busy && !state.installing, state.install)
                SecondaryAction("Discard APK", !state.busy && !state.installing, state.clear)
            }
        }
        state.error?.let { ErrorBanner(it) }
        SectionCard("CAPABILITIES") {
            MutedNotice("APK installation is separate from plugin enablement. The compatible Zara plugin host and permission controls are tracked in #924; this screen does not bind plugins, grant trust, or register tools.")
        }
    }
}
