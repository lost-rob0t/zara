package ai.zara.app.ui

import ai.zara.app.widget.WidgetStyleCompiler
import ai.zara.app.widget.WidgetStyleEnvironment
import ai.zara.app.widget.WidgetStyleStatus
import ai.zara.app.widget.ZaraWidgetUpdater
import ai.zara.ui.theme.ZaraTheme
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.platform.LocalContext
import java.nio.charset.StandardCharsets

@Composable
internal fun WidgetAppearanceControls(selectedTheme: ZaraTheme) {
    val context = LocalContext.current
    var status by remember(selectedTheme) {
        mutableStateOf<WidgetStyleStatus?>(runCatching { WidgetStyleEnvironment.status(context) }.getOrNull())
    }
    var failure by remember { mutableStateOf<String?>(null) }

    val importLauncher = rememberLauncherForActivityResult(
        ActivityResultContracts.OpenDocument(),
    ) { uri ->
        if (uri == null) return@rememberLauncherForActivityResult
        failure = null
        try {
            val bytes = context.contentResolver.openInputStream(uri)?.use {
                it.readNBytes(WidgetStyleCompiler.MAX_SOURCE_BYTES + 1)
            } ?: throw IllegalArgumentException("Widget stylesheet could not be opened")
            require(bytes.size <= WidgetStyleCompiler.MAX_SOURCE_BYTES) {
                "Widget stylesheet is too large"
            }
            val imported = WidgetStyleEnvironment.import(
                context,
                String(bytes, StandardCharsets.UTF_8),
            )
            status = imported
            ZaraWidgetUpdater.refreshAll(context)
        } catch (error: Exception) {
            failure = error.message ?: error::class.java.simpleName
        }
    }

    val exportLauncher = rememberLauncherForActivityResult(
        ActivityResultContracts.CreateDocument("text/x-prolog"),
    ) { uri ->
        if (uri == null) return@rememberLauncherForActivityResult
        failure = null
        try {
            context.contentResolver.openOutputStream(uri, "wt")?.bufferedWriter()?.use { writer ->
                writer.write(WidgetStyleEnvironment.export(context))
            } ?: throw IllegalArgumentException("Widget stylesheet destination could not be opened")
        } catch (error: Exception) {
            failure = error.message ?: error::class.java.simpleName
        }
    }

    LaunchedEffect(selectedTheme) {
        status = runCatching { WidgetStyleEnvironment.status(context) }
            .onFailure { failure = it.message ?: it::class.java.simpleName }
            .getOrNull()
        ZaraWidgetUpdater.refreshAll(context)
    }

    SectionCard("WIDGET STYLE") {
        KeyValueRow("style", status?.name ?: "unavailable")
        KeyValueRow("source", if (status?.imported == true) "imported .pl" else "built-in")
        PrimaryAction("Import .pl", true) {
            importLauncher.launch(arrayOf("text/x-prolog", "text/plain", "application/octet-stream"))
        }
        SecondaryAction("Export .pl", true) {
            exportLauncher.launch("${status?.name ?: "zara-widget-style"}.pl")
        }
        SecondaryAction("Reset widget style", status?.imported == true) {
            failure = null
            try {
                status = WidgetStyleEnvironment.reset(context)
                ZaraWidgetUpdater.refreshAll(context)
            } catch (error: Exception) {
                failure = error.message ?: error::class.java.simpleName
            }
        }
        MutedNotice("Imports are parsed as bounded data-only Prolog. Invalid files leave the last valid widget style unchanged.")
        failure?.let { ErrorBanner(it) }
    }
}
