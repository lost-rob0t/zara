package ai.zara.app.automation

import ai.zara.app.ZaraApplication
import ai.zara.app.accessibility.AccessibilityAutomationAdapter
import ai.zara.app.control.AndroidControlAccess
import ai.zara.app.control.AndroidControlAccessBroker
import ai.zara.app.device.AndroidAppLauncher
import ai.zara.app.device.AndroidAppSearchLauncher
import ai.zara.app.device.AndroidUriLauncher
import ai.zara.app.device.AppSearchAdapter
import ai.zara.app.device.OpenAppAdapter
import ai.zara.app.device.OpenUriAdapter
import ai.zara.app.prolog.AndroidAutomationAction
import ai.zara.app.prolog.AndroidAutomationCatalog
import ai.zara.app.prolog.AndroidAutomationResult
import ai.zara.app.prolog.AndroidAutomationRunner
import ai.zara.app.prolog.GitConfigTemplateImporter
import ai.zara.app.prolog.PrologSource
import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.ui.ThemePreferenceStore
import ai.zara.ui.theme.themeTokens
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Button
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.darkColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.setValue
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.unit.dp
import java.io.File
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

private data class VisionApprovalRequest(
    val action: AndroidAutomationAction,
    val future: CompletableFuture<Boolean>,
)

class AutomationActivity : ComponentActivity() {
    private val io: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-config-template-import").apply { isDaemon = true }
    }
    private lateinit var accessBroker: AndroidControlAccessBroker
    private lateinit var runner: AndroidAutomationRunner
    private lateinit var visionControl: AndroidAdbVisionConversationControl
    private var status by mutableStateOf("Preparing local Prolog automation…")
    private var access by mutableStateOf(emptyMap<AndroidControlAccess, Boolean>())
    private var busy by mutableStateOf(false)
    private var visionBusy by mutableStateOf(false)
    private var pendingVisionApproval by mutableStateOf<VisionApprovalRequest?>(null)

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        val session = (application as ZaraApplication).appSession
        accessBroker = AndroidControlAccessBroker(this)
        runner = AndroidAutomationRunner(
            queryProlog = session::queryLocalProlog,
            openApp = OpenAppAdapter(AndroidAppLauncher(this)),
            openUri = OpenUriAdapter(AndroidUriLauncher(this)),
            appSearch = AppSearchAdapter(AndroidAppSearchLauncher(this)),
            accessibility = AccessibilityAutomationAdapter(),
            adb = AndroidAdbAutomationAdapter(this),
            accessGranted = accessBroker::isGranted,
        )
        visionControl = AndroidAdbVisionConversationControl(
            context = this,
            appSession = session,
            requestApproval = ::requestVisionApproval,
        )
        refreshAccess()
        seedDemo().whenComplete { _, error ->
            runOnUiThread {
                status = error?.cause?.message ?: error?.message ?: "Prolog automation ready"
            }
        }

        val selectedTheme = ThemePreferenceStore(File(filesDir, "theme.bin")).load()
        setContent {
            val tokens = themeTokens(
                selectedTheme,
                systemDark = isSystemInDarkTheme(),
                reducedGlow = false,
            )
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
                    AutomationScreen(
                        status = status,
                        busy = busy,
                        visionBusy = visionBusy,
                        pendingVisionApproval = pendingVisionApproval,
                        access = access,
                        onRun = ::runAutomation,
                        onRunVision = ::runVision,
                        onCancelVision = ::cancelVision,
                        onResolveVisionApproval = ::resolveVisionApproval,
                        onRequestAccess = ::requestAccess,
                        onImportGit = ::importGitTemplate,
                    )
                }
            }
        }
    }

    override fun onResume() {
        super.onResume()
        if (::accessBroker.isInitialized) refreshAccess()
    }

    override fun onDestroy() {
        pendingVisionApproval?.future?.complete(false)
        pendingVisionApproval = null
        if (::visionControl.isInitialized) visionControl.close()
        io.shutdownNow()
        super.onDestroy()
    }

    private fun seedDemo(): CompletableFuture<*> {
        val session = (application as ZaraApplication).appSession
        if (session.prologSources().any { it.name == DEMO.fileName }) {
            return CompletableFuture.completedFuture(Unit)
        }
        return CompletableFuture.supplyAsync(
            {
                repeat(100) {
                    val state = session.localServerState()
                    when (state.phase) {
                        LocalServerPhase.READY -> return@supplyAsync Unit
                        LocalServerPhase.FAILED -> error(state.failure ?: "Local Prolog runtime failed")
                        else -> Thread.sleep(25)
                    }
                }
                error("Local Prolog runtime did not become ready")
            },
            io,
        ).thenCompose {
            if (session.prologSources().any { source -> source.name == DEMO.fileName }) {
                CompletableFuture.completedFuture(Unit)
            } else {
                session.savePrologSource(DEMO.fileName, DEMO.source).thenApply { Unit }
            }
        }
    }

    private fun runAutomation(name: String) {
        if (busy) return
        busy = true
        status = "Running $name…"
        runner.run(name).whenComplete { result, error ->
            runOnUiThread {
                busy = false
                status = when {
                    error != null -> error.cause?.message ?: error.message ?: "Automation failed"
                    result is AndroidAutomationResult.Completed ->
                        "Completed ${result.plan.name}: ${result.plan.actions.size} action(s)"
                    result is AndroidAutomationResult.NeedsAccess -> {
                        requestAccess(result.access)
                        "${result.plan.name} needs ${result.access.name}; Android opened the grant screen. Run it again after granting access."
                    }
                    result is AndroidAutomationResult.Failed ->
                        "${result.plan.name} stopped at action ${result.actionIndex + 1}: ${result.error.wireId}"
                    else -> "Automation failed"
                }
            }
        }
    }

    private fun runVision(goal: String) {
        val normalized = goal.trim()
        if (normalized.isEmpty()) {
            status = "Describe the Android state you want Zara to reach"
            return
        }
        if (busy) return
        busy = true
        visionBusy = true
        status = "Observing the authorized ADB target…"
        io.execute {
            visionControl.run(goal).whenComplete { result, error ->
                runOnUiThread {
                    pendingVisionApproval?.future?.complete(false)
                    pendingVisionApproval = null
                    visionBusy = false
                    busy = false
                    status = when {
                        error != null -> error.cause?.message ?: error.message ?: "ADB vision control failed"
                        result is AndroidVisionLoopResult.Completed ->
                            "Verified: ${result.summary} (${result.steps} action(s), ${result.observedBytes} observed bytes)"
                        result is AndroidVisionLoopResult.CapabilityUnavailable ->
                            "ADB capability is unavailable for the proposed typed action"
                        result is AndroidVisionLoopResult.ApprovalRejected ->
                            "ADB vision action was not approved"
                        result is AndroidVisionLoopResult.PolicyRejected ->
                            "Prolog policy rejected the proposed ADB action"
                        result is AndroidVisionLoopResult.VerificationFailed ->
                            "Fresh screenshot did not verify the approved ADB action"
                        result is AndroidVisionLoopResult.ActionFailed ->
                            "ADB action failed: ${result.message ?: "unknown failure"}"
                        result is AndroidVisionLoopResult.Unavailable ->
                            "ADB vision unavailable: ${result.reason}"
                        result is AndroidVisionLoopResult.BoundsExceeded ->
                            "ADB vision stopped at its safety bound: ${result.reason}"
                        result is AndroidVisionLoopResult.Failed ->
                            "ADB vision failed: ${result.reason}"
                        result == AndroidVisionLoopResult.Cancelled -> "ADB vision cancelled"
                        else -> "ADB vision failed"
                    }
                }
            }
        }
    }

    private fun cancelVision() {
        if (!visionBusy) return
        pendingVisionApproval?.future?.complete(false)
        pendingVisionApproval = null
        status = "Cancelling ADB vision…"
        io.execute {
            visionControl.cancel()
            runOnUiThread {
                if (visionBusy) {
                    visionBusy = false
                    busy = false
                    status = "ADB vision cancelled"
                }
            }
        }
    }

    private fun requestVisionApproval(
        action: AndroidAutomationAction,
    ): CompletableFuture<Boolean> {
        val decision = CompletableFuture<Boolean>()
        runOnUiThread {
            if (isFinishing || isDestroyed || !visionBusy) {
                decision.complete(false)
                return@runOnUiThread
            }
            pendingVisionApproval?.future?.complete(false)
            pendingVisionApproval = VisionApprovalRequest(action, decision)
            status = "Approval required before the typed ADB action can run"
        }
        return decision
    }

    private fun resolveVisionApproval(approved: Boolean) {
        val pending = pendingVisionApproval ?: return
        pendingVisionApproval = null
        status = if (approved) {
            "Executing approved ADB action; fresh verification will follow…"
        } else {
            "ADB action rejected"
        }
        io.execute {
            pending.future.complete(approved)
        }
    }

    private fun requestAccess(request: AndroidControlAccess) {
        val intent = accessBroker.requestIntent(request)
        if (intent == null) {
            status = "${request.name} is unavailable on this device"
            return
        }
        startActivity(intent)
    }

    private fun refreshAccess() {
        access = accessBroker.snapshot().associate { it.access to it.granted }
    }

    private fun importGitTemplate(repository: String, ref: String) {
        if (busy) return
        busy = true
        status = "Importing Git config template…"
        val session = (application as ZaraApplication).appSession
        CompletableFuture.supplyAsync(
            {
                GitConfigTemplateImporter(File(cacheDir, "zara-config-templates"))
                    .import(repository, ref.trim().ifEmpty { null })
            },
            io,
        ).thenCompose { template ->
            session.importPrologWorkspace(workspaceBundle(template.sources)).thenApply { template }
        }.whenComplete { template, error ->
            runOnUiThread {
                busy = false
                status = if (error != null) {
                    error.cause?.message ?: error.message ?: "Template import failed"
                } else {
                    "Imported ${template.name} @ ${template.revision.take(12)} (${template.sources.size} source(s))"
                }
            }
        }
    }

    private fun workspaceBundle(sources: List<PrologSource>): String = buildString {
        append("ZARA-PROLOG-WORKSPACE/1\n")
        sources.sortedBy(PrologSource::name).forEach { source ->
            val normalized = source.text.replace("\r\n", "\n").replace('\r', '\n')
            val text = if (normalized.endsWith('\n')) normalized else normalized + "\n"
            append("SOURCE ").append(source.name).append(' ')
                .append(text.encodeToByteArray().size).append('\n')
            append(text)
            append("END-SOURCE\n")
        }
    }

    private companion object {
        val DEMO = AndroidAutomationCatalog.examples.single()
    }
}

@Composable
private fun AutomationScreen(
    status: String,
    busy: Boolean,
    visionBusy: Boolean,
    pendingVisionApproval: VisionApprovalRequest?,
    access: Map<AndroidControlAccess, Boolean>,
    onRun: (String) -> Unit,
    onRunVision: (String) -> Unit,
    onCancelVision: () -> Unit,
    onResolveVisionApproval: (Boolean) -> Unit,
    onRequestAccess: (AndroidControlAccess) -> Unit,
    onImportGit: (String, String) -> Unit,
) {
    var repository by androidx.compose.runtime.remember { mutableStateOf("") }
    var ref by androidx.compose.runtime.remember { mutableStateOf("") }
    var visionGoal by androidx.compose.runtime.remember { mutableStateOf("") }

    pendingVisionApproval?.let { request ->
        AlertDialog(
            onDismissRequest = { onResolveVisionApproval(false) },
            title = { Text("Approve Android action?") },
            text = {
                Text(
                    describeVisionAction(request.action) +
                        "\n\nProlog policy requires confirmation. Zara will capture a fresh screenshot after the action and will not report success unless the postcondition verifies.",
                )
            },
            confirmButton = {
                Button(onClick = { onResolveVisionApproval(true) }) { Text("Approve") }
            },
            dismissButton = {
                Button(onClick = { onResolveVisionApproval(false) }) { Text("Reject") }
            },
        )
    }

    Column(
        modifier = Modifier.fillMaxSize().verticalScroll(rememberScrollState()).padding(20.dp),
        verticalArrangement = Arrangement.spacedBy(16.dp),
    ) {
        Text("Prolog Automation", style = MaterialTheme.typography.headlineSmall)
        Text(status, style = MaterialTheme.typography.bodyMedium)

        Text("ADB vision control", style = MaterialTheme.typography.titleMedium)
        Text(
            "Uses the authorized wireless ADB target, the selected local image-capable model, Prolog policy, and one approved typed action at a time.",
            style = MaterialTheme.typography.bodySmall,
        )
        OutlinedTextField(
            value = visionGoal,
            onValueChange = { visionGoal = it.take(2_048) },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Goal") },
            singleLine = false,
            enabled = !busy,
        )
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Button(
                modifier = Modifier.weight(1f),
                enabled = !busy && visionGoal.isNotBlank(),
                onClick = { onRunVision(visionGoal) },
            ) {
                Text("Observe & run")
            }
            Button(
                modifier = Modifier.weight(1f),
                enabled = visionBusy,
                onClick = onCancelVision,
            ) {
                Text("Cancel")
            }
        }

        Text("Demo", style = MaterialTheme.typography.titleMedium)
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Button(
                modifier = Modifier.weight(1f),
                enabled = !busy,
                onClick = { onRun("youtube_psytrance") },
            ) {
                Text("YouTube · psytrance")
            }
            Button(
                modifier = Modifier.weight(1f),
                enabled = !busy,
                onClick = { onRun("revanced_psytrance") },
            ) {
                Text("ReVanced · psytrance")
            }
        }
        Button(enabled = !busy, onClick = { onRun("psytrance_both") }) {
            Text("Run both")
        }

        Text("Android control access", style = MaterialTheme.typography.titleMedium)
        Text("Android grants each special access explicitly. Prolog rules cannot self-grant permissions.")
        AndroidControlAccess.entries.forEach { item ->
            Row(
                modifier = Modifier.fillMaxWidth(),
                verticalAlignment = Alignment.CenterVertically,
                horizontalArrangement = Arrangement.spacedBy(12.dp),
            ) {
                Text(
                    "${item.name}: ${if (access[item] == true) "granted" else "not granted"}",
                    modifier = Modifier.weight(1f),
                )
                if (access[item] != true) {
                    Button(onClick = { onRequestAccess(item) }) { Text("Request") }
                }
            }
        }

        Text("Git config template", style = MaterialTheme.typography.titleMedium)
        OutlinedTextField(
            value = repository,
            onValueChange = { repository = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("https://…/.git") },
            singleLine = true,
        )
        OutlinedTextField(
            value = ref,
            onValueChange = { ref = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("branch/tag (optional)") },
            singleLine = true,
        )
        Button(
            enabled = !busy && repository.startsWith("https://"),
            onClick = { onImportGit(repository.trim(), ref.trim()) },
        ) {
            Text("Clone, validate & import")
        }
        Text(
            "Templates must declare zara-template.properties or .zara/config-template.properties. " +
                "They may point to a .pl directory or Org file; with no source declaration Zara uses " +
                ".config/zarathushtra/android as the canonical dotfiles layout.",
            style = MaterialTheme.typography.bodySmall,
        )
    }
}

private fun describeVisionAction(action: AndroidAutomationAction): String = when (action) {
    is AndroidAutomationAction.AdbTap -> "Tap at (${action.x}, ${action.y})"
    is AndroidAutomationAction.AdbSwipe ->
        "Swipe (${action.x1}, ${action.y1}) → (${action.x2}, ${action.y2}) for ${action.durationMs} ms"
    is AndroidAutomationAction.AdbText -> "Type ${action.text.length} character(s)"
    is AndroidAutomationAction.AdbKey -> "Press ${action.key.name.lowercase()}"
    is AndroidAutomationAction.AdbWait -> "Wait ${action.durationMs} ms"
    else -> "Unsupported action"
}
