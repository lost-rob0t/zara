package ai.zara.org.notebook

import ai.zara.org.core.OrgExecutionFence
import ai.zara.org.core.OrgExecutionPrepareResult
import ai.zara.org.core.OrgExecutionProvider
import ai.zara.org.core.OrgExecutionRegistry
import ai.zara.org.core.OrgExecutionRequest
import ai.zara.org.core.OrgExecutionResult
import ai.zara.org.core.OrgExecutionStatus
import ai.zara.org.core.OrgExecutionToken
import ai.zara.org.core.OrgNotebookBlock
import ai.zara.org.core.OrgNotebookBlocks
import ai.zara.org.core.OrgNotebookResults
import ai.zara.org.core.OrgResultApplyResult
import ai.zara.org.storage.OrgFileRef
import ai.zara.org.storage.OrgHome
import ai.zara.org.storage.OrgHomeMode
import ai.zara.org.storage.OrgRepository
import ai.zara.ui.org.OrgTextRenderer
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.compose.setContent
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.weight
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material3.Button
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.darkColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp

private enum class NotebookTab { EDIT, RENDER, BLOCKS }

private val NotebookScheme = darkColorScheme(
    primary = Color(0xFFFF315D),
    secondary = Color(0xFF45E6FF),
    background = Color(0xFF050509),
    surface = Color(0xFF0D0D18),
    surfaceVariant = Color(0xFF171725),
    onBackground = Color(0xFFF6F2F4),
    onSurface = Color(0xFFF6F2F4),
    onSurfaceVariant = Color(0xFFBEB8CB),
)

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MaterialTheme(colorScheme = NotebookScheme) {
                NotebookWorkbench()
            }
        }
    }
}

@Composable
private fun NotebookWorkbench() {
    val context = LocalContext.current
    var homeRevision by remember { mutableStateOf(0) }
    val homeSelection = remember(homeRevision) { OrgHome.selection(context) }
    var files by remember { mutableStateOf(emptyList<OrgFileRef>()) }
    var selected by remember { mutableStateOf<OrgFileRef?>(null) }
    var source by remember { mutableStateOf("") }
    var sourceRevision by remember { mutableStateOf(0L) }
    var tab by remember { mutableStateOf(NotebookTab.EDIT) }
    var status by remember { mutableStateOf("Open an Org workspace") }
    var runtimeConnected by remember { mutableStateOf(false) }
    var activeRequest by remember { mutableStateOf<OrgExecutionRequest?>(null) }
    var activeToken by remember { mutableStateOf<OrgExecutionToken?>(null) }
    var deliveredResult by remember { mutableStateOf<OrgExecutionResult?>(null) }
    val executionFence = remember { OrgExecutionFence() }
    val registry = remember {
        OrgExecutionRegistry(
            listOf(
                RemoteProviderDescriptor("prolog", setOf("results")),
                RemoteProviderDescriptor("python", setOf("results")),
            ),
        )
    }

    val repository: OrgRepository? = remember(homeRevision) {
        runCatching { OrgHome.open(context) }.getOrNull()
    }

    fun refreshFiles() {
        val repo = repository ?: return
        runCatching { repo.listOrgFiles() }
            .onSuccess {
                files = it
                status = "${it.size} Org files"
            }
            .onFailure { status = it.message ?: "Org refresh failed" }
    }

    fun open(file: OrgFileRef) {
        val repo = repository ?: return
        runCatching { repo.read(file) }
            .onSuccess {
                selected = file
                source = it
                sourceRevision = 0
                activeRequest = null
                activeToken = null
                executionFence.cancel()
                status = file.relativePath
                tab = NotebookTab.EDIT
            }
            .onFailure { status = it.message ?: "Open failed" }
    }

    fun save() {
        val repo = repository ?: return
        val file = selected ?: return
        runCatching { repo.write(file, source) }
            .onSuccess { status = "Saved ${file.relativePath}" }
            .onFailure { status = it.message ?: "Save failed" }
    }

    val client = remember {
        OrgNotebookExecutionClient(
            context = context.applicationContext,
            onConnectionChanged = { runtimeConnected = it },
            onResult = { deliveredResult = it },
            onError = { status = it },
        )
    }
    DisposableEffect(client) {
        client.bind()
        onDispose { client.close() }
    }

    LaunchedEffect(repository) { refreshFiles() }

    LaunchedEffect(deliveredResult) {
        val result = deliveredResult ?: return@LaunchedEffect
        deliveredResult = null
        val request = activeRequest ?: return@LaunchedEffect
        val token = activeToken ?: return@LaunchedEffect
        if (result.requestId != request.requestId) return@LaunchedEffect

        when (result.status) {
            OrgExecutionStatus.CANCELLED -> status = "Block run cancelled"
            OrgExecutionStatus.FAILED -> status = result.stderr.ifBlank { "Block run failed" }
            OrgExecutionStatus.SUCCEEDED -> {
                if (!executionFence.accepts(token, sourceRevision, result.blockHash)) {
                    status = "Result is stale; document changed while block ran"
                } else {
                    when (val applied = OrgNotebookResults.apply(source, sourceRevision, request, result)) {
                        is OrgResultApplyResult.Applied -> {
                            source = applied.source
                            sourceRevision = applied.nextRevision
                            selected?.let { file -> repository?.write(file, applied.source) }
                            status = "Result saved · ${result.durationMs} ms · runtime ${result.runtimeGeneration}"
                        }
                        is OrgResultApplyResult.Stale -> status = "Result is stale: ${applied.reason}"
                        is OrgResultApplyResult.Rejected -> status = applied.reason
                    }
                }
            }
        }
        activeRequest = null
        activeToken = null
    }

    val projectPicker = rememberLauncherForActivityResult(ActivityResultContracts.OpenDocumentTree()) { uri ->
        if (uri != null) {
            runCatching { OrgHome.useCustomSaf(context, uri) }
                .onSuccess {
                    homeRevision += 1
                    selected = null
                    source = ""
                    sourceRevision = 0
                    status = "Custom Org home connected"
                }
                .onFailure { status = it.message ?: "Unable to retain Org workspace permission" }
        }
    }

    val blocks = remember(source, selected) {
        OrgNotebookBlocks.scan(source, selected?.relativePath.orEmpty())
    }

    Column(
        modifier = Modifier
            .fillMaxSize()
            .background(MaterialTheme.colorScheme.background)
            .padding(12.dp),
        verticalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        Row(modifier = Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
            Column {
                Text("Zara Org Notebook", style = MaterialTheme.typography.titleLarge)
                Text(
                    buildString {
                        append(selected?.relativePath ?: "No file")
                        append(" · r").append(sourceRevision)
                        append(if (runtimeConnected) " · Zara runtime" else " · runtime offline")
                    },
                    style = MaterialTheme.typography.labelSmall,
                )
            }
            Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
                TextButton(onClick = {
                    OrgHome.useShared(context)
                    homeRevision += 1
                    selected = null
                    source = ""
                    sourceRevision = 0
                    status = "Using shared Org home"
                }) { Text("Shared home") }
                TextButton(onClick = { projectPicker.launch(homeSelection.customTreeUri) }) {
                    Text("Custom dir")
                }
            }
        }

        if (files.isNotEmpty()) {
            Row(
                modifier = Modifier.fillMaxWidth().horizontalScroll(rememberScrollState()),
                horizontalArrangement = Arrangement.spacedBy(4.dp),
            ) {
                files.forEach { file ->
                    Text(
                        file.relativePath,
                        modifier = Modifier
                            .background(if (file == selected) MaterialTheme.colorScheme.surface else Color.Transparent)
                            .clickable { open(file) }
                            .padding(horizontal = 8.dp, vertical = 6.dp),
                        fontFamily = FontFamily.Monospace,
                        style = MaterialTheme.typography.labelSmall,
                    )
                }
            }
        }

        Row(horizontalArrangement = Arrangement.spacedBy(6.dp)) {
            NotebookTab.entries.forEach { destination ->
                TextButton(onClick = { tab = destination }, enabled = selected != null) {
                    Text(destination.name.lowercase().replaceFirstChar(Char::uppercaseChar))
                }
            }
            Button(onClick = ::save, enabled = selected != null) { Text("Save") }
            TextButton(
                enabled = selected != null,
                onClick = {
                    val repo = repository
                    val file = selected
                    if (repo != null && file != null) {
                        runCatching { repo.write(file, source); repo.tangle(file) }
                            .onSuccess { outputs -> status = "Tangled ${outputs.joinToString { it.relativePath }}" }
                            .onFailure { status = it.message ?: "Tangle failed" }
                    }
                },
            ) { Text("Tangle") }
            if (activeRequest != null) {
                TextButton(onClick = {
                    val request = activeRequest
                    if (request != null) client.cancel(request.requestId)
                    executionFence.cancel()
                    activeRequest = null
                    activeToken = null
                    status = "Block run cancelled"
                }) { Text("Stop") }
            }
        }

        Text(status, color = MaterialTheme.colorScheme.secondary, style = MaterialTheme.typography.labelSmall)

        if (repository == null) {
            if (homeSelection.mode == OrgHomeMode.SHARED) {
                Text("Shared Org home is the default. Install/open Org Sync, or choose a custom directory only for Notebook.")
            } else {
                Text("The custom Org directory is unavailable; re-grant it or switch back to the shared home.")
            }
        } else if (selected == null) {
            Text("Open a .org file. Source blocks never execute on open or render.")
        } else {
            when (tab) {
                NotebookTab.EDIT -> OutlinedTextField(
                    value = source,
                    onValueChange = {
                        if (it != source) sourceRevision += 1
                        source = it
                    },
                    modifier = Modifier.fillMaxWidth().weight(1f),
                    textStyle = MaterialTheme.typography.bodyMedium.copy(fontFamily = FontFamily.Monospace),
                    label = { Text("Canonical Org source") },
                )
                NotebookTab.RENDER -> {
                    val rendered = remember(source) { OrgTextRenderer.renderSource(source, baseFontSp = 16f) }
                    SelectionContainer {
                        Text(
                            rendered.annotated,
                            modifier = Modifier.fillMaxSize(),
                            fontFamily = FontFamily.Monospace,
                        )
                    }
                }
                NotebookTab.BLOCKS -> LazyColumn(
                    modifier = Modifier.fillMaxWidth().weight(1f),
                    verticalArrangement = Arrangement.spacedBy(8.dp),
                ) {
                    items(blocks, key = { it.id }) { block ->
                        BlockCard(
                            block = block,
                            running = activeRequest?.blockId == block.id,
                            canRun = activeRequest == null && runtimeConnected,
                            onRun = {
                                val file = selected ?: return@BlockCard
                                val prepared = registry.prepareExplicitRun(
                                    source = source,
                                    documentId = file.relativePath,
                                    sourceRevision = sourceRevision,
                                    blockId = block.id,
                                    principal = "local-user",
                                    deadlineEpochMs = System.currentTimeMillis() + 15_000,
                                )
                                when (prepared) {
                                    is OrgExecutionPrepareResult.Rejected -> status = prepared.reason
                                    is OrgExecutionPrepareResult.Ready -> {
                                        val token = executionFence.begin(sourceRevision, block.hash)
                                        activeToken = token
                                        activeRequest = prepared.request
                                        if (client.run(prepared.request)) {
                                            status = "Running ${block.language} · ${block.name ?: block.id}"
                                        } else {
                                            activeToken = null
                                            activeRequest = null
                                            status = "Zara runtime is unavailable"
                                            client.bind()
                                        }
                                    }
                                }
                            },
                        )
                    }
                }
            }
        }
    }
}

@Composable
private fun BlockCard(
    block: OrgNotebookBlock,
    running: Boolean,
    canRun: Boolean,
    onRun: () -> Unit,
) {
    Surface(color = MaterialTheme.colorScheme.surface, shape = MaterialTheme.shapes.medium) {
        Column(modifier = Modifier.fillMaxWidth().padding(10.dp), verticalArrangement = Arrangement.spacedBy(6.dp)) {
            Row(modifier = Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
                Column {
                    Text(block.name ?: block.id, fontFamily = FontFamily.Monospace)
                    Text(
                        "${block.language} · lines ${block.beginLine}-${block.endLine}",
                        style = MaterialTheme.typography.labelSmall,
                        color = MaterialTheme.colorScheme.onSurfaceVariant,
                    )
                }
                Button(onClick = onRun, enabled = canRun) { Text(if (running) "Running…" else "Run") }
            }
            Text(
                block.body.take(500),
                fontFamily = FontFamily.Monospace,
                style = MaterialTheme.typography.bodySmall,
            )
        }
    }
}

private class RemoteProviderDescriptor(
    override val language: String,
    override val supportedHeaderArgs: Set<String>,
) : OrgExecutionProvider {
    override fun run(request: OrgExecutionRequest): OrgExecutionResult =
        error("Remote notebook providers are asynchronous; use OrgNotebookExecutionClient")
}
