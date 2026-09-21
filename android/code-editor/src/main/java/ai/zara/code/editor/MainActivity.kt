package ai.zara.code.editor

import ai.zara.editor.core.ApplyResult
import ai.zara.editor.core.EditorEditPlan
import ai.zara.editor.core.EditorSelection
import ai.zara.editor.core.NavigationTarget
import ai.zara.editor.core.OperationFence
import ai.zara.editor.core.OperationToken
import ai.zara.editor.core.RevisionedEditorBuffer
import ai.zara.editor.core.SpokenCodeResolution
import ai.zara.editor.core.SpokenCodeRouter
import ai.zara.editor.core.VoiceAction
import ai.zara.editor.core.VoiceCodePlanner
import android.Manifest
import android.content.pm.PackageManager
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
import androidx.compose.foundation.rememberScrollState
import androidx.compose.material3.AlertDialog
import androidx.compose.material3.Button
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.darkColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberUpdatedState
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.dp
import kotlin.math.max
import kotlin.math.min

private val CodeScheme = darkColorScheme(
    primary = Color(0xFFFF315D),
    secondary = Color(0xFF45E6FF),
    background = Color(0xFF050509),
    surface = Color(0xFF0D0D14),
    onBackground = Color(0xFFF6F2F4),
    onSurface = Color(0xFFF6F2F4),
)

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MaterialTheme(colorScheme = CodeScheme) {
                CodeEditorWorkbench()
            }
        }
    }
}

@Composable
private fun CodeEditorWorkbench() {
    val context = LocalContext.current
    var treeUri by remember { mutableStateOf(CodeTreeRepository.remembered(context)) }
    var files by remember { mutableStateOf(emptyList<CodeFileRef>()) }
    var selected by remember { mutableStateOf<CodeFileRef?>(null) }
    var editorValue by remember { mutableStateOf(TextFieldValue("")) }
    var buffer by remember { mutableStateOf<RevisionedEditorBuffer?>(null) }
    var status by remember { mutableStateOf("Open a project tree") }
    var listening by remember { mutableStateOf(false) }
    var pendingPlan by remember { mutableStateOf<EditorEditPlan?>(null) }
    var speechToken by remember { mutableStateOf<OperationToken?>(null) }
    val speechFence = remember { OperationFence() }

    val repository = remember(treeUri) {
        treeUri?.let { uri -> runCatching { CodeTreeRepository(context, uri) }.getOrNull() }
    }

    val currentBuffer = rememberUpdatedState(buffer)
    val currentEditor = rememberUpdatedState(editorValue)
    val currentToken = rememberUpdatedState(speechToken)

    fun refresh() {
        val repo = repository ?: return
        runCatching { repo.listCodeFiles() }
            .onSuccess {
                files = it
                status = "${it.size} editable files"
            }
            .onFailure { status = it.message ?: "Project refresh failed" }
    }

    fun open(file: CodeFileRef) {
        val repo = repository ?: return
        runCatching { repo.read(file) }
            .onSuccess { source ->
                selected = file
                editorValue = TextFieldValue(source, TextRange(source.length))
                buffer = RevisionedEditorBuffer(
                    initialText = source,
                    bufferId = file.uri.toString(),
                    languageId = CodeLanguage.fromFileName(file.name),
                )
                status = file.relativePath
            }
            .onFailure { status = it.message ?: "Open failed" }
    }

    fun applyPlan(plan: EditorEditPlan) {
        val active = buffer ?: return
        when (val result = active.apply(plan)) {
            is ApplyResult.Applied -> {
                val snapshot = result.snapshot
                editorValue = TextFieldValue(snapshot.text, TextRange(snapshot.cursor))
                status = plan.summary
            }
            is ApplyResult.Stale -> status = "Voice edit stale; source changed"
            is ApplyResult.Invalid -> status = result.reason
        }
    }

    fun uiSnapshot() = currentBuffer.value?.snapshot()?.let { snapshot ->
        val value = currentEditor.value
        val start = min(value.selection.start, value.selection.end).coerceIn(0, value.text.length)
        val end = max(value.selection.start, value.selection.end).coerceIn(start, value.text.length)
        snapshot.copy(
            cursor = value.selection.end.coerceIn(0, value.text.length),
            selection = EditorSelection(start, end),
        )
    }

    fun navigate(target: NavigationTarget) {
        val value = editorValue
        when (target) {
            is NavigationTarget.Line -> {
                val offset = lineStartOffset(value.text, target.line)
                val next = value.copy(selection = TextRange(offset))
                editorValue = next
                syncEditorStateToBuffer(buffer, next)
                status = "Line ${target.line}"
            }
            is NavigationTarget.Search -> {
                val from = value.selection.end.coerceIn(0, value.text.length)
                val forward = value.text.indexOf(target.query, from, ignoreCase = true)
                val index = if (forward >= 0) forward else value.text.indexOf(target.query, ignoreCase = true)
                if (index >= 0) {
                    val next = value.copy(selection = TextRange(index, index + target.query.length))
                    editorValue = next
                    syncEditorStateToBuffer(buffer, next)
                    status = "Found ${target.query}"
                } else status = "Not found: ${target.query}"
            }
            is NavigationTarget.Symbol -> status = "Symbol navigation needs language service: ${target.name}"
        }
    }

    fun handleTranscript(transcript: String) {
        val snapshot = uiSnapshot() ?: run {
            status = "Open a file before voice coding"
            return
        }
        val token = currentToken.value
        if (token == null || !speechFence.accepts(token, snapshot.revision)) {
            status = "Ignored stale voice result"
            return
        }
        status = "Heard: $transcript"
        when (val resolved = SpokenCodeRouter.resolve(transcript, snapshot.languageId)) {
            is SpokenCodeResolution.Rejected -> status = resolved.reason
            is SpokenCodeResolution.NeedsModel -> {
                status = "Zara model edit queued for bridge: ${resolved.instruction}"
            }
            is SpokenCodeResolution.Intent -> when (val action = VoiceCodePlanner.plan(resolved.intent, snapshot)) {
                is VoiceAction.Edit -> {
                    if (action.plan.requiresConfirmation) pendingPlan = action.plan else applyPlan(action.plan)
                }
                is VoiceAction.Navigate -> navigate(action.target)
                is VoiceAction.Rejected -> status = action.reason
                is VoiceAction.LanguageService -> status = "Language service action: ${action.request.kind}"
                is VoiceAction.ReadOnly -> status = "Read-only Zara action ready"
                is VoiceAction.RuntimeCommand -> status = "Runtime command ready: ${action.command}"
            }
        }
    }

    val speech = remember {
        AndroidSpeechInput(
            context = context.applicationContext,
            onListening = { listening = it },
            onTranscript = ::handleTranscript,
            onError = { status = it },
        )
    }
    DisposableEffect(speech) { onDispose { speech.close() } }

    fun startVoice() {
        val snapshot = uiSnapshot() ?: run {
            status = "Open a file before voice coding"
            return
        }
        speechToken = speechFence.begin(snapshot.revision)
        speech.start()
    }

    val microphonePermission = rememberLauncherForActivityResult(ActivityResultContracts.RequestPermission()) { granted ->
        if (granted) startVoice() else status = "Microphone permission denied"
    }
    val projectPicker = rememberLauncherForActivityResult(ActivityResultContracts.OpenDocumentTree()) { uri ->
        if (uri != null) {
            runCatching { CodeTreeRepository.remember(context, uri) }
                .onSuccess {
                    treeUri = uri
                    selected = null
                    buffer = null
                    editorValue = TextFieldValue("")
                    status = "Project connected"
                }
                .onFailure { status = it.message ?: "Unable to retain project permission" }
        }
    }

    LaunchedEffect(repository) { refresh() }

    Column(
        modifier = Modifier
            .fillMaxSize()
            .background(MaterialTheme.colorScheme.background)
            .padding(12.dp),
        verticalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        Row(modifier = Modifier.fillMaxWidth(), horizontalArrangement = Arrangement.SpaceBetween) {
            Column {
                Text("Zara Code", style = MaterialTheme.typography.titleLarge)
                Text(
                    selected?.let { "${it.relativePath} · ${CodeLanguage.fromFileName(it.name)}" } ?: status,
                    style = MaterialTheme.typography.labelSmall,
                )
            }
            TextButton(onClick = { projectPicker.launch(treeUri) }) {
                Text(if (treeUri == null) "Open project" else "Change project")
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
            Button(
                enabled = selected != null,
                onClick = {
                    val file = selected
                    val repo = repository
                    if (file != null && repo != null) {
                        runCatching { repo.write(file, editorValue.text) }
                            .onSuccess { status = "Saved ${file.relativePath}" }
                            .onFailure { status = it.message ?: "Save failed" }
                    }
                },
            ) { Text("Save") }
            Button(
                enabled = buffer != null && !listening,
                onClick = {
                    if (context.checkSelfPermission(Manifest.permission.RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED) {
                        startVoice()
                    } else microphonePermission.launch(Manifest.permission.RECORD_AUDIO)
                },
            ) { Text(if (listening) "Listening…" else "Voice") }
            TextButton(
                enabled = listening,
                onClick = {
                    speechFence.cancel()
                    speechToken = null
                    speech.cancel()
                    status = "Voice cancelled"
                },
            ) { Text("Cancel voice") }
            TextButton(onClick = { refresh() }, enabled = repository != null) { Text("Refresh") }
        }

        Text(status, color = MaterialTheme.colorScheme.secondary, style = MaterialTheme.typography.labelSmall)

        if (selected == null) {
            Text("Choose a project tree, then open a code file. Voice accepts editor commands and code dictation.")
        } else {
            OutlinedTextField(
                value = editorValue,
                onValueChange = { next ->
                    val active = buffer
                    editorValue = next
                    syncEditorStateToBuffer(active, next)
                },
                modifier = Modifier.fillMaxWidth().weight(1f),
                textStyle = MaterialTheme.typography.bodyMedium.copy(fontFamily = FontFamily.Monospace),
                label = { Text("Code") },
            )
        }
    }

    pendingPlan?.let { plan ->
        AlertDialog(
            onDismissRequest = { pendingPlan = null },
            title = { Text("Apply voice edit?") },
            text = { Text(plan.summary) },
            confirmButton = {
                TextButton(onClick = {
                    pendingPlan = null
                    applyPlan(plan)
                }) { Text("Apply") }
            },
            dismissButton = {
                TextButton(onClick = { pendingPlan = null }) { Text("Reject") }
            },
        )
    }
}

internal fun syncEditorStateToBuffer(active: RevisionedEditorBuffer?, next: TextFieldValue) {
    active ?: return
    val start = min(next.selection.start, next.selection.end).coerceIn(0, next.text.length)
    val end = max(next.selection.start, next.selection.end).coerceIn(start, next.text.length)
    active.replaceFromUser(
        newText = next.text,
        newCursor = next.selection.end.coerceIn(0, next.text.length),
        newSelection = EditorSelection(start, end),
    )
}

private fun lineStartOffset(text: String, requestedLine: Int): Int {
    if (requestedLine <= 1) return 0
    var line = 1
    text.forEachIndexed { index, char ->
        if (char == '\n') {
            line += 1
            if (line == requestedLine) return index + 1
        }
    }
    return text.length
}
