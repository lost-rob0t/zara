package ai.zara.code.workbench

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
import ai.zara.prolog.ipc.PrologCall
import ai.zara.prolog.ipc.PrologClient
import ai.zara.ui.theme.ZaraSemanticTokens
import android.Manifest
import android.content.pm.PackageManager
import android.os.Handler
import android.os.Looper
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.BorderStroke
import androidx.compose.foundation.background
import androidx.compose.foundation.clickable
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.BoxWithConstraints
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.size
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.weight
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material3.Button
import androidx.compose.material3.ButtonDefaults
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.OutlinedTextFieldDefaults
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.rememberUpdatedState
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.text.input.TextFieldValue
import androidx.compose.ui.unit.dp
import kotlin.math.max
import kotlin.math.min

@Composable
fun CodeWorkbenchSurface(
    tokens: ZaraSemanticTokens,
    padding: PaddingValues = PaddingValues(0.dp),
    modifier: Modifier = Modifier,
    title: String = "Code",
) {
    val context = LocalContext.current
    var treeUri by remember { mutableStateOf(CodeTreeRepository.remembered(context)) }
    var files by remember { mutableStateOf(emptyList<CodeFileRef>()) }
    var selected by remember { mutableStateOf<CodeFileRef?>(null) }
    var editorValue by remember { mutableStateOf(TextFieldValue("")) }
    var savedText by remember { mutableStateOf("") }
    var buffer by remember { mutableStateOf<RevisionedEditorBuffer?>(null) }
    var status by remember { mutableStateOf("Open a project tree") }
    var listening by remember { mutableStateOf(false) }
    var pendingPlan by remember { mutableStateOf<EditorEditPlan?>(null) }
    var speechToken by remember { mutableStateOf<OperationToken?>(null) }
    var fileFilter by remember { mutableStateOf("") }
    var findQuery by remember { mutableStateOf("") }
    var prologGoal by remember { mutableStateOf("member(Result, [alpha, beta])") }
    var prologOutput by remember { mutableStateOf("ZARA-PROLOG/1 ready when main Zara is installed") }
    var prologBusy by remember { mutableStateOf(false) }
    var prologCall by remember { mutableStateOf<PrologCall?>(null) }
    val speechFence = remember { OperationFence() }
    val prologClient = remember { PrologClient(context.applicationContext) }
    val mainHandler = remember { Handler(Looper.getMainLooper()) }

    val repository = remember(treeUri) {
        treeUri?.let { uri -> runCatching { CodeTreeRepository(context, uri) }.getOrNull() }
    }
    val currentBuffer = rememberUpdatedState(buffer)
    val currentEditor = rememberUpdatedState(editorValue)
    val currentToken = rememberUpdatedState(speechToken)

    fun cancelProlog(message: String = "Cancelled") {
        prologCall?.cancel()
        prologCall = null
        prologBusy = false
        prologOutput = message
    }

    fun runProlog() {
        if (prologBusy) return
        val goal = prologGoal.trim()
        if (goal.isEmpty()) {
            prologOutput = "Enter a Prolog goal"
            return
        }
        val call = runCatching { prologClient.query(goal) }
            .getOrElse { error ->
                prologOutput = error.message ?: "Unable to start Prolog query"
                return
            }
        prologCall = call
        prologBusy = true
        prologOutput = "Running via ZARA-PROLOG/1…"
        call.result.whenComplete { reply, error ->
            mainHandler.post {
                if (prologCall !== call) return@post
                prologCall = null
                prologBusy = false
                prologOutput = when {
                    error != null -> error.message ?: "Prolog query failed"
                    reply == null -> "Prolog returned no result"
                    reply.terms.isEmpty() -> "No solutions"
                    else -> buildString {
                        append(reply.terms.joinToString("\n"))
                        if (reply.truncated) append("\n… truncated")
                        append("\n[generation ")
                        append(reply.generation)
                        append("]")
                    }
                }
            }
        }
    }

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
                cancelProlog("ZARA-PROLOG/1 ready when main Zara is installed")
                selected = file
                savedText = source
                editorValue = TextFieldValue(source, TextRange(source.length))
                buffer = RevisionedEditorBuffer(
                    initialText = source,
                    bufferId = file.uri.toString(),
                    languageId = CodeLanguage.fromFileName(file.name),
                )
                findQuery = ""
                status = file.relativePath
            }
            .onFailure { status = it.message ?: "Open failed" }
    }

    fun save() {
        val file = selected ?: return
        val repo = repository ?: return
        runCatching { repo.write(file, editorValue.text) }
            .onSuccess {
                savedText = editorValue.text
                status = "Saved ${file.relativePath}"
            }
            .onFailure { status = it.message ?: "Save failed" }
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
                findQuery = target.query
                val from = value.selection.end.coerceIn(0, value.text.length)
                val forward = value.text.indexOf(target.query, from, ignoreCase = true)
                val index = if (forward >= 0) forward else value.text.indexOf(target.query, ignoreCase = true)
                if (index >= 0) {
                    val next = value.copy(selection = TextRange(index, index + target.query.length))
                    editorValue = next
                    syncEditorStateToBuffer(buffer, next)
                    status = "Found ${target.query}"
                } else {
                    status = "Not found: ${target.query}"
                }
            }
            is NavigationTarget.Symbol -> status = "Symbol navigation needs a language service: ${target.name}"
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
        when (val resolved = SpokenCodeRouter.resolve(transcript, snapshot.languageId)) {
            is SpokenCodeResolution.Rejected -> status = resolved.reason
            is SpokenCodeResolution.NeedsModel -> {
                status = "Model edit ready for Zara coding bridge: ${resolved.instruction}"
            }
            is SpokenCodeResolution.Intent -> when (val action = VoiceCodePlanner.plan(resolved.intent, snapshot)) {
                is VoiceAction.Edit -> {
                    if (action.plan.requiresConfirmation) pendingPlan = action.plan else applyPlan(action.plan)
                }
                is VoiceAction.Navigate -> navigate(action.target)
                is VoiceAction.Rejected -> status = action.reason
                is VoiceAction.LanguageService -> status = "Language service: ${action.request.kind}"
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
    DisposableEffect(prologClient) {
        onDispose {
            prologCall?.cancel()
            prologClient.close()
        }
    }

    fun startVoice() {
        val snapshot = uiSnapshot() ?: run {
            status = "Open a file before voice coding"
            return
        }
        speechToken = speechFence.begin(snapshot.revision)
        speech.start()
    }

    val microphonePermission = rememberLauncherForActivityResult(
        ActivityResultContracts.RequestPermission()
    ) { granted ->
        if (granted) startVoice() else status = "Microphone permission denied"
    }
    val projectPicker = rememberLauncherForActivityResult(
        ActivityResultContracts.OpenDocumentTree()
    ) { uri ->
        if (uri != null) {
            runCatching { CodeTreeRepository.remember(context, uri) }
                .onSuccess {
                    treeUri = uri
                    selected = null
                    buffer = null
                    editorValue = TextFieldValue("")
                    savedText = ""
                    status = "Project connected"
                }
                .onFailure { status = it.message ?: "Unable to retain project permission" }
        }
    }

    LaunchedEffect(repository) { refresh() }

    val language = selected?.let { CodeLanguage.fromFileName(it.name) } ?: "none"
    val dirty = selected != null && editorValue.text != savedText
    val cursor = editorValue.selection.end.coerceIn(0, editorValue.text.length)
    val (line, column) = lineColumn(editorValue.text, cursor)
    val matches = if (findQuery.isBlank()) 0 else countMatches(editorValue.text, findQuery)

    Surface(
        modifier = modifier.fillMaxSize(),
        color = tokens.background,
    ) {
        BoxWithConstraints(
            modifier = Modifier
                .fillMaxSize()
                .padding(padding)
                .padding(horizontal = 14.dp, vertical = 12.dp)
        ) {
            val wide = maxWidth >= 760.dp
            Column(
                modifier = Modifier.fillMaxSize(),
                verticalArrangement = Arrangement.spacedBy(10.dp),
            ) {
                WorkbenchHeader(
                    title = title,
                    selected = selected,
                    language = language,
                    dirty = dirty,
                    status = status,
                    tokens = tokens,
                    onOpenProject = { projectPicker.launch(treeUri) },
                    onSave = ::save,
                    saveEnabled = selected != null && dirty,
                )

                if (wide) {
                    Row(
                        modifier = Modifier.fillMaxSize(),
                        horizontalArrangement = Arrangement.spacedBy(10.dp),
                    ) {
                        FileRail(
                            files = files,
                            selected = selected,
                            filter = fileFilter,
                            tokens = tokens,
                            modifier = Modifier.width(230.dp).fillMaxHeight(),
                            onFilter = { fileFilter = it },
                            onOpen = ::open,
                            onRefresh = ::refresh,
                        )
                        EditorPanel(
                            value = editorValue,
                            selected = selected,
                            findQuery = findQuery,
                            matches = matches,
                            tokens = tokens,
                            listening = listening,
                            modifier = Modifier.weight(1f).fillMaxHeight(),
                            onValue = { next ->
                                editorValue = next
                                syncEditorStateToBuffer(buffer, next)
                            },
                            onFind = { findQuery = it },
                            onVoice = {
                                if (context.checkSelfPermission(Manifest.permission.RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED) {
                                    startVoice()
                                } else {
                                    microphonePermission.launch(Manifest.permission.RECORD_AUDIO)
                                }
                            },
                            onCancelVoice = {
                                speechFence.cancel()
                                speechToken = null
                                speech.cancel()
                                status = "Voice cancelled"
                            },
                            footer = if (language == "prolog") {
                                {
                                    PrologConsole(
                                        goal = prologGoal,
                                        output = prologOutput,
                                        busy = prologBusy,
                                        tokens = tokens,
                                        onGoal = { prologGoal = it },
                                        onRun = ::runProlog,
                                        onCancel = { cancelProlog() },
                                    )
                                }
                            } else {
                                null
                            },
                        )
                        Inspector(
                            selected = selected,
                            language = language,
                            dirty = dirty,
                            line = line,
                            column = column,
                            revision = buffer?.snapshot()?.revision ?: 0,
                            selection = editorValue.selection,
                            tokens = tokens,
                            modifier = Modifier.width(245.dp).fillMaxHeight(),
                        )
                    }
                } else {
                    CompactFileStrip(
                        files = files,
                        selected = selected,
                        tokens = tokens,
                        onOpen = ::open,
                    )
                    EditorPanel(
                        value = editorValue,
                        selected = selected,
                        findQuery = findQuery,
                        matches = matches,
                        tokens = tokens,
                        listening = listening,
                        modifier = Modifier.weight(1f).fillMaxWidth(),
                        onValue = { next ->
                            editorValue = next
                            syncEditorStateToBuffer(buffer, next)
                        },
                        onFind = { findQuery = it },
                        onVoice = {
                            if (context.checkSelfPermission(Manifest.permission.RECORD_AUDIO) == PackageManager.PERMISSION_GRANTED) {
                                startVoice()
                            } else {
                                microphonePermission.launch(Manifest.permission.RECORD_AUDIO)
                            }
                        },
                        onCancelVoice = {
                            speechFence.cancel()
                            speechToken = null
                            speech.cancel()
                            status = "Voice cancelled"
                        },
                footer = if (language == "prolog") {
                    {
                        PrologConsole(
                            goal = prologGoal,
                            output = prologOutput,
                            busy = prologBusy,
                            tokens = tokens,
                            onGoal = { prologGoal = it },
                            onRun = ::runProlog,
                            onCancel = { cancelProlog() },
                        )
                    }
                } else {
                    null
                },
                    )
                    Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                        StatusPill("Ln $line : Col $column", tokens)
                        StatusPill(language, tokens)
                        if (dirty) StatusPill("modified", tokens, active = true)
                    }
                }
            }
        }
    }

    pendingPlan?.let { plan ->
        androidx.compose.material3.AlertDialog(
            onDismissRequest = { pendingPlan = null },
            title = { Text("Apply voice edit?") },
            text = { Text(plan.summary) },
            confirmButton = {
                TextButton(onClick = {
                    pendingPlan = null
                    applyPlan(plan)
                }) { Text("Apply", color = tokens.secondary) }
            },
            dismissButton = {
                TextButton(onClick = { pendingPlan = null }) {
                    Text("Reject", color = tokens.textMuted)
                }
            },
        )
    }
}

@Composable
private fun WorkbenchHeader(
    title: String,
    selected: CodeFileRef?,
    language: String,
    dirty: Boolean,
    status: String,
    tokens: ZaraSemanticTokens,
    onOpenProject: () -> Unit,
    onSave: () -> Unit,
    saveEnabled: Boolean,
) {
    Surface(
        color = tokens.surface,
        border = BorderStroke(1.dp, tokens.border),
        shape = MaterialTheme.shapes.large,
    ) {
        Row(
            modifier = Modifier.fillMaxWidth().padding(horizontal = 14.dp, vertical = 11.dp),
            horizontalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Column(modifier = Modifier.weight(1f)) {
                Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                    Text(
                        title,
                        color = tokens.text,
                        fontWeight = FontWeight.SemiBold,
                        style = MaterialTheme.typography.titleMedium,
                    )
                    StatusPill(language, tokens, active = language == "prolog")
                    if (dirty) StatusPill("modified", tokens, active = true)
                }
                Text(
                    selected?.relativePath ?: status,
                    color = tokens.textMuted,
                    fontFamily = FontFamily.Monospace,
                    style = MaterialTheme.typography.labelSmall,
                )
            }
            TextButton(onClick = onOpenProject) {
                Text(if (selected == null) "Open project" else "Project", color = tokens.secondary)
            }
            Button(
                enabled = saveEnabled,
                onClick = onSave,
                colors = ButtonDefaults.buttonColors(
                    containerColor = tokens.primary,
                    contentColor = tokens.background,
                    disabledContainerColor = tokens.surfaceElevated,
                    disabledContentColor = tokens.textMuted,
                ),
            ) {
                Text("Save")
            }
        }
    }
}

@Composable
private fun FileRail(
    files: List<CodeFileRef>,
    selected: CodeFileRef?,
    filter: String,
    tokens: ZaraSemanticTokens,
    modifier: Modifier,
    onFilter: (String) -> Unit,
    onOpen: (CodeFileRef) -> Unit,
    onRefresh: () -> Unit,
) {
    val visible = remember(files, filter) {
        if (filter.isBlank()) files else files.filter { it.relativePath.contains(filter, ignoreCase = true) }
    }
    Surface(
        modifier = modifier,
        color = tokens.surface,
        border = BorderStroke(1.dp, tokens.border),
        shape = MaterialTheme.shapes.large,
    ) {
        Column(
            modifier = Modifier.fillMaxSize().padding(10.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Text("FILES", color = tokens.accentCyan, style = MaterialTheme.typography.labelSmall)
            OutlinedTextField(
                value = filter,
                onValueChange = onFilter,
                modifier = Modifier.fillMaxWidth(),
                singleLine = true,
                label = { Text("Filter") },
                colors = fieldColors(tokens),
                textStyle = MaterialTheme.typography.bodySmall.copy(fontFamily = FontFamily.Monospace),
            )
            LazyColumn(modifier = Modifier.weight(1f)) {
                items(visible, key = { it.uri.toString() }) { file ->
                    val active = file == selected
                    Surface(
                        modifier = Modifier
                            .fillMaxWidth()
                            .clickable { onOpen(file) },
                        color = if (active) tokens.ambientGlow else tokens.surface,
                        border = BorderStroke(1.dp, if (active) tokens.borderActive else tokens.border),
                        shape = MaterialTheme.shapes.small,
                    ) {
                        Text(
                            file.relativePath,
                            modifier = Modifier.padding(horizontal = 9.dp, vertical = 8.dp),
                            color = if (active) tokens.text else tokens.textMuted,
                            fontFamily = FontFamily.Monospace,
                            style = MaterialTheme.typography.labelSmall,
                        )
                    }
                }
            }
            TextButton(onClick = onRefresh) {
                Text("Refresh · ${files.size}", color = tokens.secondary)
            }
        }
    }
}

@Composable
private fun CompactFileStrip(
    files: List<CodeFileRef>,
    selected: CodeFileRef?,
    tokens: ZaraSemanticTokens,
    onOpen: (CodeFileRef) -> Unit,
) {
    Row(
        modifier = Modifier.fillMaxWidth().horizontalScroll(rememberScrollState()),
        horizontalArrangement = Arrangement.spacedBy(6.dp),
    ) {
        files.forEach { file ->
            Surface(
                modifier = Modifier.clickable { onOpen(file) },
                color = if (file == selected) tokens.ambientGlow else tokens.surface,
                border = BorderStroke(1.dp, if (file == selected) tokens.borderActive else tokens.border),
                shape = MaterialTheme.shapes.medium,
            ) {
                Text(
                    file.relativePath,
                    modifier = Modifier.padding(horizontal = 10.dp, vertical = 7.dp),
                    color = if (file == selected) tokens.text else tokens.textMuted,
                    fontFamily = FontFamily.Monospace,
                    style = MaterialTheme.typography.labelSmall,
                )
            }
        }
    }
}

@Composable
private fun EditorPanel(
    value: TextFieldValue,
    selected: CodeFileRef?,
    findQuery: String,
    matches: Int,
    tokens: ZaraSemanticTokens,
    listening: Boolean,
    modifier: Modifier,
    onValue: (TextFieldValue) -> Unit,
    onFind: (String) -> Unit,
    onVoice: () -> Unit,
    onCancelVoice: () -> Unit,
    footer: (@Composable () -> Unit)? = null,
) {
    Surface(
        modifier = modifier,
        color = tokens.surface,
        border = BorderStroke(1.dp, tokens.border),
        shape = MaterialTheme.shapes.large,
    ) {
        Column(
            modifier = Modifier.fillMaxSize().padding(12.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                OutlinedTextField(
                    value = findQuery,
                    onValueChange = onFind,
                    modifier = Modifier.weight(1f),
                    singleLine = true,
                    label = { Text(if (findQuery.isBlank()) "Find in buffer" else "Find · $matches matches") },
                    colors = fieldColors(tokens),
                    textStyle = MaterialTheme.typography.bodySmall.copy(fontFamily = FontFamily.Monospace),
                )
                Button(
                    enabled = selected != null && !listening,
                    onClick = onVoice,
                    colors = ButtonDefaults.buttonColors(
                        containerColor = tokens.secondary,
                        contentColor = tokens.background,
                    ),
                ) {
                    Text(if (listening) "Listening…" else "Voice")
                }
                if (listening) {
                    TextButton(onClick = onCancelVoice) {
                        Text("Cancel", color = tokens.error)
                    }
                }
            }
            if (selected == null) {
                Surface(
                    modifier = Modifier.fillMaxSize(),
                    color = tokens.surfaceInput,
                    border = BorderStroke(1.dp, tokens.border),
                    shape = MaterialTheme.shapes.medium,
                ) {
                    Column(
                        modifier = Modifier.fillMaxSize().padding(24.dp),
                        verticalArrangement = Arrangement.Center,
                    ) {
                        Text("OPEN A PROJECT", color = tokens.accentMagenta)
                        Text(
                            "Pick a SAF workspace, choose a file, then edit with revision-fenced voice actions.",
                            color = tokens.textMuted,
                        )
                    }
                }
            } else {
                OutlinedTextField(
                    value = value,
                    onValueChange = onValue,
                    modifier = Modifier.weight(1f).fillMaxWidth().heightIn(min = 180.dp),
                    textStyle = MaterialTheme.typography.bodyMedium.copy(
                        color = tokens.text,
                        fontFamily = FontFamily.Monospace,
                    ),
                    label = { Text(selected.name) },
                    colors = fieldColors(tokens),
                )
                footer?.invoke()
            }
        }
    }
}

@Composable
private fun PrologConsole(
    goal: String,
    output: String,
    busy: Boolean,
    tokens: ZaraSemanticTokens,
    onGoal: (String) -> Unit,
    onRun: () -> Unit,
    onCancel: () -> Unit,
) {
    Surface(
        modifier = Modifier.fillMaxWidth(),
        color = tokens.surfaceInput,
        border = BorderStroke(1.dp, tokens.borderActive),
        shape = MaterialTheme.shapes.medium,
    ) {
        Column(
            modifier = Modifier.fillMaxWidth().padding(10.dp),
            verticalArrangement = Arrangement.spacedBy(8.dp),
        ) {
            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                Text(
                    "PROLOG QUERY",
                    modifier = Modifier.weight(1f),
                    color = tokens.accentMagenta,
                    fontFamily = FontFamily.Monospace,
                    style = MaterialTheme.typography.labelSmall,
                )
                StatusPill("ZARA-PROLOG/1", tokens, active = true)
            }
            OutlinedTextField(
                value = goal,
                onValueChange = onGoal,
                modifier = Modifier.fillMaxWidth(),
                enabled = !busy,
                singleLine = true,
                label = { Text("Goal") },
                colors = fieldColors(tokens),
                textStyle = MaterialTheme.typography.bodySmall.copy(fontFamily = FontFamily.Monospace),
            )
            Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
                Button(
                    enabled = !busy && goal.isNotBlank(),
                    onClick = onRun,
                    colors = ButtonDefaults.buttonColors(
                        containerColor = tokens.primary,
                        contentColor = tokens.background,
                    ),
                ) {
                    Text(if (busy) "Running…" else "Run")
                }
                if (busy) {
                    TextButton(onClick = onCancel) {
                        Text("Cancel", color = tokens.error)
                    }
                }
            }
            SelectionContainer {
                Text(
                    output,
                    modifier = Modifier.fillMaxWidth().heightIn(max = 120.dp),
                    color = tokens.textMuted,
                    fontFamily = FontFamily.Monospace,
                    style = MaterialTheme.typography.bodySmall,
                )
            }
        }
    }
}

@Composable
private fun Inspector(
    selected: CodeFileRef?,
    language: String,
    dirty: Boolean,
    line: Int,
    column: Int,
    revision: Long,
    selection: TextRange,
    tokens: ZaraSemanticTokens,
    modifier: Modifier,
) {
    Surface(
        modifier = modifier,
        color = tokens.surface,
        border = BorderStroke(1.dp, tokens.border),
        shape = MaterialTheme.shapes.large,
    ) {
        Column(
            modifier = Modifier.fillMaxSize().padding(12.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Text("BUFFER", color = tokens.accentCyan, style = MaterialTheme.typography.labelSmall)
            Metric("file", selected?.name ?: "none", tokens)
            Metric("language", language, tokens)
            Metric("line", line.toString(), tokens)
            Metric("column", column.toString(), tokens)
            Metric("revision", revision.toString(), tokens)
            Metric("selection", "${selection.start}..${selection.end}", tokens)
            Metric("state", if (dirty) "modified" else "saved", tokens)
            if (language == "prolog") {
                Spacer(Modifier.size(4.dp))
                Text("PROLOG", color = tokens.accentMagenta, style = MaterialTheme.typography.labelSmall)
                Text(
                    "Query, expert, KB and graph tooling lives in Zara's Logic workspace and canonical Trealla actor.",
                    color = tokens.textMuted,
                    style = MaterialTheme.typography.bodySmall,
                )
            }
        }
    }
}

@Composable
private fun Metric(label: String, value: String, tokens: ZaraSemanticTokens) {
    Row(modifier = Modifier.fillMaxWidth()) {
        Text(label, modifier = Modifier.weight(1f), color = tokens.textMuted, style = MaterialTheme.typography.bodySmall)
        SelectionContainer {
            Text(value, color = tokens.text, fontFamily = FontFamily.Monospace, style = MaterialTheme.typography.bodySmall)
        }
    }
}

@Composable
private fun StatusPill(
    label: String,
    tokens: ZaraSemanticTokens,
    active: Boolean = false,
) {
    Surface(
        color = if (active) tokens.ambientGlow else tokens.surfaceElevated,
        border = BorderStroke(1.dp, if (active) tokens.borderActive else tokens.border),
        shape = MaterialTheme.shapes.extraLarge,
    ) {
        Text(
            label.uppercase(),
            modifier = Modifier.padding(horizontal = 8.dp, vertical = 4.dp),
            color = if (active) tokens.text else tokens.textMuted,
            fontFamily = FontFamily.Monospace,
            style = MaterialTheme.typography.labelSmall,
        )
    }
}

@Composable
private fun fieldColors(tokens: ZaraSemanticTokens) = OutlinedTextFieldDefaults.colors(
    focusedTextColor = tokens.text,
    unfocusedTextColor = tokens.text,
    focusedBorderColor = tokens.borderActive,
    unfocusedBorderColor = tokens.border,
    focusedLabelColor = tokens.secondary,
    unfocusedLabelColor = tokens.textMuted,
    cursorColor = tokens.secondary,
    focusedContainerColor = tokens.surfaceInput,
    unfocusedContainerColor = tokens.surfaceInput,
)

fun syncEditorStateToBuffer(active: RevisionedEditorBuffer?, next: TextFieldValue) {
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

private fun lineColumn(text: String, offset: Int): Pair<Int, Int> {
    val safe = offset.coerceIn(0, text.length)
    val before = text.substring(0, safe)
    val line = before.count { it == '\n' } + 1
    val lastBreak = before.lastIndexOf('\n')
    val column = safe - lastBreak
    return line to column
}

private fun countMatches(text: String, query: String): Int {
    if (query.isBlank()) return 0
    var count = 0
    var start = 0
    while (start <= text.length - query.length) {
        val next = text.indexOf(query, start, ignoreCase = true)
        if (next < 0) break
        count += 1
        start = next + maxOf(1, query.length)
        if (count >= 500) break
    }
    return count
}
