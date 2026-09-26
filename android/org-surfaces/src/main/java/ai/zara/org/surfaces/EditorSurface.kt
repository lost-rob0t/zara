package ai.zara.org.surfaces

import ai.zara.org.core.OrgAppPolicy
import ai.zara.org.core.OrgPageBlock
import ai.zara.org.core.OrgPageParser
import ai.zara.org.storage.OrgRepository
import androidx.compose.foundation.clickable
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.Spacer
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.lazy.LazyColumn
import androidx.compose.foundation.lazy.items
import androidx.compose.material3.HorizontalDivider
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.text.font.FontWeight
import androidx.compose.ui.unit.dp

internal enum class EditorMode { PAGE, RAW, CONFIG }

@Composable
fun EditorSurface(model: OrgWorkspaceModel) {
    val repository = model.repository ?: return
    var session by remember { mutableStateOf<OrgEditorSession?>(null) }
    var files by remember { mutableStateOf(repository.listOrgFiles()) }

    val selected = session
    if (selected == null) {
        LazyColumn(
            modifier = Modifier.fillMaxSize(),
            verticalArrangement = Arrangement.spacedBy(4.dp),
        ) {
            item("pages") {
                Text("Pages", style = MaterialTheme.typography.headlineMedium, fontWeight = FontWeight.SemiBold)
                OrgMutedText("${files.size} ordinary Org files · source remains canonical")
                Spacer(Modifier.width(1.dp))
            }
            items(files, key = { it.relativePath }) { file ->
                Column(
                    modifier = Modifier
                        .fillMaxWidth()
                        .clickable {
                            runCatching { OrgEditorSessions.open(file.relativePath, repository.read(file)) }
                                .onSuccess { session = it }
                                .onFailure { failure ->
                                    model.report(failure.message ?: "Unable to open ${file.relativePath}")
                                }
                        }
                        .padding(vertical = 14.dp),
                ) {
                    Text(file.name, style = MaterialTheme.typography.titleMedium)
                    OrgMutedText(file.relativePath)
                }
                HorizontalDivider(color = LocalOrgTokens.current.border)
            }
        }
        return
    }

    EditorWorkspace(
        session = selected,
        repository = repository,
        onSession = { session = it },
        onClose = {
            session = null
            files = repository.listOrgFiles()
        },
        report = model::report,
    )
}

@Composable
private fun EditorWorkspace(
    session: OrgEditorSession,
    repository: OrgRepository,
    onSession: (OrgEditorSession) -> Unit,
    onClose: () -> Unit,
    report: (String) -> Unit,
) {
    var mode by remember(session.path) { mutableStateOf(EditorMode.PAGE) }
    var configDraft by remember(session.path) {
        mutableStateOf(repository.readRelative("config.pl") ?: OrgAppPolicy.defaultSource)
    }

    Column(modifier = Modifier.fillMaxSize()) {
        Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween,
        ) {
            TextButton(onClick = onClose) { Text("Pages") }
            Row {
                TextButton(
                    enabled = session.dirty,
                    onClick = {
                        val file = repository.listOrgFiles().firstOrNull { it.relativePath == session.path }
                            ?: return@TextButton report("Missing ${session.path}")
                        runCatching { repository.read(file) }
                            .onSuccess { diskText ->
                                val stale = OrgEditorSessions.staleReason(session, diskText)
                                if (stale != null) {
                                    report(stale)
                                } else {
                                    runCatching { repository.write(file, session.draft) }
                                        .onSuccess {
                                            onSession(OrgEditorSessions.settle(session, session.draft))
                                            report("Saved ${session.path}")
                                        }
                                        .onFailure { report(it.message ?: "Save failed") }
                                }
                            }
                            .onFailure { report(it.message ?: "Unable to check ${session.path}") }
                    },
                ) { Text(if (session.dirty) "Save" else "Saved") }
            }
        }

        Text(
            session.path.substringAfterLast('/').removeSuffix(".org"),
            style = MaterialTheme.typography.headlineMedium,
            fontWeight = FontWeight.SemiBold,
        )
        OrgMutedText(session.path + if (session.dirty) " · unsaved" else " · canonical Org")

        Row(horizontalArrangement = Arrangement.spacedBy(4.dp)) {
            EditorMode.entries.forEach { candidate ->
                TextButton(onClick = { mode = candidate }) {
                    Text(
                        when (candidate) {
                            EditorMode.PAGE -> "Page"
                            EditorMode.RAW -> "Raw"
                            EditorMode.CONFIG -> "config.pl"
                        },
                        color = if (mode == candidate) {
                            MaterialTheme.colorScheme.secondary
                        } else {
                            MaterialTheme.colorScheme.onSurfaceVariant
                        },
                    )
                }
            }
        }
        HorizontalDivider(color = LocalOrgTokens.current.border)

        when (mode) {
            EditorMode.PAGE -> BlockPage(
                session = session,
                onSession = onSession,
                modifier = Modifier
                    .fillMaxSize()
                    .padding(top = 8.dp),
            )
            EditorMode.RAW -> OutlinedTextField(
                value = session.draft,
                onValueChange = { onSession(session.withDraft(it)) },
                modifier = Modifier
                    .fillMaxSize()
                    .padding(top = 8.dp),
                label = { Text("Raw Org markup") },
                textStyle = MaterialTheme.typography.bodyMedium,
            )
            EditorMode.CONFIG -> Column(
                modifier = Modifier
                    .fillMaxSize()
                    .padding(top = 8.dp),
            ) {
                OrgMutedText("Typed app policy facts. Effects still require Zara capability approval.")
                OutlinedTextField(
                    value = configDraft,
                    onValueChange = { configDraft = it },
                    modifier = Modifier
                        .weight(1f)
                        .fillMaxWidth(),
                    label = { Text("config.pl") },
                    textStyle = MaterialTheme.typography.bodyMedium,
                )
                TextButton(onClick = {
                    runCatching {
                        OrgAppPolicy.parse(configDraft)
                        repository.writeRelative("config.pl", configDraft)
                    }
                        .onSuccess { report("Saved validated config.pl") }
                        .onFailure { report(it.message ?: "config.pl is invalid") }
                }) { Text("Validate and save policy") }
            }
        }
    }
}

@Composable
private fun BlockPage(
    session: OrgEditorSession,
    onSession: (OrgEditorSession) -> Unit,
    modifier: Modifier = Modifier,
) {
    val document = remember(session.draft) { OrgPageParser.parse(session.draft) }
    var activeStart by remember(session.path) { mutableStateOf<Int?>(null) }
    var activeDraft by remember(session.path) { mutableStateOf("") }

    LazyColumn(modifier = modifier) {
        document.title?.let { title ->
            item("document-title") {
                Text(title, style = MaterialTheme.typography.headlineLarge, fontWeight = FontWeight.SemiBold)
                Spacer(Modifier.width(1.dp))
            }
        }
        items(document.blocks, key = { it.start }) { block ->
            if (activeStart == block.start) {
                Column(modifier = Modifier.padding(vertical = 6.dp)) {
                    OutlinedTextField(
                        value = activeDraft,
                        onValueChange = { activeDraft = it },
                        modifier = Modifier.fillMaxWidth(),
                        label = { Text("Editing block") },
                        minLines = 3,
                    )
                    Row {
                        TextButton(onClick = {
                            onSession(OrgEditorSessions.replaceBlock(session, block, activeDraft))
                            activeStart = null
                        }) { Text("Done") }
                        TextButton(onClick = { activeStart = null }) { Text("Cancel") }
                    }
                }
            } else {
                RenderedBlock(
                    block = block,
                    onEdit = {
                        activeStart = block.start
                        activeDraft = block.raw
                    },
                )
            }
        }
    }
}

@Composable
private fun RenderedBlock(block: OrgPageBlock, onEdit: () -> Unit) {
    Row(
        modifier = Modifier
            .fillMaxWidth()
            .clickable(onClick = onEdit)
            .padding(start = ((block.depth - 1) * 18).dp, top = 10.dp, bottom = 10.dp),
    ) {
        Text("•", color = LocalOrgTokens.current.accentCyan, style = MaterialTheme.typography.titleLarge)
        Column(modifier = Modifier.padding(start = 10.dp)) {
            Text(block.heading, style = MaterialTheme.typography.titleMedium)
            renderedBody(block.body).takeIf { it.isNotBlank() }?.let { body ->
                Text(
                    body,
                    color = MaterialTheme.colorScheme.onSurfaceVariant,
                    style = MaterialTheme.typography.bodyMedium,
                )
            }
        }
    }
}

private fun renderedBody(source: String): String = source
    .lineSequence()
    .filterNot { line ->
        val trimmed = line.trim()
        trimmed.startsWith(":") || trimmed.startsWith("#+")
    }
    .joinToString("\n")
    .trim()
