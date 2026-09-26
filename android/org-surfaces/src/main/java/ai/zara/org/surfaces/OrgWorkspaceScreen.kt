package ai.zara.org.surfaces

import ai.zara.org.storage.OrgHome
import ai.zara.org.storage.OrgHomeSelection
import ai.zara.org.storage.OrgRepository
import android.content.Context
import androidx.compose.foundation.background
import androidx.compose.foundation.horizontalScroll
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Box
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.IntrinsicSize
import androidx.compose.foundation.layout.WindowInsets
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.height
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.layout.safeDrawing
import androidx.compose.foundation.layout.width
import androidx.compose.foundation.layout.windowInsetsPadding
import androidx.compose.material3.Button
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Surface
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.LaunchedEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableIntStateOf
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import ai.zara.org.core.OrgTask
import ai.zara.org.core.OrgWorkspaceProjection
import ai.zara.org.core.OrgWorkspaceProjector

class OrgWorkspaceModel(
    val context: Context,
    val home: OrgHomeSelection,
    val repository: OrgRepository?,
    val pickDirectory: () -> Unit,
    val useShared: () -> Unit,
) {
    var projection by mutableStateOf(OrgWorkspaceProjector.project(emptyMap()))
        private set
    var status by mutableStateOf("")
        private set

    fun refresh() {
        val repo = repository ?: return
        runCatching {
            val files = repo.listOrgFiles()
            val documents = files.associate { it.relativePath to repo.read(it) }
            projection = OrgWorkspaceProjector.project(documents, OrgHome.dailySpec(context))
            status = "${files.size} files · ${projection.tasks.size} tasks · ${projection.roam.nodes.size} nodes"
        }.onFailure { status = it.message ?: "Unable to project Org workspace" }
    }

    fun cycle(task: OrgTask) {
        val repo = repository ?: return
        runCatching { repo.cycleTodo(task) }
            .onSuccess { refresh() }
            .onFailure { status = it.message ?: "TODO update failed" }
    }

    fun report(message: String) {
        status = message
    }
}

@Composable
fun rememberOrgWorkspaceModel(): OrgWorkspaceModel {
    val context = LocalContext.current
    var revision by rememberSaveable { mutableIntStateOf(0) }
    val home = remember(revision) { OrgHome.selection(context) }
    val repository = remember(revision) { runCatching { OrgHome.open(context) }.getOrNull() }
    val picker = androidx.activity.compose.rememberLauncherForActivityResult(
        androidx.activity.result.contract.ActivityResultContracts.OpenDocumentTree(),
    ) { uri ->
        if (uri != null) {
            runCatching { OrgHome.useCustomSaf(context, uri) }
                .onSuccess { revision += 1 }
                .onFailure { it.message?.let { message -> android.util.Log.e("OrgWorkspace", message) } }
        }
    }
    val model = remember(revision) {
        OrgWorkspaceModel(
            context = context,
            home = home,
            repository = repository,
            pickDirectory = { picker.launch(home.customTreeUri) },
            useShared = {
                OrgHome.useShared(context)
                revision += 1
            },
        )
    }
    LaunchedEffect(repository) { model.refresh() }
    return model
}

data class OrgSurfaceTab(
    val name: String,
    val content: @Composable (OrgWorkspaceModel) -> Unit,
)

@Composable
fun OrgWorkspaceScreen(title: String, tabs: List<OrgSurfaceTab>) {
    val model = rememberOrgWorkspaceModel()
    var selected by rememberSaveable { mutableIntStateOf(0) }
    val activeTab = tabs[selected.coerceAtLeast(0).coerceAtMost(tabs.lastIndex)]
    val tokens = LocalOrgTokens.current
    val connected = model.repository != null

    Surface(
        modifier = Modifier
            .fillMaxSize(),
        color = MaterialTheme.colorScheme.background,
        contentColor = MaterialTheme.colorScheme.onBackground,
    ) {
        Column(
            modifier = Modifier
                .fillMaxSize()
                .windowInsetsPadding(WindowInsets.safeDrawing)
                .padding(14.dp),
            verticalArrangement = Arrangement.spacedBy(10.dp),
        ) {
            Row {
                OrgStatusDot(color = if (connected) tokens.success else tokens.warning)
                Text(
                    title,
                    modifier = Modifier.padding(start = 8.dp),
                    style = MaterialTheme.typography.headlineSmall,
                )
            }
            OrgMutedText(
                model.status.ifBlank {
                    when (model.home.mode) {
                        ai.zara.org.storage.OrgHomeMode.SHARED -> "Shared canonical Org workspace"
                        ai.zara.org.storage.OrgHomeMode.CUSTOM_SAF -> "Custom canonical Org workspace"
                    }
                },
            )

            Row {
                if (tabs.size > 1) {
                    Row(
                        modifier = Modifier
                            .weight(1f)
                            .horizontalScroll(rememberScrollState()),
                        horizontalArrangement = Arrangement.spacedBy(4.dp),
                    ) {
                        tabs.forEachIndexed { index, tab ->
                            val active = index == selected
                            Column(modifier = Modifier.width(IntrinsicSize.Max)) {
                                TextButton(onClick = { selected = index }) {
                                    Text(
                                        tab.name,
                                        color = if (active) {
                                            MaterialTheme.colorScheme.secondary
                                        } else {
                                            MaterialTheme.colorScheme.onSurfaceVariant
                                        },
                                    )
                                }
                                Box(
                                    modifier = Modifier
                                        .fillMaxWidth()
                                        .height(1.dp)
                                        .background(if (active) tokens.borderActive else Color.Transparent),
                                )
                            }
                        }
                    }
                }
                TextButton(onClick = { model.refresh() }, enabled = connected) {
                    Text("Refresh")
                }
            }

            if (model.repository == null) {
                OrgWorkspaceUnavailable(
                    shared = model.home.mode == ai.zara.org.storage.OrgHomeMode.SHARED,
                    onChooseDirectory = model.pickDirectory,
                    onUseShared = model.useShared,
                )
            } else {
                activeTab.content(model)
            }
        }
    }
}

@Composable
private fun OrgWorkspaceUnavailable(
    shared: Boolean,
    onChooseDirectory: () -> Unit,
    onUseShared: () -> Unit,
) {
    Column(verticalArrangement = Arrangement.spacedBy(10.dp)) {
        Text(
            if (shared) {
                "Shared Org workspace is unavailable. Choose a directory or install/connect the canonical Org Sync provider."
            } else {
                "The selected Org directory is unavailable. Re-grant it or return to the shared workspace."
            },
        )
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            Button(onClick = onChooseDirectory) {
                Text(if (shared) "Choose Org directory" else "Re-grant directory")
            }
            if (!shared) TextButton(onClick = onUseShared) { Text("Use shared workspace") }
        }
        Text(
            "Ordinary Org files remain canonical. Surfaces are derived projections; this app owns no shadow note/task database.",
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )
    }
}
