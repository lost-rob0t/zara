package ai.zara.org.sync

import ai.zara.org.sync.core.GitOrgWorkspace
import ai.zara.org.sync.core.GitSyncResult
import ai.zara.org.sync.core.OrgWorkspaceDescriptor
import ai.zara.org.sync.core.OrgWorkspaceMapper
import ai.zara.org.sync.core.SyncGenerationFence
import ai.zara.org.sync.core.SyncLease
import ai.zara.org.sync.core.WorkspaceId
import android.os.Bundle
import android.os.Handler
import android.os.Looper
import androidx.activity.ComponentActivity
import androidx.activity.compose.rememberLauncherForActivityResult
import androidx.activity.compose.setContent
import androidx.activity.result.contract.ActivityResultContracts
import androidx.compose.foundation.background
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxSize
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.Button
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.OutlinedTextField
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.material3.darkColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.DisposableEffect
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp
import java.util.concurrent.Executors

private val SyncScheme = darkColorScheme(
    primary = Color(0xFFFF4FD8),
    secondary = Color(0xFF45E6FF),
    background = Color(0xFF050510),
    surface = Color(0xFF0B0B1D),
    onPrimary = Color.Black,
    onSecondary = Color.Black,
    onBackground = Color(0xFFF4EEFF),
    onSurface = Color(0xFFF4EEFF),
)

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            MaterialTheme(colorScheme = SyncScheme) {
                OrgSyncApp()
            }
        }
    }
}

@Composable
private fun OrgSyncApp() {
    val context = LocalContext.current
    val prefs = remember { context.getSharedPreferences("org-sync", MODE_PRIVATE) }
    val initialSelection = remember { runCatching { SharedWorkspaceStore.load(context) }.getOrNull() }
    val initialRootId = remember(initialSelection) {
        when (initialSelection) {
            is SharedWorkspaceSelection.AppPrivate -> initialSelection.descriptor.rootId
            is SharedWorkspaceSelection.Git -> initialSelection.descriptor.localRootId
            else -> "main"
        }
    }
    var activeRootId by rememberSaveable { mutableStateOf(initialRootId) }
    var rootIdInput by rememberSaveable { mutableStateOf(activeRootId) }
    var remote by rememberSaveable {
        mutableStateOf(
            (initialSelection as? SharedWorkspaceSelection.Git)?.descriptor?.remote
                ?: prefs.getString("remote", "").orEmpty(),
        )
    }
    var branch by rememberSaveable {
        mutableStateOf(
            (initialSelection as? SharedWorkspaceSelection.Git)?.descriptor?.branch
                ?: prefs.getString("branch", "main").orEmpty(),
        )
    }

    val root = remember(activeRootId) {
        val descriptor = OrgWorkspaceDescriptor.AppPrivate(
            id = WorkspaceId("shared"),
            displayName = "Shared Org",
            rootId = activeRootId,
        )
        OrgWorkspaceMapper.appPrivateRoot(context.filesDir, descriptor)
    }
    val workspace = remember(root) { GitOrgWorkspace(root) }
    val syncFence = remember { SyncGenerationFence() }
    val worker = remember { Executors.newSingleThreadExecutor() }
    val mainHandler = remember { Handler(Looper.getMainLooper()) }
    var activeLease by remember { mutableStateOf<SyncLease?>(null) }
    var status by remember { mutableStateOf(describe(workspace)) }

    fun cancelActive(message: String = "Sync cancelled") {
        val lease = activeLease ?: return
        syncFence.cancel(lease)
        activeLease = null
        status = message
    }

    fun selectGitWorkspace() {
        SharedWorkspaceStore.useGit(
            context = context,
            localRootId = rootIdInput,
            remote = remote.trim(),
            branch = branch.trim(),
        )
        activeRootId = rootIdInput
    }

    fun launchGitOperation(label: String, action: (SyncLease) -> String) {
        cancelActive("Previous sync cancelled")
        val lease = syncFence.begin()
        activeLease = lease
        status = label
        val selectedWorkspace = workspace
        worker.execute {
            val result = runCatching { action(lease) }
            mainHandler.post {
                if (activeLease?.generation != lease.generation) return@post
                activeLease = null
                if (!lease.isCurrent()) return@post
                status = result.fold(
                    onSuccess = { it },
                    onFailure = { it.message ?: "$label failed" },
                )
            }
        }
    }

    val safPicker = rememberLauncherForActivityResult(ActivityResultContracts.OpenDocumentTree()) { uri ->
        if (uri == null) return@rememberLauncherForActivityResult
        cancelActive("Sync cancelled for workspace change")
        runCatching { SharedWorkspaceStore.useSaf(context, uri) }
            .onSuccess { status = "SAF workspace selected · $uri" }
            .onFailure { status = it.message ?: "Unable to persist SAF workspace" }
    }

    DisposableEffect(Unit) {
        onDispose {
            activeLease?.let(syncFence::cancel)
            worker.shutdownNow()
        }
    }

    Column(
        modifier = Modifier
            .fillMaxSize()
            .background(MaterialTheme.colorScheme.background)
            .padding(16.dp),
        verticalArrangement = Arrangement.spacedBy(12.dp),
    ) {
        Text("Org Sync", style = MaterialTheme.typography.headlineSmall)
        Text(
            "Owns the default shared Org home used by Org, Todo, Notebook and other first-party Org APKs.",
            color = MaterialTheme.colorScheme.onSurfaceVariant,
        )

        OutlinedTextField(
            value = rootIdInput,
            onValueChange = { rootIdInput = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Shared workspace root") },
            supportingText = {
                Text("Logical Zara-owned root. External document trees use persisted SAF selection below.")
            },
            singleLine = true,
        )
        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            Button(onClick = {
                cancelActive("Sync cancelled for workspace change")
                runCatching { SharedWorkspaceStore.useAppPrivate(context, rootIdInput) }
                    .onSuccess {
                        activeRootId = rootIdInput
                        status = "Local workspace selected · $rootIdInput"
                    }
                    .onFailure { status = it.message ?: "Invalid workspace root" }
            }) {
                Text("Use local")
            }
            Button(onClick = { safPicker.launch(null) }) {
                Text("Choose SAF")
            }
        }
        Text(root.absolutePath, style = MaterialTheme.typography.labelSmall)

        OutlinedTextField(
            value = remote,
            onValueChange = { remote = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Git remote") },
            supportingText = { Text("Do not embed passwords or tokens in URLs.") },
            singleLine = true,
        )
        OutlinedTextField(
            value = branch,
            onValueChange = { branch = it },
            modifier = Modifier.fillMaxWidth(),
            label = { Text("Branch") },
            singleLine = true,
        )

        Row(horizontalArrangement = Arrangement.spacedBy(8.dp)) {
            Button(
                enabled = activeLease == null && remote.isNotBlank() && branch.isNotBlank(),
                onClick = {
                    runCatching {
                        selectGitWorkspace()
                        workspace.initialize()
                        workspace.configureRemote(remote.trim(), branch.trim())
                    }.onSuccess {
                        status = describe(workspace)
                    }.onFailure {
                        status = it.message ?: "Git init failed"
                    }
                },
            ) {
                Text("Initialize")
            }

            Button(
                enabled = activeLease == null && remote.isNotBlank() && branch.isNotBlank(),
                onClick = {
                    runCatching { selectGitWorkspace() }
                        .onSuccess {
                            launchGitOperation("Cloning Git workspace…") { lease ->
                                workspace.clone(remote.trim(), branch.trim(), lease)
                                describe(workspace)
                            }
                        }
                        .onFailure { status = it.message ?: "Invalid Git workspace" }
                },
            ) {
                Text("Clone")
            }

            Button(
                enabled = activeLease == null && remote.isNotBlank() && branch.isNotBlank(),
                onClick = {
                    runCatching { selectGitWorkspace() }
                        .onSuccess {
                            launchGitOperation("Syncing Git workspace…") { lease ->
                                when (val result = workspace.sync(lease)) {
                                    is GitSyncResult.Synced -> result.message
                                    is GitSyncResult.Conflict -> {
                                        val files = result.files.take(5).joinToString()
                                        if (files.isBlank()) result.message else "${result.message}: $files"
                                    }
                                    is GitSyncResult.Cancelled -> result.message
                                    is GitSyncResult.Failed -> result.message
                                }
                            }
                        }
                        .onFailure { status = it.message ?: "Invalid Git workspace" }
                },
            ) {
                Text("Sync")
            }

            if (activeLease != null) {
                Button(onClick = { cancelActive() }) {
                    Text("Cancel")
                }
            }
        }

        TextButton(onClick = { status = describe(workspace) }) {
            Text("Refresh status")
        }

        Text(status, color = MaterialTheme.colorScheme.secondary)
        Text(
            "Shared home is exposed only to apps signed with the same Zara signing certificate. " +
                "Git and local workspaces use the configured logical root; SAF grants stay user-selected and revocable.",
            style = MaterialTheme.typography.bodySmall,
        )
    }
}

private fun describe(workspace: GitOrgWorkspace): String {
    val state = workspace.status()
    if (!state.initialized) return "Shared Org home ready · Git not initialized"
    if (state.conflicting.isNotEmpty()) {
        return "Git conflicts: ${state.conflicting.joinToString()}"
    }
    val dirty = state.added.size + state.changed.size + state.removed.size
    return buildString {
        append("Git ")
        append(state.branch ?: "detached")
        append(if (state.clean) " · clean" else " · $dirty changed")
    }
}
