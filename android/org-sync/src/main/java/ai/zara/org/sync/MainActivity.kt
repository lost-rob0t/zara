package ai.zara.org.sync

import ai.zara.org.sync.core.GitOrgWorkspace
import ai.zara.org.sync.core.GitSyncResult
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
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
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.unit.dp

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
    val root = remember { context.filesDir.resolve("org-workspaces/main") }
    val workspace = remember { GitOrgWorkspace(root) }
    val prefs = remember { context.getSharedPreferences("org-sync", MODE_PRIVATE) }

    var remote by rememberSaveable { mutableStateOf(prefs.getString("remote", "") ?: "") }
    var branch by rememberSaveable { mutableStateOf(prefs.getString("branch", "main") ?: "main") }
    var status by remember { mutableStateOf(describe(workspace)) }

    fun persist() {
        prefs.edit().putString("remote", remote.trim()).putString("branch", branch.trim()).apply()
    }

    fun refresh() {
        status = describe(workspace)
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
            Button(onClick = {
                runCatching {
                    persist()
                    workspace.initialize()
                    if (remote.isNotBlank()) workspace.configureRemote(remote.trim(), branch.trim())
                }.onSuccess { refresh() }
                    .onFailure { status = it.message ?: "Git init failed" }
            }) {
                Text("Initialize")
            }

            Button(
                enabled = remote.isNotBlank() && branch.isNotBlank(),
                onClick = {
                    runCatching {
                        persist()
                        workspace.clone(remote.trim(), branch.trim())
                    }.onSuccess { refresh() }
                        .onFailure { status = it.message ?: "Clone failed" }
                },
            ) {
                Text("Clone")
            }

            Button(onClick = {
                persist()
                status = when (val result = workspace.sync()) {
                    is GitSyncResult.Synced -> result.message
                    is GitSyncResult.Conflict -> {
                        val files = result.files.take(5).joinToString()
                        if (files.isBlank()) result.message else "${result.message}: $files"
                    }
                    is GitSyncResult.Failed -> result.message
                }
            }) {
                Text("Sync")
            }
        }

        TextButton(onClick = { refresh() }) {
            Text("Refresh status")
        }

        Text(status, color = MaterialTheme.colorScheme.secondary)
        Text(
            "Shared home is exposed only to apps signed with the same Zara signing certificate. " +
                "Individual Org apps can switch to their own custom SAF directory.",
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
