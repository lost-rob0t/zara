package ai.zara.app.ui

import ai.zara.ui.org.OrgTextRenderer
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.text.selection.SelectionContainer
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.Text
import androidx.compose.material3.TextButton
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.mutableStateOf
import androidx.compose.runtime.remember
import androidx.compose.runtime.saveable.rememberSaveable
import androidx.compose.runtime.setValue
import androidx.compose.ui.Modifier
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp

private val preferredRepoOrgHelp = listOf(
    "README.org",
    "docs/README.org",
    "wiki/android.org",
    "wiki/customization.org",
    "wiki/agent-mode.org",
)

internal fun selectRepoOrgHelpPaths(indexText: String, limit: Int = 12): List<String> {
    val available = indexText.lineSequence()
        .map(String::trim)
        .filter { it.endsWith(".org") }
        .distinct()
        .toList()
    val preferred = preferredRepoOrgHelp.filter(available::contains)
    return (preferred + available.filterNot(preferred::contains)).distinct().take(limit)
}

@Composable
internal fun RepoOrgHelpPanel(modifier: Modifier = Modifier) {
    val context = LocalContext.current
    val paths = remember {
        runCatching {
            context.assets.open("help/index.txt").bufferedReader().use { reader ->
                selectRepoOrgHelpPaths(reader.readText())
            }
        }.getOrDefault(emptyList())
    }
    var selected by rememberSaveable(paths) {
        mutableStateOf(paths.firstOrNull() ?: "")
    }
    val source = remember(selected) {
        if (selected.isBlank()) {
            "* Help unavailable\nNo packaged Org help sources were found."
        } else {
            runCatching {
                context.assets.open("help/$selected").bufferedReader().use { it.readText() }
            }.getOrElse { error ->
                "* Help source unavailable\n$selected\n\n${error.message.orEmpty()}"
            }
        }
    }
    val rendered = remember(source) { OrgTextRenderer.renderSource(source, baseFontSp = 15f) }
    val tokens = LocalZaraTokens.current

    Column(
        modifier = modifier.fillMaxWidth(),
        verticalArrangement = Arrangement.spacedBy(8.dp),
    ) {
        if (paths.isNotEmpty()) {
            paths.chunked(3).forEach { row ->
                Row(modifier = Modifier.fillMaxWidth()) {
                    row.forEach { path ->
                        TextButton(
                            modifier = Modifier.weight(1f),
                            onClick = { selected = path },
                        ) {
                            Text(
                                path.substringAfterLast('/'),
                                color = if (selected == path) tokens.accentCyan else tokens.textMuted,
                                style = MaterialTheme.typography.labelSmall,
                            )
                        }
                    }
                }
            }
        }
        Text(
            selected.ifBlank { "Org help" },
            color = tokens.textMuted,
            fontFamily = FontFamily.Monospace,
            style = MaterialTheme.typography.labelSmall,
        )
        SelectionContainer {
            Text(
                text = rendered.annotated,
                modifier = Modifier.fillMaxWidth().padding(bottom = 8.dp),
                color = tokens.text,
                fontFamily = FontFamily.Monospace,
                style = MaterialTheme.typography.bodyMedium,
            )
        }
    }
}
