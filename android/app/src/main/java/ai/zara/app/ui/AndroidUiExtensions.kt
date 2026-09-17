package ai.zara.app.ui

import ai.zara.app.ZaraApplication
import ai.zara.app.ui.extensions.AndroidUiExtensionRepository
import ai.zara.app.ui.extensions.UiContribution
import ai.zara.app.ui.extensions.UiContributionKind
import ai.zara.app.ui.extensions.UiPlatform
import ai.zara.app.ui.extensions.UiSlot
import androidx.compose.foundation.layout.Arrangement
import androidx.compose.foundation.layout.Column
import androidx.compose.foundation.layout.PaddingValues
import androidx.compose.foundation.layout.Row
import androidx.compose.foundation.layout.fillMaxWidth
import androidx.compose.foundation.layout.padding
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.NavigationDrawerItem
import androidx.compose.material3.NavigationDrawerItemDefaults
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.getValue
import androidx.compose.runtime.produceState
import androidx.compose.runtime.remember
import androidx.compose.ui.Alignment
import androidx.compose.ui.Modifier
import androidx.compose.ui.graphics.Color
import androidx.compose.ui.platform.LocalContext
import androidx.compose.ui.text.font.FontFamily
import androidx.compose.ui.unit.dp
import androidx.compose.ui.unit.sp
import java.io.File
import java.util.concurrent.TimeUnit
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.withContext

internal fun UiContribution.isUiActionEnabled(): Boolean =
    !action.startsWith("plugin:")

@Composable
internal fun rememberAndroidUiContributions(localGeneration: Long): List<UiContribution> {
    val context = LocalContext.current.applicationContext
    val application = context as ZaraApplication
    val repository = remember(context) {
        AndroidUiExtensionRepository(
            File(context.filesDir, "ui-config"),
            application.appSession::queryLocalProlog,
        )
    }
    val contributions by produceState<List<UiContribution>>(
        initialValue = emptyList(),
        key1 = localGeneration,
        key2 = repository,
    ) {
        value = runCatching {
            withContext(Dispatchers.IO) {
                repository.load().get(3, TimeUnit.SECONDS)
            }
        }.getOrDefault(emptyList())
    }
    return contributions
}

@Composable
internal fun AndroidUiExtensionSlot(
    contributions: List<UiContribution>,
    slot: UiSlot,
    onAction: (String) -> Unit,
) {
    contributions
        .asSequence()
        .filter { UiPlatform.ANDROID in it.platforms && it.slot == slot }
        .forEach { contribution ->
            UiContributionControl(contribution, onAction)
        }
}

@Composable
internal fun AndroidDrawerUiExtensions(
    contributions: List<UiContribution>,
    onAction: (String) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    val items = contributions.filter {
        UiPlatform.ANDROID in it.platforms && it.slot == UiSlot.DRAWER
    }
    if (items.isEmpty()) return

    Text(
        "EXTENSIONS",
        modifier = Modifier.padding(horizontal = 10.dp, vertical = 4.dp),
        color = tokens.accentCyan,
        fontFamily = FontFamily.Monospace,
        fontSize = 10.sp,
        letterSpacing = 1.8.sp,
    )
    items.forEach { contribution ->
        when (contribution.kind) {
            UiContributionKind.SURFACE, UiContributionKind.BUTTON -> NavigationDrawerItem(
                label = { Text(contribution.label) },
                selected = false,
                enabled = contribution.isUiActionEnabled(),
                onClick = { onAction(contribution.action) },
                colors = NavigationDrawerItemDefaults.colors(
                    selectedContainerColor = tokens.ambientGlow,
                    unselectedContainerColor = Color.Transparent,
                    selectedTextColor = tokens.text,
                    unselectedTextColor = tokens.textMuted,
                ),
            )
            UiContributionKind.SECTION -> Text(
                contribution.label.uppercase(),
                modifier = Modifier.padding(horizontal = 10.dp, vertical = 6.dp),
                color = tokens.accentCyan,
                style = MaterialTheme.typography.labelSmall,
            )
            UiContributionKind.TEXT, UiContributionKind.STATUS -> MutedNotice(contribution.label)
            UiContributionKind.TOGGLE -> UiContributionControl(contribution, onAction)
        }
    }
}

@Composable
internal fun PluginExtensionsSurface(
    contributions: List<UiContribution>,
    onAction: (String) -> Unit,
    padding: PaddingValues,
) {
    val pluginItems = contributions.filter {
        UiPlatform.ANDROID in it.platforms && it.owner.startsWith("plugin:")
    }
    ScreenBody(padding) {
        ScreenTitle("Plugins", "Trusted plugin UI extensions")
        if (pluginItems.isEmpty()) {
            SectionCard("NO UI EXTENSIONS") {
                MutedNotice(
                    "Trusted and enabled Android plugins can contribute bounded native UI through the ZARA-ANDROID-PLUGIN/1 host. Plugin code is not loaded into Zara's UI process."
                )
            }
        } else {
            pluginItems.groupBy { it.owner.removePrefix("plugin:") }.forEach { (owner, items) ->
                SectionCard(owner.uppercase()) {
                    items.forEach { contribution ->
                        UiContributionControl(contribution, onAction)
                    }
                }
            }
        }
    }
}

@Composable
private fun UiContributionControl(
    contribution: UiContribution,
    onAction: (String) -> Unit,
) {
    val tokens = LocalZaraTokens.current
    when (contribution.kind) {
        UiContributionKind.SECTION -> Text(
            contribution.label.uppercase(),
            color = tokens.accentCyan,
            fontFamily = FontFamily.Monospace,
            style = MaterialTheme.typography.labelSmall,
        )
        UiContributionKind.TEXT -> Text(
            contribution.label,
            color = tokens.text,
            style = MaterialTheme.typography.bodyMedium,
        )
        UiContributionKind.STATUS -> MutedNotice(contribution.label)
        UiContributionKind.BUTTON, UiContributionKind.SURFACE ->
            SecondaryAction(
                contribution.label,
                enabled = contribution.isUiActionEnabled(),
            ) {
                onAction(contribution.action)
            }
        UiContributionKind.TOGGLE -> Row(
            modifier = Modifier.fillMaxWidth(),
            horizontalArrangement = Arrangement.SpaceBetween,
            verticalAlignment = Alignment.CenterVertically,
        ) {
            Text(contribution.label, color = tokens.text)
            Text(
                "STATE UNAVAILABLE",
                color = tokens.textMuted,
                style = MaterialTheme.typography.labelSmall,
            )
        }
    }
}
