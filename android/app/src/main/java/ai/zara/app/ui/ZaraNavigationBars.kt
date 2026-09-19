package ai.zara.app.ui

import androidx.compose.foundation.layout.fillMaxHeight
import androidx.compose.foundation.layout.heightIn
import androidx.compose.foundation.layout.padding
import androidx.compose.foundation.rememberScrollState
import androidx.compose.foundation.verticalScroll
import androidx.compose.material3.ExperimentalMaterial3Api
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.NavigationRail
import androidx.compose.material3.NavigationRailItem
import androidx.compose.material3.NavigationRailItemDefaults
import androidx.compose.material3.ScrollableTabRow
import androidx.compose.material3.Tab
import androidx.compose.material3.Text
import androidx.compose.runtime.Composable
import androidx.compose.runtime.saveable.listSaver
import androidx.compose.ui.Modifier
import androidx.compose.ui.semantics.clearAndSetSemantics
import androidx.compose.ui.unit.dp

internal val AppNavigationSaver = listSaver<AppNavigation, String>(
    save = { it.save() },
    restore = { AppNavigation.restore(it) },
)

@Composable
internal fun ZaraNavigationRail(selected: AppMenu, onSelect: (AppMenu) -> Unit) {
    val tokens = LocalZaraTokens.current
    NavigationRail(
        modifier = Modifier.fillMaxHeight().verticalScroll(rememberScrollState()),
        containerColor = tokens.surfaceElevated,
        contentColor = tokens.text,
    ) {
        AppMenu.entries.forEach { menu ->
            NavigationRailItem(
                selected = selected == menu,
                onClick = { onSelect(menu) },
                icon = { Text(menu.glyph, modifier = Modifier.clearAndSetSemantics { }) },
                label = { Text(menu.label, maxLines = 2) },
                modifier = Modifier.padding(vertical = 4.dp).heightIn(min = 64.dp),
                colors = NavigationRailItemDefaults.colors(
                    selectedIconColor = tokens.accentCyan,
                    selectedTextColor = tokens.text,
                    indicatorColor = tokens.ambientGlow,
                    unselectedIconColor = tokens.textMuted,
                    unselectedTextColor = tokens.textMuted,
                ),
            )
        }
    }
}

@OptIn(ExperimentalMaterial3Api::class)
@Composable
internal fun ZaraRouteTabs(navigation: AppNavigation, onSelect: (AppRoute) -> Unit) {
    val tokens = LocalZaraTokens.current
    val routes = routesFor(navigation.menu)
    ScrollableTabRow(
        selectedTabIndex = routes.indexOf(navigation.route),
        containerColor = tokens.background,
        contentColor = tokens.accentCyan,
        edgePadding = 8.dp,
        divider = {},
    ) {
        routes.forEach { route ->
            Tab(
                selected = route == navigation.route,
                onClick = { onSelect(route) },
                modifier = Modifier.heightIn(min = 48.dp),
                selectedContentColor = tokens.accentCyan,
                unselectedContentColor = tokens.textMuted,
                text = { Text(route.label, style = MaterialTheme.typography.labelLarge) },
            )
        }
    }
}
