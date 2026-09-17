package ai.zara.app.ui

enum class AppSurface(val label: String, val glyph: String, val gatedIssue: String? = null) {
    Chat("Chat", "⌂"),
    Logic("Logic", "λ"),
    Voice("Voice", "◉"),
    Projects("Projects", "◇", "#653"),
    Remote("Remote", "⇄"),
    Scheduled("Scheduled", "◷", "#654"),
    Plugins("Plugins", "⬡"),
    Themes("Themes", "◐"),
    Diagnostics("Diagnostics", "⌁"),
    Settings("Settings", "⚙"),
    About("About", "ⓘ"),
}

internal enum class SettingsTab(val label: String) { General("General"), Plugins("Plugins") }

internal fun drawerSurfaces(): List<AppSurface> = AppSurface.entries.filter { it != AppSurface.Plugins }

internal fun displaySurface(surface: AppSurface): AppSurface =
    if (surface == AppSurface.Plugins) AppSurface.Settings else surface

internal fun initialSettingsTab(surface: AppSurface): SettingsTab =
    if (surface == AppSurface.Plugins) SettingsTab.Plugins else SettingsTab.General
