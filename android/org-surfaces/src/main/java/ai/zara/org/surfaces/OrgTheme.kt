package ai.zara.org.surfaces

import ai.zara.ui.theme.ZaraSemanticTokens
import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.darkColorScheme
import androidx.compose.runtime.Composable
import androidx.compose.runtime.CompositionLocalProvider
import androidx.compose.runtime.staticCompositionLocalOf

val LocalOrgTokens = staticCompositionLocalOf {
    themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
}

fun orgColorScheme(tokens: ZaraSemanticTokens) = darkColorScheme(
    primary = tokens.primary,
    secondary = tokens.secondary,
    background = tokens.background,
    surface = tokens.surface,
    surfaceVariant = tokens.surfaceElevated,
    onPrimary = tokens.surfaceInput,
    onSecondary = tokens.surfaceInput,
    onBackground = tokens.text,
    onSurface = tokens.text,
    onSurfaceVariant = tokens.textMuted,
    error = tokens.error,
)

@Composable
fun OrgTheme(
    theme: ZaraTheme = ZaraTheme.Outrun,
    reducedGlow: Boolean = false,
    content: @Composable () -> Unit,
) {
    val systemDark = isSystemInDarkTheme()
    val tokens = themeTokens(theme, systemDark, reducedGlow)
    CompositionLocalProvider(LocalOrgTokens provides tokens) {
        MaterialTheme(colorScheme = orgColorScheme(tokens), content = content)
    }
}
