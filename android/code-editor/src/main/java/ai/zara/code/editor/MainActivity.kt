package ai.zara.code.editor

import ai.zara.code.workbench.CodeWorkbenchSurface
import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import android.os.Bundle
import androidx.activity.ComponentActivity
import androidx.activity.compose.setContent
import androidx.compose.foundation.isSystemInDarkTheme
import androidx.compose.material3.MaterialTheme
import androidx.compose.material3.darkColorScheme

class MainActivity : ComponentActivity() {
    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContent {
            val tokens = themeTokens(
                theme = ZaraTheme.Outrun,
                systemDark = isSystemInDarkTheme(),
                reducedGlow = false,
            )
            MaterialTheme(
                colorScheme = darkColorScheme(
                    primary = tokens.primary,
                    secondary = tokens.secondary,
                    background = tokens.background,
                    surface = tokens.surface,
                    onBackground = tokens.text,
                    onSurface = tokens.text,
                    error = tokens.error,
                )
            ) {
                CodeWorkbenchSurface(
                    tokens = tokens,
                    title = "Zara Code",
                    platformVoiceEnabled = true,
                )
            }
        }
    }
}
