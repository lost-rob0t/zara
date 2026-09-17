package ai.zara.wear.surface

import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import android.content.ComponentName
import androidx.compose.ui.graphics.toArgb
import androidx.wear.protolayout.ActionBuilders.launchAction
import androidx.wear.protolayout.LayoutElementBuilders.Column
import androidx.wear.protolayout.ResourceBuilders.Resources
import androidx.wear.protolayout.TimelineBuilders.Timeline
import androidx.wear.protolayout.material3.ColorScheme
import androidx.wear.protolayout.material3.Typography.BODY_MEDIUM
import androidx.wear.protolayout.material3.Typography.TITLE_MEDIUM
import androidx.wear.protolayout.material3.materialScope
import androidx.wear.protolayout.material3.primaryLayout
import androidx.wear.protolayout.material3.text
import androidx.wear.protolayout.material3.textButton
import androidx.wear.protolayout.modifiers.clickable
import androidx.wear.protolayout.types.argb
import androidx.wear.protolayout.types.layoutString
import androidx.wear.tiles.RequestBuilders
import androidx.wear.tiles.RequestBuilders.ResourcesRequest
import androidx.wear.tiles.TileBuilders.Tile
import androidx.wear.tiles.TileService
import com.google.common.util.concurrent.Futures

private const val RESOURCES_VERSION = "zara-1"

class ZaraTileService : TileService() {
    override fun onTileRequest(requestParams: RequestBuilders.TileRequest) =
        Futures.immediateFuture(
            Tile.Builder()
                .setResourcesVersion(RESOURCES_VERSION)
                .setTileTimeline(
                    Timeline.fromLayoutElement(
                        materialScope(
                            context = this,
                            deviceConfiguration = requestParams.deviceConfiguration,
                            allowDynamicTheme = false,
                            defaultColorScheme = zaraColorScheme(),
                        ) {
                            primaryLayout(
                                titleSlot = {
                                    text("ZARA".layoutString, typography = TITLE_MEDIUM)
                                },
                                mainSlot = {
                                    val main = ZaraWearLaunchTargets.mainComponent()
                                    val voice = ZaraWearLaunchTargets.voiceComponent()
                                    Column.Builder()
                                        .addContent(
                                            text(
                                                "OFFLINE • WATCH CLIENT".layoutString,
                                                typography = BODY_MEDIUM,
                                            )
                                        )
                                        .addContent(
                                            textButton(
                                                labelContent = { text("OPEN ZARA".layoutString) },
                                                onClick = clickable(
                                                    action = launchAction(ComponentName(main.first, main.second))
                                                ),
                                            )
                                        )
                                        .addContent(
                                            textButton(
                                                labelContent = { text("VOICE".layoutString) },
                                                onClick = clickable(
                                                    action = launchAction(ComponentName(voice.first, voice.second))
                                                ),
                                            )
                                        )
                                        .build()
                                },
                            )
                        }
                    )
                )
                .build()
        )

    override fun onTileResourcesRequest(requestParams: ResourcesRequest) =
        Futures.immediateFuture(
            Resources.Builder().setVersion(requestParams.version.ifBlank { RESOURCES_VERSION }).build()
        )

    private fun zaraColorScheme(): ColorScheme {
        val tokens = themeTokens(ZaraTheme.Outrun, systemDark = true, reducedGlow = false)
        return ColorScheme(
            primary = tokens.primary.toArgb().argb,
            primaryDim = tokens.borderActive.toArgb().argb,
            primaryContainer = tokens.surfaceElevated.toArgb().argb,
            onPrimary = tokens.background.toArgb().argb,
            onPrimaryContainer = tokens.text.toArgb().argb,
            secondary = tokens.secondary.toArgb().argb,
            secondaryDim = tokens.accentCyan.toArgb().argb,
            secondaryContainer = tokens.surface.toArgb().argb,
            onSecondary = tokens.background.toArgb().argb,
            onSecondaryContainer = tokens.text.toArgb().argb,
            tertiary = tokens.accentMagenta.toArgb().argb,
            tertiaryDim = tokens.borderActive.toArgb().argb,
            tertiaryContainer = tokens.surfaceElevated.toArgb().argb,
            onTertiary = tokens.background.toArgb().argb,
            onTertiaryContainer = tokens.text.toArgb().argb,
            surfaceContainerLow = tokens.background.toArgb().argb,
            surfaceContainer = tokens.surface.toArgb().argb,
            surfaceContainerHigh = tokens.surfaceElevated.toArgb().argb,
            onSurface = tokens.text.toArgb().argb,
            onSurfaceVariant = tokens.textMuted.toArgb().argb,
            outline = tokens.borderActive.toArgb().argb,
            outlineVariant = tokens.border.toArgb().argb,
            background = tokens.background.toArgb().argb,
            onBackground = tokens.text.toArgb().argb,
            error = tokens.error.toArgb().argb,
            errorDim = tokens.error.toArgb().argb,
            errorContainer = tokens.surfaceElevated.toArgb().argb,
            onError = tokens.background.toArgb().argb,
            onErrorContainer = tokens.text.toArgb().argb,
        )
    }
}
