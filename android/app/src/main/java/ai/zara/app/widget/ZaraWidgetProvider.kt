package ai.zara.app.widget

import ai.zara.app.MainActivity
import ai.zara.app.R
import ai.zara.app.ui.ThemePreferenceStore
import ai.zara.ui.theme.ZaraSemanticTokens
import ai.zara.ui.theme.ZaraTheme
import ai.zara.ui.theme.themeTokens
import android.app.PendingIntent
import android.appwidget.AppWidgetManager
import android.appwidget.AppWidgetProvider
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.content.res.Configuration
import android.graphics.Bitmap
import android.graphics.Canvas
import android.graphics.Paint
import android.graphics.RectF
import android.os.Build
import android.os.Bundle
import android.util.TypedValue
import android.view.Gravity
import android.view.View
import android.widget.RemoteViews
import androidx.compose.ui.graphics.toArgb
import java.io.File

const val ACTION_WIDGET_ROUTE = "ai.zara.app.action.WIDGET_ROUTE"
const val EXTRA_WIDGET_ROUTE = "ai.zara.app.extra.WIDGET_ROUTE"

class ZaraAssistantWidgetProvider : ZaraWidgetProvider(WidgetKind.ASSISTANT)
class ZaraRuntimeWidgetProvider : ZaraWidgetProvider(WidgetKind.RUNTIME)
class ZaraActionsWidgetProvider : ZaraWidgetProvider(WidgetKind.ACTIONS)

abstract class ZaraWidgetProvider(
    private val kind: WidgetKind,
) : AppWidgetProvider() {
    override fun onUpdate(context: Context, manager: AppWidgetManager, appWidgetIds: IntArray) {
        appWidgetIds.forEach { ZaraWidgetRenderer.update(context, manager, it, kind) }
    }

    override fun onAppWidgetOptionsChanged(
        context: Context,
        manager: AppWidgetManager,
        appWidgetId: Int,
        newOptions: Bundle,
    ) {
        ZaraWidgetRenderer.update(context, manager, appWidgetId, kind)
    }
}

object ZaraWidgetUpdater {
    fun refreshAll(context: Context) {
        val manager = AppWidgetManager.getInstance(context)
        listOf(
            ZaraAssistantWidgetProvider::class.java to WidgetKind.ASSISTANT,
            ZaraRuntimeWidgetProvider::class.java to WidgetKind.RUNTIME,
            ZaraActionsWidgetProvider::class.java to WidgetKind.ACTIONS,
        ).forEach { (provider, kind) ->
            manager.getAppWidgetIds(ComponentName(context, provider)).forEach { appWidgetId ->
                ZaraWidgetRenderer.update(context, manager, appWidgetId, kind)
            }
        }
    }
}

data class WidgetStyleStatus(
    val name: String,
    val theme: ZaraTheme,
    val imported: Boolean,
)

object WidgetStyleEnvironment {
    fun load(context: Context): WidgetStyleSheet {
        val selectedTheme = ThemePreferenceStore(File(context.filesDir, "theme.bin")).load()
        return store(context).load(defaultSource(context), selectedTheme, systemDark(context))
    }

    fun status(context: Context): WidgetStyleStatus {
        val sheet = load(context)
        return WidgetStyleStatus(sheet.name, sheet.theme, store(context).hasImport())
    }

    fun import(context: Context, source: String): WidgetStyleStatus {
        val selectedTheme = ThemePreferenceStore(File(context.filesDir, "theme.bin")).load()
        val sheet = store(context).import(source, selectedTheme, systemDark(context))
        return WidgetStyleStatus(sheet.name, sheet.theme, imported = true)
    }

    fun export(context: Context): String = store(context).exportSource() ?: defaultSource(context)

    fun reset(context: Context): WidgetStyleStatus {
        store(context).reset()
        return status(context)
    }

    private fun store(context: Context) = WidgetStyleStore(
        File(context.filesDir, "widget-style.pl"),
        WidgetStyleCompiler(AndroidWidgetSemanticPaletteResolver),
    )

    private fun defaultSource(context: Context): String =
        context.assets.open("prolog/widget_styles.pl").bufferedReader().use { it.readText() }

    private fun systemDark(context: Context): Boolean =
        context.resources.configuration.uiMode and Configuration.UI_MODE_NIGHT_MASK ==
            Configuration.UI_MODE_NIGHT_YES
}

private object AndroidWidgetSemanticPaletteResolver : WidgetSemanticPaletteResolver {
    override fun resolve(theme: ZaraTheme, role: WidgetSemanticColorRole): Int {
        val tokens = themeTokens(theme, systemDark = theme != ZaraTheme.Light, reducedGlow = false)
        return tokens.color(role).toArgb()
    }

    private fun ZaraSemanticTokens.color(role: WidgetSemanticColorRole) = when (role) {
        WidgetSemanticColorRole.BACKGROUND -> background
        WidgetSemanticColorRole.SURFACE -> surface
        WidgetSemanticColorRole.SURFACE_ELEVATED -> surfaceElevated
        WidgetSemanticColorRole.BORDER -> border
        WidgetSemanticColorRole.BORDER_ACTIVE -> borderActive
        WidgetSemanticColorRole.PRIMARY -> primary
        WidgetSemanticColorRole.SECONDARY -> secondary
        WidgetSemanticColorRole.ACCENT_MAGENTA -> accentMagenta
        WidgetSemanticColorRole.ACCENT_CYAN -> accentCyan
        WidgetSemanticColorRole.TEXT -> text
        WidgetSemanticColorRole.TEXT_MUTED -> textMuted
        WidgetSemanticColorRole.SUCCESS -> success
        WidgetSemanticColorRole.WARNING -> warning
        WidgetSemanticColorRole.ERROR -> error
        WidgetSemanticColorRole.FOCUS -> focus
        WidgetSemanticColorRole.AMBIENT_GLOW -> ambientGlow
    }
}

private object ZaraWidgetRenderer {
    fun update(context: Context, manager: AppWidgetManager, appWidgetId: Int, kind: WidgetKind) {
        val style = WidgetStyleEnvironment.load(context).forWidget(kind)
        val snapshot = WidgetRuntimeSnapshotStore(
            File(context.noBackupFilesDir, "zara/widget-runtime.bin"),
        ).load()
        val options = manager.getAppWidgetOptions(appWidgetId)
        val minWidthDp = options.getInt(AppWidgetManager.OPTION_APPWIDGET_MIN_WIDTH, 250)
        val minHeightDp = options.getInt(AppWidgetManager.OPTION_APPWIDGET_MIN_HEIGHT, 110)
        val density = context.resources.displayMetrics.density
        val widthPx = (minWidthDp * density).toInt().coerceIn(64, 1_024)
        val heightPx = (minHeightDp * density).toInt().coerceIn(64, 1_024)
        val views = RemoteViews(context.packageName, R.layout.widget_zara)
        val content = WidgetContent.from(kind, style, snapshot)

        views.setImageViewBitmap(
            R.id.widget_background,
            roundedPanel(
                widthPx,
                heightPx,
                style.colors.background,
                style.colors.border,
                style.metrics.cornerRadiusDp * density,
                style.metrics.borderWidthDp * density,
            ),
        )
        val padding = (style.metrics.outerPaddingDp * density).toInt()
        views.setViewPadding(R.id.widget_content, padding, padding, padding, padding)
        bindText(views, style, content)
        bindVisibility(views, style, minWidthDp, minHeightDp)
        bindAlignment(views, style.alignment)
        bindActions(context, views, appWidgetId, style, widthPx, density)
        val sigilPixels = (style.metrics.sigilSizeDp * density).toInt().coerceAtLeast(12)
        views.setImageViewBitmap(
            R.id.widget_sigil_background,
            roundedPanel(
                sigilPixels,
                sigilPixels,
                style.colors.surface,
                style.colors.border,
                style.metrics.actionCornerRadiusDp * density,
                style.metrics.borderWidthDp * density,
            ),
        )
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            views.setViewLayoutWidth(R.id.widget_sigil_container, style.metrics.sigilSizeDp.toFloat(), TypedValue.COMPLEX_UNIT_DIP)
            views.setViewLayoutHeight(R.id.widget_sigil_container, style.metrics.sigilSizeDp.toFloat(), TypedValue.COMPLEX_UNIT_DIP)
        }
        views.setInt(R.id.widget_sigil, "setColorFilter", style.colors.sigil)
        views.setOnClickPendingIntent(
            R.id.widget_root,
            routeIntent(context, appWidgetId, 0, style.actions.primary),
        )
        manager.updateAppWidget(appWidgetId, views)
    }

    private fun bindText(views: RemoteViews, style: WidgetResolvedStyle, content: WidgetContent) {
        val entries = listOf(
            Triple(R.id.widget_eyebrow, content.eyebrow, style.colors.label),
            Triple(R.id.widget_title, content.title, style.colors.title),
            Triple(R.id.widget_subtitle, content.subtitle, style.colors.body),
            Triple(R.id.widget_status, content.status, style.colors.status),
            Triple(R.id.widget_primary_label, style.text.primaryLabel, style.colors.actionText),
            Triple(R.id.widget_secondary_label, style.text.secondaryLabel, style.colors.actionText),
            Triple(R.id.widget_tertiary_label, style.text.tertiaryLabel, style.colors.actionText),
        )
        entries.forEach { (id, text, color) ->
            views.setTextViewText(id, text)
            views.setTextColor(id, color)
        }
        views.setTextViewTextSize(R.id.widget_eyebrow, TypedValue.COMPLEX_UNIT_SP, style.metrics.labelSp.toFloat())
        views.setTextViewTextSize(R.id.widget_title, TypedValue.COMPLEX_UNIT_SP, style.metrics.titleSp.toFloat())
        views.setTextViewTextSize(R.id.widget_subtitle, TypedValue.COMPLEX_UNIT_SP, style.metrics.bodySp.toFloat())
        views.setTextViewTextSize(R.id.widget_status, TypedValue.COMPLEX_UNIT_SP, style.metrics.labelSp.toFloat())
        listOf(R.id.widget_primary_label, R.id.widget_secondary_label, R.id.widget_tertiary_label).forEach { id ->
            views.setTextViewTextSize(id, TypedValue.COMPLEX_UNIT_SP, style.metrics.actionSp.toFloat())
        }
        val gap = style.metrics.contentGapDp
        views.setViewPadding(R.id.widget_subtitle, 0, gap, 0, 0)
        views.setViewPadding(R.id.widget_status, 0, gap, 0, gap)
    }

    private fun bindVisibility(
        views: RemoteViews,
        style: WidgetResolvedStyle,
        widthDp: Int,
        heightDp: Int,
    ) {
        fun visibility(show: Boolean) = if (show) View.VISIBLE else View.GONE
        views.setViewVisibility(R.id.widget_sigil_container, visibility(style.visibility.showSigil && widthDp >= 180))
        views.setViewVisibility(
            R.id.widget_subtitle,
            visibility(style.visibility.showSubtitle && heightDp >= 105),
        )
        views.setViewVisibility(
            R.id.widget_status,
            visibility(style.visibility.showStatus && heightDp >= 90),
        )
        views.setViewVisibility(
            R.id.widget_secondary_action,
            visibility(style.visibility.showSecondary && widthDp >= 180),
        )
        views.setViewVisibility(
            R.id.widget_tertiary_action,
            visibility(style.visibility.showTertiary && widthDp >= 250),
        )
    }

    private fun bindAlignment(views: RemoteViews, alignment: WidgetAlignment) {
        val horizontal = when (alignment) {
            WidgetAlignment.START -> Gravity.START
            WidgetAlignment.CENTER -> Gravity.CENTER_HORIZONTAL
            WidgetAlignment.END -> Gravity.END
        }
        views.setInt(R.id.widget_content, "setGravity", horizontal or Gravity.CENTER_VERTICAL)
        listOf(R.id.widget_eyebrow, R.id.widget_title, R.id.widget_subtitle, R.id.widget_status).forEach { id ->
            views.setInt(id, "setGravity", horizontal)
        }
    }

    private fun bindActions(
        context: Context,
        views: RemoteViews,
        appWidgetId: Int,
        style: WidgetResolvedStyle,
        widthPx: Int,
        density: Float,
    ) {
        val backgrounds = listOf(
            R.id.widget_primary_background,
            R.id.widget_secondary_background,
            R.id.widget_tertiary_background,
        )
        val panel = roundedPanel(
            (widthPx / 3).coerceAtLeast(48),
            (40 * density).toInt().coerceAtLeast(32),
            style.colors.actionBackground,
            style.colors.border,
            style.metrics.actionCornerRadiusDp * density,
            style.metrics.borderWidthDp * density,
        )
        backgrounds.forEach { views.setImageViewBitmap(it, panel) }
        val slots = listOf(
            Triple(R.id.widget_primary_action, 1, style.actions.primary),
            Triple(R.id.widget_secondary_action, 2, style.actions.secondary),
            Triple(R.id.widget_tertiary_action, 3, style.actions.tertiary),
        )
        slots.forEach { (id, slot, route) ->
            views.setOnClickPendingIntent(id, routeIntent(context, appWidgetId, slot, route))
        }
    }

    private fun routeIntent(
        context: Context,
        appWidgetId: Int,
        slot: Int,
        route: WidgetRoute,
    ): PendingIntent {
        val intent = Intent(context, MainActivity::class.java)
            .setAction(ACTION_WIDGET_ROUTE)
            .putExtra(EXTRA_WIDGET_ROUTE, route.atom)
            .addFlags(Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TOP)
        return PendingIntent.getActivity(
            context,
            appWidgetId * 10 + slot,
            intent,
            PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE,
        )
    }

    private fun roundedPanel(
        width: Int,
        height: Int,
        fillColor: Int,
        borderColor: Int,
        radius: Float,
        borderWidth: Float,
    ): Bitmap {
        val bitmap = Bitmap.createBitmap(width.coerceAtLeast(1), height.coerceAtLeast(1), Bitmap.Config.ARGB_8888)
        val canvas = Canvas(bitmap)
        val bounds = RectF(0f, 0f, bitmap.width.toFloat(), bitmap.height.toFloat())
        val fill = Paint(Paint.ANTI_ALIAS_FLAG).apply {
            color = fillColor
            style = Paint.Style.FILL
        }
        canvas.drawRoundRect(bounds, radius, radius, fill)
        if (borderWidth > 0f) {
            val half = borderWidth / 2f
            val strokeBounds = RectF(half, half, bitmap.width - half, bitmap.height - half)
            val stroke = Paint(Paint.ANTI_ALIAS_FLAG).apply {
                color = borderColor
                style = Paint.Style.STROKE
                strokeWidth = borderWidth
            }
            canvas.drawRoundRect(strokeBounds, radius, radius, stroke)
        }
        return bitmap
    }
}

private data class WidgetContent(
    val eyebrow: String,
    val title: String,
    val subtitle: String,
    val status: String,
) {
    companion object {
        fun from(
            kind: WidgetKind,
            style: WidgetResolvedStyle,
            snapshot: WidgetRuntimeSnapshot,
        ): WidgetContent = when (kind) {
            WidgetKind.ASSISTANT -> WidgetContent(
                style.text.eyebrow,
                style.text.title,
                style.text.subtitle,
                "${snapshot.mode} · ${snapshot.local}",
            )
            WidgetKind.RUNTIME -> WidgetContent(
                style.text.eyebrow,
                style.text.title,
                "${snapshot.remote} · ${snapshot.local}",
                "MODE ${snapshot.mode}",
            )
            WidgetKind.ACTIONS -> WidgetContent(
                style.text.eyebrow,
                style.text.title,
                style.text.subtitle,
                snapshot.remote,
            )
        }
    }
}
