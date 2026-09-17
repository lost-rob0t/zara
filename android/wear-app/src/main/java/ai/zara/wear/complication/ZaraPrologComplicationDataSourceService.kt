package ai.zara.wear.complication

import ai.zara.ui.complication.PrologComplicationTemplate
import ai.zara.ui.complication.PrologComplicationType
import ai.zara.wear.WearMainActivity
import android.app.PendingIntent
import android.content.Intent
import androidx.wear.watchface.complications.data.ComplicationData
import androidx.wear.watchface.complications.data.ComplicationType
import androidx.wear.watchface.complications.data.LongTextComplicationData
import androidx.wear.watchface.complications.data.NoDataComplicationData
import androidx.wear.watchface.complications.data.PlainComplicationText
import androidx.wear.watchface.complications.data.RangedValueComplicationData
import androidx.wear.watchface.complications.data.ShortTextComplicationData
import androidx.wear.watchface.complications.datasource.ComplicationRequest
import androidx.wear.watchface.complications.datasource.SuspendingComplicationDataSourceService

class ZaraPrologComplicationDataSourceService : SuspendingComplicationDataSourceService() {
    override suspend fun onComplicationRequest(request: ComplicationRequest): ComplicationData {
        val template = WearComplicationStore(this).load()?.template ?: return NoDataComplicationData()
        return render(template, request.complicationType)
    }

    override fun getPreviewData(type: ComplicationType): ComplicationData = when (type) {
        ComplicationType.SHORT_TEXT -> shortText(
            PrologComplicationTemplate(
                id = "preview",
                type = PrologComplicationType.SHORT_TEXT,
                text = "Ready",
                title = "Zara",
                description = "Zara status",
            )
        )
        ComplicationType.LONG_TEXT -> longText(
            PrologComplicationTemplate(
                id = "preview",
                type = PrologComplicationType.LONG_TEXT,
                text = "Symbolic runtime ready",
                title = "Zara",
                description = "Zara symbolic runtime status",
            )
        )
        ComplicationType.RANGED_VALUE -> rangedValue(
            PrologComplicationTemplate(
                id = "preview",
                type = PrologComplicationType.RANGED_VALUE,
                text = "72%",
                title = "Load",
                description = "Example runtime load",
                rangeValue = 72f,
                rangeMin = 0f,
                rangeMax = 100f,
            )
        )
        else -> NoDataComplicationData()
    }

    private fun render(
        template: PrologComplicationTemplate,
        requestedType: ComplicationType,
    ): ComplicationData = when {
        template.type == PrologComplicationType.SHORT_TEXT && requestedType == ComplicationType.SHORT_TEXT ->
            shortText(template)
        template.type == PrologComplicationType.LONG_TEXT && requestedType == ComplicationType.LONG_TEXT ->
            longText(template)
        template.type == PrologComplicationType.RANGED_VALUE && requestedType == ComplicationType.RANGED_VALUE ->
            rangedValue(template)
        else -> NoDataComplicationData()
    }

    private fun shortText(template: PrologComplicationTemplate): ComplicationData {
        val builder = ShortTextComplicationData.Builder(
            text = text(template.text),
            contentDescription = text(template.description),
        ).setTapAction(openAppAction())
        template.title?.let { builder.setTitle(text(it)) }
        return builder.build()
    }

    private fun longText(template: PrologComplicationTemplate): ComplicationData {
        val builder = LongTextComplicationData.Builder(
            text = text(template.text),
            contentDescription = text(template.description),
        ).setTapAction(openAppAction())
        template.title?.let { builder.setTitle(text(it)) }
        return builder.build()
    }

    private fun rangedValue(template: PrologComplicationTemplate): ComplicationData {
        val builder = RangedValueComplicationData.Builder(
            value = requireNotNull(template.rangeValue),
            min = requireNotNull(template.rangeMin),
            max = requireNotNull(template.rangeMax),
            contentDescription = text(template.description),
        )
            .setText(text(template.text))
            .setTapAction(openAppAction())
        template.title?.let { builder.setTitle(text(it)) }
        return builder.build()
    }

    private fun text(value: String) = PlainComplicationText.Builder(value).build()

    private fun openAppAction(): PendingIntent = PendingIntent.getActivity(
        this,
        0,
        Intent(this, WearMainActivity::class.java).apply {
            flags = Intent.FLAG_ACTIVITY_NEW_TASK or Intent.FLAG_ACTIVITY_CLEAR_TOP
        },
        PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE,
    )
}
