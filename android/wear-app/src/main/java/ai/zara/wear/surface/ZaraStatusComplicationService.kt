package ai.zara.wear.surface

import android.app.PendingIntent
import androidx.wear.watchface.complications.data.ComplicationData
import androidx.wear.watchface.complications.data.ComplicationType
import androidx.wear.watchface.complications.data.PlainComplicationText
import androidx.wear.watchface.complications.data.ShortTextComplicationData
import androidx.wear.watchface.complications.datasource.ComplicationRequest
import androidx.wear.watchface.complications.datasource.SuspendingComplicationDataSourceService

class ZaraStatusComplicationService : SuspendingComplicationDataSourceService() {
    override suspend fun onComplicationRequest(request: ComplicationRequest): ComplicationData? =
        buildStatusComplicationData(
            request.complicationType,
            ZaraWearLaunchTargets.mainPendingIntent(this),
        )

    override fun getPreviewData(type: ComplicationType): ComplicationData? =
        buildStatusComplicationData(type)
}

internal fun buildStatusComplicationData(
    type: ComplicationType,
    tapAction: PendingIntent? = null,
): ComplicationData? {
    if (type != ComplicationType.SHORT_TEXT) return null

    val builder = ShortTextComplicationData.Builder(
        text = PlainComplicationText.Builder("Zara").build(),
        contentDescription = PlainComplicationText.Builder("Open Zara").build(),
    ).setTitle(PlainComplicationText.Builder("offline").build())

    if (tapAction != null) builder.setTapAction(tapAction)
    return builder.build()
}
