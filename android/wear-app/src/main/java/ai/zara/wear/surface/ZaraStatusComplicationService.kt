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

internal data class ZaraShortTextComplicationPayload(
    val text: String,
    val title: String,
    val contentDescription: String,
)

internal fun statusComplicationPayload(type: ComplicationType): ZaraShortTextComplicationPayload? {
    if (type != ComplicationType.SHORT_TEXT) return null
    return ZaraShortTextComplicationPayload(
        text = "Zara",
        title = "offline",
        contentDescription = "Open Zara",
    )
}

internal fun ZaraShortTextComplicationPayload.toComplicationData(
    tapAction: PendingIntent? = null,
): ComplicationData {
    val builder = ShortTextComplicationData.Builder(
        text = PlainComplicationText.Builder(text).build(),
        contentDescription = PlainComplicationText.Builder(contentDescription).build(),
    ).setTitle(PlainComplicationText.Builder(title).build())

    if (tapAction != null) builder.setTapAction(tapAction)
    return builder.build()
}

internal fun buildStatusComplicationData(
    type: ComplicationType,
    tapAction: PendingIntent? = null,
): ComplicationData? = statusComplicationPayload(type)?.toComplicationData(tapAction)
