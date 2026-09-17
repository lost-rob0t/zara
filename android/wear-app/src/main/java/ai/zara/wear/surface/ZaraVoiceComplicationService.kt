package ai.zara.wear.surface

import android.app.PendingIntent
import androidx.wear.watchface.complications.data.ComplicationData
import androidx.wear.watchface.complications.data.ComplicationType
import androidx.wear.watchface.complications.data.PlainComplicationText
import androidx.wear.watchface.complications.data.ShortTextComplicationData
import androidx.wear.watchface.complications.datasource.ComplicationRequest
import androidx.wear.watchface.complications.datasource.SuspendingComplicationDataSourceService

class ZaraVoiceComplicationService : SuspendingComplicationDataSourceService() {
    override suspend fun onComplicationRequest(request: ComplicationRequest): ComplicationData? =
        buildVoiceComplicationData(
            request.complicationType,
            ZaraWearLaunchTargets.voicePendingIntent(this),
        )

    override fun getPreviewData(type: ComplicationType): ComplicationData? =
        buildVoiceComplicationData(type)
}

internal fun buildVoiceComplicationData(
    type: ComplicationType,
    tapAction: PendingIntent? = null,
): ComplicationData? {
    if (type != ComplicationType.SHORT_TEXT) return null

    val builder = ShortTextComplicationData.Builder(
        text = PlainComplicationText.Builder("Voice").build(),
        contentDescription = PlainComplicationText.Builder("Open Zara Voice").build(),
    ).setTitle(PlainComplicationText.Builder("Zara").build())

    if (tapAction != null) builder.setTapAction(tapAction)
    return builder.build()
}
