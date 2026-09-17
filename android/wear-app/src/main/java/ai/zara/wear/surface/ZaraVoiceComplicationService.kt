package ai.zara.wear.surface

import android.app.PendingIntent
import androidx.wear.watchface.complications.data.ComplicationData
import androidx.wear.watchface.complications.data.ComplicationType
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

internal fun voiceComplicationPayload(type: ComplicationType): ZaraShortTextComplicationPayload? {
    if (type != ComplicationType.SHORT_TEXT) return null
    return ZaraShortTextComplicationPayload(
        text = "Voice",
        title = "Zara",
        contentDescription = "Open Zara Voice",
    )
}

internal fun buildVoiceComplicationData(
    type: ComplicationType,
    tapAction: PendingIntent? = null,
): ComplicationData? = voiceComplicationPayload(type)?.toComplicationData(tapAction)
