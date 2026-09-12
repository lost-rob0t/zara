package ai.zara.wear.surface

import androidx.wear.watchface.complications.data.ComplicationData
import androidx.wear.watchface.complications.data.ComplicationType
import androidx.wear.watchface.complications.data.PlainComplicationText
import androidx.wear.watchface.complications.data.ShortTextComplicationData
import androidx.wear.watchface.complications.datasource.ComplicationRequest
import androidx.wear.watchface.complications.datasource.SuspendingComplicationDataSourceService

class ZaraVoiceComplicationService : SuspendingComplicationDataSourceService() {
    override suspend fun onComplicationRequest(request: ComplicationRequest): ComplicationData? =
        dataFor(request.complicationType)

    override fun getPreviewData(type: ComplicationType): ComplicationData? = dataFor(type)

    private fun dataFor(type: ComplicationType): ComplicationData? {
        if (type != ComplicationType.SHORT_TEXT) return null
        return ShortTextComplicationData.Builder(
            text = PlainComplicationText.Builder("Voice").build(),
            contentDescription = PlainComplicationText.Builder("Open Zara Voice").build(),
        )
            .setTitle(PlainComplicationText.Builder("Zara").build())
            .setTapAction(ZaraWearLaunchTargets.voicePendingIntent(this))
            .build()
    }
}
