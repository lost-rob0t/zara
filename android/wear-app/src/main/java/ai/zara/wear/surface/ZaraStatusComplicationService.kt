package ai.zara.wear.surface

import androidx.wear.watchface.complications.data.ComplicationData
import androidx.wear.watchface.complications.data.ComplicationType
import androidx.wear.watchface.complications.data.PlainComplicationText
import androidx.wear.watchface.complications.data.ShortTextComplicationData
import androidx.wear.watchface.complications.datasource.ComplicationRequest
import androidx.wear.watchface.complications.datasource.SuspendingComplicationDataSourceService

class ZaraStatusComplicationService : SuspendingComplicationDataSourceService() {
    override suspend fun onComplicationRequest(request: ComplicationRequest): ComplicationData? =
        dataFor(request.complicationType)

    override fun getPreviewData(type: ComplicationType): ComplicationData? = dataFor(type)

    private fun dataFor(type: ComplicationType): ComplicationData? {
        if (type != ComplicationType.SHORT_TEXT) return null
        return ShortTextComplicationData.Builder(
            text = PlainComplicationText.Builder("Zara").build(),
            contentDescription = PlainComplicationText.Builder("Open Zara").build(),
        )
            .setTitle(PlainComplicationText.Builder("offline").build())
            .setTapAction(ZaraWearLaunchTargets.mainPendingIntent(this))
            .build()
    }
}
