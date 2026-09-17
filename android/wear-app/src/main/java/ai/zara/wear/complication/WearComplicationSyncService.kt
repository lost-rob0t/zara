package ai.zara.wear.complication

import ai.zara.ui.complication.ComplicationWire
import ai.zara.ui.complication.PrologComplicationTemplate
import ai.zara.ui.complication.PrologComplicationType
import android.content.ComponentName
import androidx.wear.watchface.complications.datasource.ComplicationDataSourceUpdateRequester
import com.google.android.gms.wearable.DataEvent
import com.google.android.gms.wearable.DataEventBuffer
import com.google.android.gms.wearable.DataMap
import com.google.android.gms.wearable.DataMapItem
import com.google.android.gms.wearable.WearableListenerService

class WearComplicationSyncService : WearableListenerService() {
    override fun onDataChanged(dataEvents: DataEventBuffer) {
        super.onDataChanged(dataEvents)
        dataEvents.forEach { event ->
            if (event.type != DataEvent.TYPE_CHANGED) return@forEach
            if (event.dataItem.uri.path != ComplicationWire.PATH) return@forEach
            val dataMap = DataMapItem.fromDataItem(event.dataItem).dataMap
            val decoded = runCatching { decode(dataMap) }.getOrNull() ?: return@forEach
            WearComplicationStore(this).save(decoded.template, decoded.revision)
            ComplicationDataSourceUpdateRequester.create(
                this,
                ComponentName(this, ZaraPrologComplicationDataSourceService::class.java),
            ).requestUpdateAll()
        }
    }

    private fun decode(dataMap: DataMap): DecodedTemplate {
        require(dataMap.getInt(ComplicationWire.KEY_SCHEMA, -1) == ComplicationWire.SCHEMA_VERSION) {
            "Unsupported complication payload version"
        }
        val id = requireNotNull(dataMap.getString(ComplicationWire.KEY_ID)) { "Missing complication id" }
        val type = requireNotNull(dataMap.getString(ComplicationWire.KEY_TYPE)) { "Missing complication type" }
            .let(PrologComplicationType::fromAtom)
        val text = requireNotNull(dataMap.getString(ComplicationWire.KEY_TEXT)) { "Missing complication text" }
        val title = dataMap.getString(ComplicationWire.KEY_TITLE).orEmpty().ifBlank { null }
        val description = dataMap.getString(ComplicationWire.KEY_DESCRIPTION).orEmpty().ifBlank { text }
        val template = PrologComplicationTemplate(
            id = id,
            type = type,
            text = text,
            title = title,
            description = description,
            rangeValue = if (type == PrologComplicationType.RANGED_VALUE) {
                require(dataMap.containsKey(ComplicationWire.KEY_RANGE_VALUE)) { "Missing range value" }
                dataMap.getFloat(ComplicationWire.KEY_RANGE_VALUE)
            } else null,
            rangeMin = if (type == PrologComplicationType.RANGED_VALUE) {
                require(dataMap.containsKey(ComplicationWire.KEY_RANGE_MIN)) { "Missing range minimum" }
                dataMap.getFloat(ComplicationWire.KEY_RANGE_MIN)
            } else null,
            rangeMax = if (type == PrologComplicationType.RANGED_VALUE) {
                require(dataMap.containsKey(ComplicationWire.KEY_RANGE_MAX)) { "Missing range maximum" }
                dataMap.getFloat(ComplicationWire.KEY_RANGE_MAX)
            } else null,
        ).validated()
        return DecodedTemplate(
            template = template,
            revision = dataMap.getLong(ComplicationWire.KEY_REVISION, 0L),
        )
    }

    private data class DecodedTemplate(
        val template: PrologComplicationTemplate,
        val revision: Long,
    )
}
