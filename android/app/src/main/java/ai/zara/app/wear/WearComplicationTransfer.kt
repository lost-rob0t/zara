package ai.zara.app.wear

import ai.zara.ui.complication.ComplicationWire
import ai.zara.ui.complication.PrologComplicationTemplate
import ai.zara.ui.complication.PrologComplicationType
import android.content.Context
import com.google.android.gms.tasks.Task
import com.google.android.gms.wearable.DataItem
import com.google.android.gms.wearable.PutDataMapRequest
import com.google.android.gms.wearable.Wearable

class WearComplicationTransfer(
    context: Context,
) {
    private val appContext = context.applicationContext

    fun pushActive(template: PrologComplicationTemplate): Task<DataItem> {
        val safe = template.validated()
        val request = PutDataMapRequest.create(ComplicationWire.PATH).apply {
            dataMap.putInt(ComplicationWire.KEY_SCHEMA, ComplicationWire.SCHEMA_VERSION)
            dataMap.putLong(ComplicationWire.KEY_REVISION, System.currentTimeMillis())
            dataMap.putString(ComplicationWire.KEY_ID, safe.id)
            dataMap.putString(ComplicationWire.KEY_TYPE, safe.type.atom)
            dataMap.putString(ComplicationWire.KEY_TEXT, safe.text)
            dataMap.putString(ComplicationWire.KEY_TITLE, safe.title.orEmpty())
            dataMap.putString(ComplicationWire.KEY_DESCRIPTION, safe.description)
            if (safe.type == PrologComplicationType.RANGED_VALUE) {
                dataMap.putFloat(ComplicationWire.KEY_RANGE_VALUE, requireNotNull(safe.rangeValue))
                dataMap.putFloat(ComplicationWire.KEY_RANGE_MIN, requireNotNull(safe.rangeMin))
                dataMap.putFloat(ComplicationWire.KEY_RANGE_MAX, requireNotNull(safe.rangeMax))
            }
        }.asPutDataRequest().setUrgent()
        return Wearable.getDataClient(appContext).putDataItem(request)
    }

    fun connectedWatchCount(): Task<Int> =
        Wearable.getNodeClient(appContext).connectedNodes.continueWith { task ->
            task.result?.size ?: 0
        }
}
