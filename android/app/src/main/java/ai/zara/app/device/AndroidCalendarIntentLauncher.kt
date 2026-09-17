package ai.zara.app.device

import android.content.Context
import android.content.Intent
import android.provider.CalendarContract

class AndroidCalendarIntentLauncher(context: Context) : CalendarIntentLauncher {
    private val appContext = context.applicationContext

    override fun isAvailable(): Boolean =
        insertIntent(null).resolveActivity(appContext.packageManager) != null

    override fun insert(arguments: DeviceActionArguments.CalendarInsert) {
        val intent = insertIntent(arguments)
        check(intent.resolveActivity(appContext.packageManager) != null) {
            "no calendar insert handler is available"
        }
        appContext.startActivity(intent)
    }

    private fun insertIntent(arguments: DeviceActionArguments.CalendarInsert?): Intent =
        Intent(Intent.ACTION_INSERT).apply {
            data = CalendarContract.Events.CONTENT_URI
            addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
            arguments?.let { event ->
                putExtra(CalendarContract.Events.TITLE, event.title)
                putExtra(CalendarContract.EXTRA_EVENT_BEGIN_TIME, event.startMillis)
                putExtra(CalendarContract.EXTRA_EVENT_END_TIME, event.endMillis)
                event.location?.let { putExtra(CalendarContract.Events.EVENT_LOCATION, it) }
                event.description?.let { putExtra(CalendarContract.Events.DESCRIPTION, it) }
            }
        }
}
