package ai.zara.org.reminder

import ai.zara.org.core.OrgReminderSpec
import ai.zara.org.core.OrgReminders
import ai.zara.org.storage.OrgHome
import android.app.AlarmManager
import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Build
import androidx.core.app.NotificationCompat
import java.time.LocalDateTime
import java.time.ZoneId

data class ReminderScheduleSummary(
    val future: Int,
    val exact: Int,
    val inexact: Int,
    val cancelledStale: Int,
    val exactAccess: Boolean,
)

object OrgReminderScheduler {
    private const val PREFS = "org-reminder-schedule"
    private const val KEY_ACTIVE = "active-keys"
    private const val ACTION_FIRE = "ai.zara.org.reminder.FIRE"

    fun reconcile(context: Context): ReminderScheduleSummary {
        val repository = OrgHome.open(context)
            ?: return ReminderScheduleSummary(0, 0, 0, 0, exactAccess(context))

        val now = LocalDateTime.now()
        val future = OrgReminders.fromTasks(repository.allTasks())
            .filter { it.whenLocal.isAfter(now) }

        val prefs = context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
        val previous = prefs.getStringSet(KEY_ACTIVE, emptySet()).orEmpty().toSet()
        val current = future.mapTo(linkedSetOf()) { it.stableKey }

        var cancelled = 0
        (previous - current).forEach { key ->
            cancel(context, key)
            cancelled += 1
        }

        var exact = 0
        var inexact = 0
        future.forEach { reminder ->
            if (schedule(context, reminder)) exact += 1 else inexact += 1
        }

        prefs.edit().putStringSet(KEY_ACTIVE, current).apply()
        return ReminderScheduleSummary(
            future = future.size,
            exact = exact,
            inexact = inexact,
            cancelledStale = cancelled,
            exactAccess = exactAccess(context),
        )
    }

    fun exactAccess(context: Context): Boolean {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.S) return true
        val alarms = context.getSystemService(AlarmManager::class.java)
        return alarms.canScheduleExactAlarms()
    }

    private fun schedule(context: Context, reminder: OrgReminderSpec): Boolean {
        val alarms = context.getSystemService(AlarmManager::class.java)
        val triggerAt = reminder.whenLocal
            .atZone(ZoneId.systemDefault())
            .toInstant()
            .toEpochMilli()
        val pending = pendingIntent(context, reminder, PendingIntent.FLAG_UPDATE_CURRENT)
        val useExact = reminder.explicitTime && exactAccess(context)

        if (useExact) {
            alarms.setExactAndAllowWhileIdle(AlarmManager.RTC_WAKEUP, triggerAt, pending)
        } else {
            alarms.setAndAllowWhileIdle(AlarmManager.RTC_WAKEUP, triggerAt, pending)
        }
        return useExact
    }

    private fun cancel(context: Context, stableKey: String) {
        val pending = pendingIntentForKey(
            context,
            stableKey,
            PendingIntent.FLAG_NO_CREATE,
        ) ?: return
        context.getSystemService(AlarmManager::class.java).cancel(pending)
        pending.cancel()
    }

    private fun pendingIntent(
        context: Context,
        reminder: OrgReminderSpec,
        extraFlags: Int,
    ): PendingIntent = requireNotNull(
        PendingIntent.getBroadcast(
            context,
            requestCode(reminder.stableKey),
            Intent(context, OrgReminderReceiver::class.java)
                .setAction(ACTION_FIRE)
                .setData(reminderUri(reminder.stableKey))
                .putExtra("title", reminder.title)
                .putExtra("kind", reminder.kind.name)
                .putExtra("path", reminder.taskPath)
                .putExtra("line", reminder.taskLine),
            PendingIntent.FLAG_IMMUTABLE or extraFlags,
        ),
    )

    private fun pendingIntentForKey(
        context: Context,
        stableKey: String,
        extraFlags: Int,
    ): PendingIntent? = PendingIntent.getBroadcast(
        context,
        requestCode(stableKey),
        Intent(context, OrgReminderReceiver::class.java)
            .setAction(ACTION_FIRE)
            .setData(reminderUri(stableKey)),
        PendingIntent.FLAG_IMMUTABLE or extraFlags,
    )

    private fun requestCode(stableKey: String): Int = stableKey.hashCode() and Int.MAX_VALUE

    private fun reminderUri(stableKey: String): Uri =
        Uri.parse("zara-org-reminder://alarm/${Uri.encode(stableKey)}")
}

class OrgReminderReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        ReminderNotifications.show(
            context = context,
            title = intent.getStringExtra("title").orEmpty().ifBlank { "Org reminder" },
            kind = intent.getStringExtra("kind").orEmpty(),
            path = intent.getStringExtra("path").orEmpty(),
            line = intent.getIntExtra("line", 0),
            notificationId = intent.dataString.orEmpty().hashCode() and Int.MAX_VALUE,
        )
    }
}

class OrgReminderBootReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        OrgReminderScheduler.reconcile(context)
    }
}

private object ReminderNotifications {
    private const val CHANNEL_ID = "org-reminders"

    fun show(
        context: Context,
        title: String,
        kind: String,
        path: String,
        line: Int,
        notificationId: Int,
    ) {
        if (
            Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU &&
            context.checkSelfPermission(android.Manifest.permission.POST_NOTIFICATIONS) !=
            PackageManager.PERMISSION_GRANTED
        ) {
            return
        }

        val manager = context.getSystemService(NotificationManager::class.java)
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            manager.createNotificationChannel(
                NotificationChannel(
                    CHANNEL_ID,
                    "Org reminders",
                    NotificationManager.IMPORTANCE_HIGH,
                ),
            )
        }

        val open = PendingIntent.getActivity(
            context,
            notificationId,
            Intent(context, MainActivity::class.java),
            PendingIntent.FLAG_IMMUTABLE or PendingIntent.FLAG_UPDATE_CURRENT,
        )

        val detail = buildString {
            if (kind.isNotBlank()) append(kind.lowercase().replaceFirstChar(Char::uppercaseChar))
            if (path.isNotBlank()) {
                if (isNotEmpty()) append(" · ")
                append(path)
                if (line > 0) append(':').append(line)
            }
        }

        manager.notify(
            notificationId,
            NotificationCompat.Builder(context, CHANNEL_ID)
                .setSmallIcon(android.R.drawable.ic_dialog_info)
                .setContentTitle(title)
                .setContentText(detail)
                .setPriority(NotificationCompat.PRIORITY_HIGH)
                .setAutoCancel(true)
                .setContentIntent(open)
                .build(),
        )
    }
}
