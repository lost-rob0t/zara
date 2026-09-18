package ai.zara.org.timer

import ai.zara.org.core.OrgTimerTemplate
import android.app.AlarmManager
import android.app.Notification
import android.app.NotificationChannel
import android.app.NotificationManager
import android.app.PendingIntent
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent
import android.content.pm.PackageManager
import android.net.Uri
import android.os.Build
import org.json.JSONObject

data class RunningOrgTimer(
    val id: Int,
    val name: String,
    val sourceKey: String,
    val finishEpochMs: Long,
    val remainingMs: Long,
    val paused: Boolean,
)

object OrgTimerRuntime {
    private const val PREFS = "org-timers"
    private const val KEY_IDS = "active-ids"
    private const val ACTION_FINISH = "ai.zara.org.timer.FINISH"

    fun start(context: Context, template: OrgTimerTemplate): RunningOrgTimer {
        val id = requestCode(template.stableKey)
        val durationMs = template.duration.toMillis()
        require(durationMs > 0L) { "Timer duration must be positive" }
        val timer = RunningOrgTimer(
            id = id,
            name = template.name,
            sourceKey = template.stableKey,
            finishEpochMs = System.currentTimeMillis() + durationMs,
            remainingMs = durationMs,
            paused = false,
        )
        persist(context, timer)
        schedule(context, timer)
        return timer
    }

    fun pause(context: Context, id: Int): RunningOrgTimer? {
        val timer = load(context, id) ?: return null
        if (timer.paused) return timer
        val remaining = (timer.finishEpochMs - System.currentTimeMillis()).coerceAtLeast(0L)
        cancelAlarm(context, id)
        val paused = timer.copy(
            finishEpochMs = 0L,
            remainingMs = remaining,
            paused = true,
        )
        persist(context, paused)
        return paused
    }

    fun resume(context: Context, id: Int): RunningOrgTimer? {
        val timer = load(context, id) ?: return null
        if (!timer.paused) return timer
        if (timer.remainingMs <= 0L) {
            consume(context, id)
            return null
        }
        val resumed = timer.copy(
            finishEpochMs = System.currentTimeMillis() + timer.remainingMs,
            paused = false,
        )
        persist(context, resumed)
        schedule(context, resumed)
        return resumed
    }

    fun cancel(context: Context, id: Int): Boolean {
        val existed = load(context, id) != null
        cancelAlarm(context, id)
        remove(context, id)
        return existed
    }

    fun active(context: Context): List<RunningOrgTimer> =
        ids(context).mapNotNull { load(context, it) }.sortedBy { timer ->
            if (timer.paused) Long.MAX_VALUE else timer.finishEpochMs
        }

    fun restore(context: Context) {
        active(context).forEach { timer ->
            when {
                timer.paused -> Unit
                timer.finishEpochMs > System.currentTimeMillis() -> schedule(context, timer)
                else -> finish(context, timer.id)
            }
        }
    }

    fun exactAccess(context: Context): Boolean =
        Build.VERSION.SDK_INT < Build.VERSION_CODES.S ||
            context.getSystemService(AlarmManager::class.java).canScheduleExactAlarms()

    fun finish(context: Context, id: Int) {
        val timer = load(context, id) ?: return
        remove(context, id)
        TimerNotifications.show(context, timer)
    }

    private fun schedule(context: Context, timer: RunningOrgTimer) {
        if (timer.paused || timer.finishEpochMs <= 0L) return
        val alarms = context.getSystemService(AlarmManager::class.java)
        val pending = finishIntent(context, timer, PendingIntent.FLAG_UPDATE_CURRENT)
        if (exactAccess(context)) {
            alarms.setExactAndAllowWhileIdle(
                AlarmManager.RTC_WAKEUP,
                timer.finishEpochMs,
                pending,
            )
        } else {
            alarms.setAndAllowWhileIdle(
                AlarmManager.RTC_WAKEUP,
                timer.finishEpochMs,
                pending,
            )
        }
    }

    private fun cancelAlarm(context: Context, id: Int) {
        val pending = PendingIntent.getBroadcast(
            context,
            id,
            Intent(context, OrgTimerReceiver::class.java)
                .setAction(ACTION_FINISH)
                .setData(timerUri(id)),
            PendingIntent.FLAG_IMMUTABLE or PendingIntent.FLAG_NO_CREATE,
        ) ?: return
        context.getSystemService(AlarmManager::class.java).cancel(pending)
        pending.cancel()
    }

    private fun finishIntent(
        context: Context,
        timer: RunningOrgTimer,
        extraFlags: Int,
    ): PendingIntent = requireNotNull(
        PendingIntent.getBroadcast(
            context,
            timer.id,
            Intent(context, OrgTimerReceiver::class.java)
                .setAction(ACTION_FINISH)
                .setData(timerUri(timer.id))
                .putExtra("timer_id", timer.id),
            PendingIntent.FLAG_IMMUTABLE or extraFlags,
        ),
    )

    private fun persist(context: Context, timer: RunningOrgTimer) {
        val prefs = context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
        val current = prefs.getStringSet(KEY_IDS, emptySet()).orEmpty().toMutableSet()
        current += timer.id.toString()
        prefs.edit()
            .putStringSet(KEY_IDS, current)
            .putString(key(timer.id), encode(timer))
            .apply()
    }

    private fun load(context: Context, id: Int): RunningOrgTimer? =
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .getString(key(id), null)
            ?.let(::decode)

    private fun remove(context: Context, id: Int) {
        val prefs = context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
        val current = prefs.getStringSet(KEY_IDS, emptySet()).orEmpty().toMutableSet()
        current -= id.toString()
        prefs.edit()
            .putStringSet(KEY_IDS, current)
            .remove(key(id))
            .apply()
    }

    private fun ids(context: Context): Set<Int> =
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .getStringSet(KEY_IDS, emptySet())
            .orEmpty()
            .mapNotNullTo(linkedSetOf()) { it.toIntOrNull() }

    private fun key(id: Int): String = "timer.$id"

    private fun encode(timer: RunningOrgTimer): String = JSONObject()
        .put("id", timer.id)
        .put("name", timer.name)
        .put("sourceKey", timer.sourceKey)
        .put("finishEpochMs", timer.finishEpochMs)
        .put("remainingMs", timer.remainingMs)
        .put("paused", timer.paused)
        .toString()

    private fun decode(raw: String): RunningOrgTimer? = runCatching {
        val json = JSONObject(raw)
        RunningOrgTimer(
            id = json.getInt("id"),
            name = json.getString("name"),
            sourceKey = json.getString("sourceKey"),
            finishEpochMs = json.getLong("finishEpochMs"),
            remainingMs = json.getLong("remainingMs"),
            paused = json.getBoolean("paused"),
        )
    }.getOrNull()

    private fun requestCode(sourceKey: String): Int = sourceKey.hashCode() and Int.MAX_VALUE

    private fun timerUri(id: Int): Uri = Uri.parse("zara-org-timer://finish/$id")
}

class OrgTimerReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        OrgTimerRuntime.finish(context, intent.getIntExtra("timer_id", -1))
    }
}

class OrgTimerBootReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        OrgTimerRuntime.restore(context)
    }
}

private object TimerNotifications {
    private const val CHANNEL_ID = "org-timers"

    fun show(context: Context, timer: RunningOrgTimer) {
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
                    "Org timers",
                    NotificationManager.IMPORTANCE_HIGH,
                ),
            )
        }

        val open = PendingIntent.getActivity(
            context,
            timer.id,
            Intent(context, MainActivity::class.java),
            PendingIntent.FLAG_IMMUTABLE or PendingIntent.FLAG_UPDATE_CURRENT,
        )

        val builder = if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            Notification.Builder(context, CHANNEL_ID)
        } else {
            @Suppress("DEPRECATION")
            Notification.Builder(context)
        }

        manager.notify(
            timer.id,
            builder
                .setSmallIcon(android.R.drawable.ic_lock_idle_alarm)
                .setContentTitle(timer.name)
                .setContentText("Org timer finished")
                .setAutoCancel(true)
                .setContentIntent(open)
                .build(),
        )
    }
}
