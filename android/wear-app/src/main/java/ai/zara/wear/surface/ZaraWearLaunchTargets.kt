package ai.zara.wear.surface

import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import android.net.Uri

internal object ZaraWearLaunchTargets {
    const val VOICE_ACTION = "ai.zara.action.WEAR_VOICE"
    const val OPEN_ORG_TODO_ACTION = "ai.zara.action.OPEN_ORG_TODO"
    const val EXTRA_ORG_TODO_ID = "ai.zara.extra.ORG_TODO_ID"
    private const val MAIN_PACKAGE = "ai.zara.app"
    private const val MAIN_ACTIVITY = "ai.zara.wear.WearMainActivity"
    private const val VOICE_PACKAGE = "ai.zara.wear.voice"
    private const val VOICE_ACTIVITY = "ai.zara.wear.voice.WearVoiceActivity"

    fun mainPendingIntent(context: Context): PendingIntent = PendingIntent.getActivity(
        context,
        4100,
        Intent().setClassName(MAIN_PACKAGE, MAIN_ACTIVITY),
        PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE,
    )

    fun voicePendingIntent(context: Context): PendingIntent = PendingIntent.getActivity(
        context,
        4101,
        Intent(VOICE_ACTION).setClassName(VOICE_PACKAGE, VOICE_ACTIVITY),
        PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE,
    )

    fun orgTodoPendingIntent(context: Context, todoId: String): PendingIntent {
        require(todoId.isNotBlank()) { "Org todo id must not be blank" }
        val identity = Uri.Builder()
            .scheme("zara")
            .authority("org-todo")
            .appendPath(todoId)
            .build()
        val requestCode = 4200 + (todoId.hashCode() and 0x3ff)
        val intent = Intent(OPEN_ORG_TODO_ACTION)
            .setClassName(MAIN_PACKAGE, MAIN_ACTIVITY)
            .setData(identity)
            .putExtra(EXTRA_ORG_TODO_ID, todoId)
        return PendingIntent.getActivity(
            context,
            requestCode,
            intent,
            PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE,
        )
    }

    fun mainComponent(): Pair<String, String> = MAIN_PACKAGE to MAIN_ACTIVITY

    fun voiceComponent(): Pair<String, String> = VOICE_PACKAGE to VOICE_ACTIVITY
}
