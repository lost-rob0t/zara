package ai.zara.wear.surface

import android.app.PendingIntent
import android.content.Context
import android.content.Intent

internal object ZaraWearLaunchTargets {
    const val VOICE_ACTION = "ai.zara.action.WEAR_VOICE"
    private const val MAIN_PACKAGE = "ai.zara.wear"
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

    fun mainComponent(): Pair<String, String> = MAIN_PACKAGE to MAIN_ACTIVITY

    fun voiceComponent(): Pair<String, String> = VOICE_PACKAGE to VOICE_ACTIVITY
}
