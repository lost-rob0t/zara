package ai.zara.llmserve

import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent

class LlmServeBootReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        if (intent.action != Intent.ACTION_BOOT_COMPLETED) return
        val preferences = context.getSharedPreferences(LlmServeService.PREFS, Context.MODE_PRIVATE)
        if (!preferences.getBoolean(LlmServeService.KEY_RESUME_ON_BOOT, false)) return
        try {
            context.startForegroundService(Intent(context, LlmServeService::class.java).apply {
                action = LlmServeService.ACTION_RESUME
            })
        } catch (_: Exception) {
            preferences.edit().putString(LlmServeService.KEY_STATUS,
                "boot_start_failed: open Zara LLM Serve and start the server").apply()
        }
    }
}
