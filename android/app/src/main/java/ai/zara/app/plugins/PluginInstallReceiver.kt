package ai.zara.app.plugins

import ai.zara.app.ZaraApplication
import android.content.BroadcastReceiver
import android.content.Context
import android.content.Intent

class PluginInstallReceiver : BroadcastReceiver() {
    override fun onReceive(context: Context, intent: Intent) {
        val installer = (context.applicationContext as ZaraApplication).pluginInstaller
        val confirmation = installer.receiveResult(intent) ?: return
        try {
            context.startActivity(confirmation.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK))
        } catch (_: RuntimeException) {
            installer.confirmationFailed(intent)
        }
    }
}
