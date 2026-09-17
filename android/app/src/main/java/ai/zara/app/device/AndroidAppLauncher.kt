package ai.zara.app.device

import android.content.ActivityNotFoundException
import android.content.Context
import android.content.Intent

class AndroidAppLauncher(context: Context) : AppLauncher {
    private val appContext = context.applicationContext
    private val packageManager = appContext.packageManager

    override fun isAvailable(alias: String): Boolean =
        AndroidAppAliases.packageCandidates(alias).any { packageName ->
            packageManager.getLaunchIntentForPackage(packageName) != null
        }

    override fun launch(alias: String) {
        val intent = AndroidAppAliases.packageCandidates(alias)
            .asSequence()
            .mapNotNull(packageManager::getLaunchIntentForPackage)
            .firstOrNull()
            ?: throw ActivityNotFoundException("reviewed app alias is unavailable")
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        appContext.startActivity(intent)
    }
}
