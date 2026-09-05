package ai.zara.app.device

import android.content.ActivityNotFoundException
import android.content.Context
import android.content.Intent

class AndroidAppLauncher(context: Context) : AppLauncher {
    private val appContext = context.applicationContext
    private val packageManager = appContext.packageManager

    override fun isAvailable(alias: String): Boolean =
        packageCandidates(alias).any { packageName ->
            packageManager.getLaunchIntentForPackage(packageName) != null
        }

    override fun launch(alias: String) {
        val intent = packageCandidates(alias)
            .asSequence()
            .mapNotNull(packageManager::getLaunchIntentForPackage)
            .firstOrNull()
            ?: throw ActivityNotFoundException("reviewed app alias is unavailable")
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        appContext.startActivity(intent)
    }

    private fun packageCandidates(alias: String): List<String> = when (alias) {
        "browser" -> BROWSER_PACKAGES
        "youtube" -> YOUTUBE_PACKAGES
        else -> emptyList()
    }

    private companion object {
        val BROWSER_PACKAGES = listOf(
            "com.sec.android.app.sbrowser",
            "com.android.chrome",
            "com.brave.browser",
            "org.mozilla.firefox",
        )
        val YOUTUBE_PACKAGES = listOf("com.google.android.youtube")
    }
}
