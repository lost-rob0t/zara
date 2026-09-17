package ai.zara.app.device

import android.app.SearchManager
import android.content.ActivityNotFoundException
import android.content.Context
import android.content.Intent
import android.net.Uri

interface AppSearchLauncher {
    fun isAvailable(alias: String): Boolean
    fun search(alias: String, query: String)
}

class AndroidAppSearchLauncher(context: Context) : AppSearchLauncher {
    private val appContext = context.applicationContext
    private val packageManager = appContext.packageManager

    override fun isAvailable(alias: String): Boolean =
        AndroidAppAliases.packageCandidates(alias).any { packageName ->
            packageManager.getLaunchIntentForPackage(packageName) != null
        }

    override fun search(alias: String, query: String) {
        val packageName = AndroidAppAliases.packageCandidates(alias)
            .firstOrNull { packageManager.getLaunchIntentForPackage(it) != null }
            ?: throw ActivityNotFoundException("reviewed app alias is unavailable")

        val search = Intent(Intent.ACTION_SEARCH).apply {
            setPackage(packageName)
            putExtra(SearchManager.QUERY, query)
            addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        }
        if (search.resolveActivity(packageManager) != null) {
            appContext.startActivity(search)
            return
        }

        val fallback = Intent(
            Intent.ACTION_VIEW,
            Uri.parse("https://www.youtube.com/results?search_query=${Uri.encode(query)}"),
        ).apply {
            setPackage(packageName)
            addCategory(Intent.CATEGORY_BROWSABLE)
            addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        }
        if (fallback.resolveActivity(packageManager) == null) {
            throw ActivityNotFoundException("reviewed app does not expose a search surface")
        }
        appContext.startActivity(fallback)
    }
}
