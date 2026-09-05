package ai.zara.app.device

import android.content.ActivityNotFoundException
import android.content.Context
import android.content.Intent
import android.net.Uri

class AndroidUriLauncher(
    context: Context,
) : UriLauncher {
    private val appContext = context.applicationContext

    override fun isAvailable(): Boolean =
        viewIntent("https://example.com/").resolveActivity(appContext.packageManager) != null

    override fun open(uri: String) {
        val intent = viewIntent(OpenUriPolicy.normalize(uri))
        if (intent.resolveActivity(appContext.packageManager) == null) {
            throw ActivityNotFoundException("no handler for reviewed URI")
        }
        appContext.startActivity(intent)
    }

    private fun viewIntent(uri: String): Intent =
        Intent(Intent.ACTION_VIEW, Uri.parse(uri)).apply {
            addCategory(Intent.CATEGORY_BROWSABLE)
            addFlags(Intent.FLAG_ACTIVITY_NEW_TASK)
        }
}
