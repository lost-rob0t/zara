package ai.zara.app

import ai.zara.app.assistant.AssistantLifecycleFence
import ai.zara.app.update.AndroidUpdateManager
import android.app.Application

class ZaraApplication : Application() {
    internal val assistantLifecycleFence = AssistantLifecycleFence()

    val appSession: AndroidAppSession by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        AndroidAppSession(this)
    }

    val updateManager: AndroidUpdateManager by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        AndroidUpdateManager(this, BuildConfig.VERSION_NAME)
    }
}
