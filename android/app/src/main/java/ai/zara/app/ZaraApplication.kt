package ai.zara.app

import ai.zara.app.assistant.AssistantLifecycleFence
import ai.zara.app.integration.AndroidIntegrationRuntime
import ai.zara.app.update.AndroidUpdateManager
import android.app.Application

class ZaraApplication : Application() {
    internal val assistantLifecycleFence = AssistantLifecycleFence()

    val androidIntegration: AndroidIntegrationRuntime by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        AndroidIntegrationRuntime(this)
    }

    val appSession: AndroidAppSession by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        AndroidAppSession(this)
    }

    val updateManager: AndroidUpdateManager by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        AndroidUpdateManager(this, BuildConfig.VERSION_NAME)
    }
}
