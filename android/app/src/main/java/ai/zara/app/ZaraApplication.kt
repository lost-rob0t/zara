package ai.zara.app

import ai.zara.app.assistant.AssistantLifecycleFence
import android.app.Application

class ZaraApplication : Application() {
    internal val assistantLifecycleFence = AssistantLifecycleFence()

    val appSession: AndroidAppSession by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        AndroidAppSession(this)
    }
}
