package ai.zara.app

import ai.zara.app.assistant.AssistantLifecycleFence
import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.smartthings.SmartThingsAndroidPlugin
import ai.zara.app.smartthings.SmartThingsPrologPlugin
import ai.zara.app.update.AndroidUpdateManager
import android.app.Application
import java.io.File

class ZaraApplication : Application() {
    internal val assistantLifecycleFence = AssistantLifecycleFence()

    val appSession: AndroidAppSession by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        SmartThingsPrologPlugin.install(
            PrologWorkspace(File(filesDir, "prolog-workspace")),
        )
        AndroidAppSession(this)
    }

    val smartThingsPlugin: SmartThingsAndroidPlugin by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        SmartThingsAndroidPlugin.create(
            context = this,
            queryProlog = appSession::queryLocalProlog,
        )
    }

    val updateManager: AndroidUpdateManager by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        AndroidUpdateManager(this, BuildConfig.VERSION_NAME)
    }
}
