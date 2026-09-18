package ai.zara.app

import ai.zara.app.assistant.AssistantLifecycleFence
import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.samsunghealth.SamsungHealthAndroidPlugin
import ai.zara.app.samsunghealth.SamsungHealthPrologPlugin
import ai.zara.app.update.AndroidUpdateManager
import android.app.Application
import java.io.File

class ZaraApplication : Application() {
    internal val assistantLifecycleFence = AssistantLifecycleFence()

    val appSession: AndroidAppSession by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        SamsungHealthPrologPlugin.install(
            PrologWorkspace(File(filesDir, "prolog-workspace")),
        )
        AndroidAppSession(this)
    }

    val samsungHealthPlugin: SamsungHealthAndroidPlugin by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        SamsungHealthAndroidPlugin.create(
            context = this,
            queryProlog = appSession::queryLocalProlog,
        )
    }

    val updateManager: AndroidUpdateManager by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        AndroidUpdateManager(
            context = this,
            currentVersion = BuildConfig.VERSION_NAME,
            currentSourceSha = BuildConfig.SOURCE_SHA,
        )
    }
}