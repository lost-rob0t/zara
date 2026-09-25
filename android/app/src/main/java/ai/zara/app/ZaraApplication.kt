package ai.zara.app

import ai.zara.app.assistant.AssistantLifecycleFence
import ai.zara.app.expert.CanonicalExpertInvocationPort
import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.samsunghealth.SamsungHealthAndroidPlugin
import ai.zara.app.samsunghealth.SamsungHealthPrologPlugin
import ai.zara.app.update.AndroidUpdateManager
import android.app.Application
import java.io.File

class ZaraApplication : Application() {
    internal val assistantLifecycleFence = AssistantLifecycleFence()

    /**
     * Consumer-side composition slot for the Core-owned canonical expert invocation authority.
     *
     * The release graph does not yet provide that owner to Android, so absence is explicit here
     * and natural expert turns remain fail-closed. This slot must be bound to the existing Core
     * owner; Android must never construct a registry, activation issuer, or executor for it.
     */
    private val canonicalExpertInvocationPortProvider: () -> CanonicalExpertInvocationPort? = {
        null
    }

    val appSession: AndroidAppSession by lazy(LazyThreadSafetyMode.SYNCHRONIZED) {
        SamsungHealthPrologPlugin.install(
            PrologWorkspace(File(filesDir, "prolog-workspace")),
        )
        AndroidAppSession(
            context = this,
            canonicalExpertInvocationPortProvider = canonicalExpertInvocationPortProvider,
        )
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