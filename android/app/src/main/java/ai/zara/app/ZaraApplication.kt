package ai.zara.app

import ai.zara.app.assistant.AssistantLifecycleFence
import ai.zara.app.device.AndroidRawAdapter
import ai.zara.app.device.DeviceCapabilityExtensions
import ai.zara.app.integration.AndroidIntegrationRuntime
import ai.zara.app.update.AndroidUpdateManager
import android.app.Application
import rikka.shizuku.ShizukuProvider
import rikka.sui.Sui

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

    override fun onCreate() {
        super.onCreate()
        if (!suiAvailable && !providerProcess) {
            ShizukuProvider.requestBinderForNonProviderProcess(this)
        }
        DeviceCapabilityExtensions.installAndroidRaw(AndroidRawAdapter(androidIntegration))
    }

    companion object {
        private val providerProcess = Application.getProcessName() == BuildConfig.APPLICATION_ID
        private val suiAvailable = Sui.init(BuildConfig.APPLICATION_ID)

        init {
            if (!suiAvailable) {
                ShizukuProvider.enableMultiProcessSupport(providerProcess)
            }
        }
    }
}
