package ai.zara.app.watch

import android.content.Context
import android.net.nsd.NsdManager
import android.net.nsd.NsdServiceInfo
import android.os.Handler
import android.os.Looper
import java.util.concurrent.ConcurrentHashMap

class WatchDebugDiscovery(
    context: Context,
) {
    private val nsd = context.applicationContext.getSystemService(Context.NSD_SERVICE) as NsdManager
    private val handler = Handler(Looper.getMainLooper())

    fun scan(
        durationMillis: Long = 5_000,
        onUpdate: (List<WatchDebugEndpoint>) -> Unit,
        onDone: () -> Unit,
    ) {
        val found = ConcurrentHashMap<String, WatchDebugEndpoint>()
        val listeners = mutableListOf<NsdManager.DiscoveryListener>()

        fun publish() {
            onUpdate(found.values.sortedWith(compareBy({ it.host }, { it.port })))
        }

        SERVICE_TYPES.forEach { (serviceType, pairing) ->
            val listener = object : NsdManager.DiscoveryListener {
                override fun onDiscoveryStarted(serviceType: String) = Unit
                override fun onDiscoveryStopped(serviceType: String) = Unit
                override fun onStartDiscoveryFailed(serviceType: String, errorCode: Int) = Unit
                override fun onStopDiscoveryFailed(serviceType: String, errorCode: Int) = Unit

                override fun onServiceFound(serviceInfo: NsdServiceInfo) {
                    @Suppress("DEPRECATION")
                    nsd.resolveService(serviceInfo, object : NsdManager.ResolveListener {
                        override fun onResolveFailed(serviceInfo: NsdServiceInfo, errorCode: Int) = Unit

                        override fun onServiceResolved(resolved: NsdServiceInfo) {
                            @Suppress("DEPRECATION")
                            val host = resolved.host?.hostAddress ?: return
                            val endpoint = WatchDebugEndpoint(
                                host = host,
                                port = resolved.port,
                                pairing = pairing,
                                serviceName = resolved.serviceName,
                            )
                            found["${endpoint.pairing}:${endpoint.serviceName}"] = endpoint
                            publish()
                        }
                    })
                }

                override fun onServiceLost(serviceInfo: NsdServiceInfo) {
                    found.entries.removeIf { it.value.serviceName == serviceInfo.serviceName }
                    publish()
                }
            }
            listeners += listener
            runCatching {
                nsd.discoverServices(serviceType, NsdManager.PROTOCOL_DNS_SD, listener)
            }
        }

        handler.postDelayed({
            listeners.forEach { listener -> runCatching { nsd.stopServiceDiscovery(listener) } }
            onDone()
        }, durationMillis)
    }

    companion object {
        private val SERVICE_TYPES = listOf(
            "_adb-tls-pairing._tcp" to true,
            "_adb-tls-connect._tcp" to false,
        )
    }
}
