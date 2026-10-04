package ai.zara.wear

import android.content.Context
import android.os.Handler
import android.os.Looper
import com.google.android.gms.wearable.CapabilityClient
import com.google.android.gms.wearable.CapabilityInfo
import com.google.android.gms.wearable.MessageClient
import com.google.android.gms.wearable.Wearable
import ai.zara.ui.continuity.WearCompanionContract

/**
 * Process-wide shell around [WearCompanionClient]: discovers the paired phone
 * over the Wear Data Layer, sends the auto-pairing hello, applies provision
 * payloads, and keeps one truthful link state for every Wear surface.
 */
class WearCompanionRuntime(
    context: Context,
    private val handler: Handler = Handler(Looper.getMainLooper()),
    private val requestTimeoutMs: Long = DEFAULT_REQUEST_TIMEOUT_MS,
) {
    private val appContext = context.applicationContext
    private val capabilityClient = Wearable.getCapabilityClient(appContext)
    private val messageClient = Wearable.getMessageClient(appContext)

    private val lock = Any()
    private var stateValue: WearCompanionLinkState = WearCompanionClient.initial()
    private val observers = mutableSetOf<(WearCompanionLinkState) -> Unit>()
    private var started = false
    private var pendingTimeout = false

    private val capabilityListener = CapabilityClient.OnCapabilityChangedListener { info ->
        onCapabilityChanged(info)
    }

    fun start() {
        synchronized(lock) {
            if (started) return
            started = true
        }
        runCatching {
            capabilityClient.addListener(capabilityListener, WearCompanionContract.CAPABILITY_PHONE)
        }
        capabilityClient
            .getCapability(WearCompanionContract.CAPABILITY_PHONE, CapabilityClient.FILTER_REACHABLE)
            .addOnSuccessListener { info -> onCapabilityChanged(info) }
    }

    fun stop() {
        synchronized(lock) {
            if (!started) return
            started = false
            handler.removeCallbacks(timeoutRunnable)
        }
        runCatching { capabilityClient.removeListener(capabilityListener) }
    }

    fun state(): WearCompanionLinkState = synchronized(lock) { stateValue }

    fun observe(callback: ((WearCompanionLinkState) -> Unit)?) {
        synchronized(lock) {
            observers.clear()
            if (callback != null) observers.add(callback)
            callback?.invoke(stateValue)
        }
    }

    fun onProvisionReceived(phoneNodeId: String, payload: ByteArray) {
        apply(WearCompanionClient.onProvision(state(), phoneNodeId, payload))
    }

    private fun onCapabilityChanged(info: CapabilityInfo) {
        val reachable = info.nodes.toList()
        if (reachable.isEmpty()) {
            (state() as? WearCompanionLinkState.Paired)?.let { paired ->
                apply(WearCompanionClient.onPhoneReachabilityLost(state(), paired.phoneNodeId))
            }
            return
        }
        val current = state()
        val paired = current as? WearCompanionLinkState.Paired
        val target = reachable.firstOrNull { it.id == paired?.phoneNodeId } ?: reachable.first()
        apply(WearCompanionClient.onPhoneReachable(current, target.id))
        requestProvision(target.id)
    }

    private fun requestProvision(phoneNodeId: String) {
        applyRequesting(phoneNodeId)
        messageClient
            .sendMessage(phoneNodeId, WearCompanionContract.PATH_WATCH_HELLO, ByteArray(0))
        synchronized(lock) {
            if (stateValue !is WearCompanionLinkState.Paired) {
                pendingTimeout = true
                handler.postDelayed(timeoutRunnable, requestTimeoutMs)
            }
        }
    }

    private fun applyRequesting(phoneNodeId: String) {
        val current = state()
        if (current is WearCompanionLinkState.Paired) return
        apply(WearCompanionClient.onPhoneReachable(current, phoneNodeId))
    }

    private val timeoutRunnable = Runnable {
        val stillRequesting =
            synchronized(lock) {
                val requesting = stateValue is WearCompanionLinkState.RequestingProvision
                if (requesting) pendingTimeout = false
                requesting
            }
        if (stillRequesting) {
            apply(WearCompanionClient.onRequestTimeout(state()))
        }
    }

    private fun apply(next: WearCompanionLinkState) {
        val listeners: List<(WearCompanionLinkState) -> Unit>
        synchronized(lock) {
            if (next == stateValue) return
            stateValue = next
            if (next !is WearCompanionLinkState.RequestingProvision && pendingTimeout) {
                pendingTimeout = false
                handler.removeCallbacks(timeoutRunnable)
            }
            listeners = observers.toList()
        }
        listeners.forEach { it(next) }
    }

    companion object {
        const val DEFAULT_REQUEST_TIMEOUT_MS = 15_000L

        @Volatile
        private var defaultRuntime: WearCompanionRuntime? = null

        fun get(context: Context): WearCompanionRuntime =
            defaultRuntime ?: WearCompanionRuntime(context.applicationContext).also {
                defaultRuntime = it
            }
    }
}
