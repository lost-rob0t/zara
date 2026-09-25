package ai.zara.app.watch

import android.os.Build
import com.google.android.gms.wearable.MessageEvent
import com.google.android.gms.wearable.Wearable
import com.google.android.gms.wearable.WearableListenerService
import ai.zara.app.history.PortableConversationStore
import ai.zara.app.history.latestSymbolicEdgeSnapshot
import ai.zara.ui.continuity.WearCompanionContract

/**
 * Answers the paired watch's auto-pairing hello with the current canonical
 * provision. The payload carries only the phone display name and the latest
 * pure-symbolic conversation projection; enrollment material and credentials
 * never leave the phone.
 */
class WearCompanionLinkService : WearableListenerService() {
    private val provisionSource by lazy {
        WearCompanionProvisionSource(
            phoneName = { Build.MODEL ?: "Zara phone" },
            latestSnapshot = {
                val store = PortableConversationStore(this)
                try {
                    store.latestSymbolicEdgeSnapshot()
                } finally {
                    store.close()
                }
            },
        )
    }

    override fun onMessageReceived(event: MessageEvent) {
        if (event.path != WearCompanionContract.PATH_WATCH_HELLO) return
        val payload = provisionSource.encodeProvision() ?: return
        Wearable.getMessageClient(this)
            .sendMessage(event.sourceNodeId, WearCompanionContract.PATH_PHONE_PROVISION, payload)
    }
}
