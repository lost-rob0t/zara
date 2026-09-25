package ai.zara.wear

import com.google.android.gms.wearable.MessageEvent
import com.google.android.gms.wearable.WearableListenerService
import ai.zara.ui.continuity.WearCompanionContract

/** Feeds phone provisions into the shared [WearCompanionRuntime] and keeps it started. */
class WearCompanionLinkService : WearableListenerService() {
    override fun onMessageReceived(event: MessageEvent) {
        if (event.path != WearCompanionContract.PATH_PHONE_PROVISION) return
        WearCompanionRuntime.get(this).apply {
            start()
            onProvisionReceived(event.sourceNodeId, event.data)
        }
    }
}
