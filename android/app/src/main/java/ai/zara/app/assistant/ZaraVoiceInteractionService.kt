package ai.zara.app.assistant

import ai.zara.app.AndroidAppSession
import ai.zara.app.ZaraApplication
import ai.zara.app.voice.ManualVoiceState
import android.service.voice.VoiceInteractionService

class ZaraVoiceInteractionService : VoiceInteractionService() {
    private val appSession: AndroidAppSession
        get() = (application as ZaraApplication).appSession

    override fun onReady() {
        super.onReady()
        appSession.assessAssistantRole()
    }

    override fun onShutdown() {
        if (appSession.voiceState() is ManualVoiceState.Capturing) {
            appSession.cancelPushToTalk()
        }
        appSession.assessAssistantRole()
        super.onShutdown()
    }
}
