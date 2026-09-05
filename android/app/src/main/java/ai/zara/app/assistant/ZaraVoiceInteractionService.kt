package ai.zara.app.assistant

import ai.zara.app.AndroidAppSession
import ai.zara.app.ZaraApplication
import ai.zara.app.voice.ManualVoiceState
import android.service.voice.VoiceInteractionService

class ZaraVoiceInteractionService : VoiceInteractionService() {
    private val zaraApplication: ZaraApplication
        get() = application as ZaraApplication

    private val appSession: AndroidAppSession
        get() = zaraApplication.appSession

    override fun onReady() {
        super.onReady()
        appSession.assessAssistantRole()
    }

    override fun onShutdown() {
        zaraApplication.assistantLifecycleFence.invalidate()
        if (appSession.voiceState() is ManualVoiceState.Capturing) {
            appSession.cancelPushToTalk()
        }
        appSession.assessAssistantRole()
        super.onShutdown()
    }
}
