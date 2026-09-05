package ai.zara.app.assistant

import ai.zara.app.AndroidAppSession
import ai.zara.app.ZaraApplication
import ai.zara.app.voice.ManualVoiceState
import android.Manifest
import android.content.Context
import android.content.pm.PackageManager
import android.os.Bundle
import android.service.voice.VoiceInteractionSession
import androidx.core.content.ContextCompat

class ZaraVoiceInteractionSession(
    private val context: Context,
) : VoiceInteractionSession(context) {
    private val appSession: AndroidAppSession =
        (context.applicationContext as ZaraApplication).appSession
    private val invocationGate = AssistantInvocationGate()

    override fun onShow(args: Bundle?, showFlags: Int) {
        super.onShow(args, showFlags)
        if (!invocationGate.beginShow()) return

        val permissionGranted =
            ContextCompat.checkSelfPermission(context, Manifest.permission.RECORD_AUDIO) ==
                PackageManager.PERMISSION_GRANTED
        appSession.startAssistantVoice(permissionGranted).whenComplete { _, error ->
            if (error != null) {
                invocationGate.endShow()
                context.mainExecutor.execute { finish() }
            } else if (invocationGate.shouldCancelLateStart()) {
                cancelCaptureIfActive()
            }
        }
    }

    override fun onHide() {
        invocationGate.endShow()
        cancelCaptureIfActive()
        super.onHide()
    }

    override fun onDestroy() {
        invocationGate.endShow()
        cancelCaptureIfActive()
        super.onDestroy()
    }

    private fun cancelCaptureIfActive() {
        if (
            appSession.voiceState() is ManualVoiceState.Capturing &&
            invocationGate.claimCancellation()
        ) {
            appSession.cancelPushToTalk()
        }
    }
}
