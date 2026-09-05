package ai.zara.app.assistant

import ai.zara.app.AndroidAppSession
import ai.zara.app.ZaraApplication
import android.Manifest
import android.content.Context
import android.content.pm.PackageManager
import android.os.Bundle
import android.service.voice.VoiceInteractionSession
import android.view.Gravity
import android.view.MotionEvent
import android.view.View
import android.widget.Button
import android.widget.LinearLayout
import android.widget.TextView
import androidx.core.content.ContextCompat

class ZaraVoiceInteractionSession(
    private val context: Context,
) : VoiceInteractionSession(context) {
    private val application = context.applicationContext as ZaraApplication
    private val appSession: AndroidAppSession = application.appSession
    private val lifecycleFence = application.assistantLifecycleFence
    private val invocationGate = AssistantInvocationGate()
    private var statusView: TextView? = null

    override fun onCreateContentView(): View {
        val density = context.resources.displayMetrics.density
        val padding = (24 * density).toInt()
        val root = LinearLayout(context).apply {
            orientation = LinearLayout.VERTICAL
            gravity = Gravity.CENTER
            setPadding(padding, padding, padding, padding)
        }
        statusView = TextView(context).apply {
            text = "Hold to talk to Zara"
            textSize = 18f
            gravity = Gravity.CENTER
        }
        val pushToTalk = Button(context).apply {
            text = "Hold to talk"
            contentDescription = "Hold to talk to Zara"
            setOnTouchListener { _, event ->
                when (event.actionMasked) {
                    MotionEvent.ACTION_DOWN -> {
                        beginPushToTalk()
                        true
                    }
                    MotionEvent.ACTION_UP -> {
                        finishPushToTalk(commit = true)
                        performClick()
                        true
                    }
                    MotionEvent.ACTION_CANCEL -> {
                        finishPushToTalk(commit = false)
                        true
                    }
                    else -> true
                }
            }
        }
        root.addView(statusView)
        root.addView(pushToTalk)
        return root
    }

    override fun onShow(args: Bundle?, showFlags: Int) {
        super.onShow(args, showFlags)
        invocationGate.show()
        updateStatus("Hold to talk to Zara")
    }

    override fun onHide() {
        executeFinish(invocationGate.hide())
        super.onHide()
    }

    override fun onDestroy() {
        executeFinish(invocationGate.hide())
        statusView = null
        super.onDestroy()
    }

    private fun beginPushToTalk() {
        if (!invocationGate.beginPress()) return
        updateStatus("Connecting microphone…")
        val permissionGranted =
            ContextCompat.checkSelfPermission(context, Manifest.permission.RECORD_AUDIO) ==
                PackageManager.PERMISSION_GRANTED
        val lifecycleToken = lifecycleFence.beginStart()
        appSession.startAssistantVoice(
            permissionGranted,
            lifecycleFence,
            lifecycleToken,
        ).whenComplete { _, error ->
            context.mainExecutor.execute {
                if (error != null) {
                    invocationGate.startFailed()
                    updateStatus("Voice unavailable: ${rootMessage(error)}")
                    return@execute
                }
                val finish = invocationGate.startSucceeded()
                if (finish is AssistantCaptureFinish.None) {
                    updateStatus("Listening… release to send")
                } else {
                    executeFinish(finish)
                }
            }
        }
    }

    private fun finishPushToTalk(commit: Boolean) {
        val finish = if (commit) {
            invocationGate.releasePress()
        } else {
            invocationGate.cancelPress()
        }
        executeFinish(finish)
    }

    private fun executeFinish(finish: AssistantCaptureFinish) {
        when (finish) {
            AssistantCaptureFinish.None -> Unit
            AssistantCaptureFinish.Commit -> {
                updateStatus("Sending to Zara…")
                appSession.releasePushToTalk().whenComplete { _, error ->
                    context.mainExecutor.execute {
                        updateStatus(
                            if (error == null) "Waiting for Zara…"
                            else "Voice send failed: ${rootMessage(error)}"
                        )
                    }
                }
            }
            AssistantCaptureFinish.Cancel -> {
                updateStatus("Voice cancelled")
                appSession.cancelPushToTalk().whenComplete { _, error ->
                    if (error != null) {
                        context.mainExecutor.execute {
                            updateStatus("Voice cancel failed: ${rootMessage(error)}")
                        }
                    }
                }
            }
        }
    }

    private fun updateStatus(message: String) {
        statusView?.text = message
    }

    private fun rootMessage(error: Throwable): String {
        var current = error
        while (current.cause != null && current.cause !== current) {
            current = current.cause!!
        }
        return current.message ?: current::class.java.simpleName
    }
}
