package ai.zara.app.assistant

import ai.zara.app.AndroidAppSession
import ai.zara.app.ZaraApplication
import ai.zara.app.ui.UiOperationFailure
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
    private enum class CaptureBackend { Local, Remote }

    private val application = context.applicationContext as ZaraApplication
    private val appSession: AndroidAppSession =
        (context.applicationContext as ZaraApplication).appSession
    private val lifecycleFence = application.assistantLifecycleFence
    private val invocationGate = AssistantInvocationGate()
    private val localVoice = LocalAssistantVoiceController(
        context,
        appSession,
        lifecycleFence,
        ::updateStatus,
    )
    private val lifecycleInvalidationRegistration = lifecycleFence.onInvalidate {
        context.mainExecutor.execute(::cancelLocalCaptureForLifecycleInvalidation)
    }
    private var statusView: TextView? = null
    private var captureBackend: CaptureBackend? = null

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
        appSession.assessAssistantRole()
        updateStatus("Hold to talk to Zara")
    }

    override fun onHide() {
        executeFinish(invocationGate.hide())
        localVoice.cancel(notify = false)
        super.onHide()
    }

    override fun onDestroy() {
        executeFinish(invocationGate.hide())
        lifecycleInvalidationRegistration.close()
        localVoice.close()
        statusView = null
        super.onDestroy()
    }

    private fun beginPushToTalk() {
        if (!invocationGate.beginPress()) return
        updateStatus("Starting microphone…")
        val permissionGranted =
            ContextCompat.checkSelfPermission(context, Manifest.permission.RECORD_AUDIO) ==
                PackageManager.PERMISSION_GRANTED
        appSession.assessAssistantRole()
        when (
            val plan = planAssistantCapture(
                mode = appSession.runtimeMode(),
                localState = appSession.localServerState(),
                runtimeState = appSession.state(),
            )
        ) {
            AssistantCapturePlan.Local -> beginLocalPushToTalk(permissionGranted)
            AssistantCapturePlan.Remote -> beginRemotePushToTalk(permissionGranted)
            is AssistantCapturePlan.Reject -> {
                invocationGate.startFailed()
                captureBackend = null
                updateStatus("Voice unavailable: ${plan.reason}")
            }
        }
    }

    private fun beginLocalPushToTalk(permissionGranted: Boolean) {
        captureBackend = CaptureBackend.Local
        try {
            localVoice.start(permissionGranted)
        } catch (error: Throwable) {
            localVoice.cancel()
            captureBackend = null
            invocationGate.startFailed()
            updateStatus("Voice unavailable: ${UiOperationFailure.summarize(error)}")
            return
        }
        val finish = invocationGate.startSucceeded()
        if (finish is AssistantCaptureFinish.None) {
            updateStatus("Listening locally… release to send")
        } else {
            executeFinish(finish)
        }
    }

    private fun beginRemotePushToTalk(permissionGranted: Boolean) {
        captureBackend = CaptureBackend.Remote
        val lifecycleToken = lifecycleFence.beginStart()
        appSession.startAssistantVoice(
            permissionGranted,
            lifecycleFence,
            lifecycleToken,
        ).whenComplete { _, error ->
            context.mainExecutor.execute {
                if (error != null) {
                    invocationGate.startFailed()
                    captureBackend = null
                    updateStatus("Voice unavailable: ${UiOperationFailure.summarize(error)}")
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
            AssistantCaptureFinish.Commit -> when (captureBackend) {
                CaptureBackend.Local -> {
                    captureBackend = null
                    updateStatus("Transcribing locally…")
                    localVoice.stop()
                }
                CaptureBackend.Remote -> {
                    captureBackend = null
                    updateStatus("Sending to Zara…")
                    appSession.releasePushToTalk().whenComplete { _, error ->
                        context.mainExecutor.execute {
                            updateStatus(
                                if (error == null) "Waiting for Zara…"
                                else "Voice send failed: ${UiOperationFailure.summarize(error)}"
                            )
                        }
                    }
                }
                null -> Unit
            }
            AssistantCaptureFinish.Cancel -> when (captureBackend) {
                CaptureBackend.Local -> {
                    captureBackend = null
                    localVoice.cancel()
                }
                CaptureBackend.Remote -> {
                    captureBackend = null
                    updateStatus("Voice cancelled")
                    appSession.cancelPushToTalk().whenComplete { _, error ->
                        if (error != null) {
                            context.mainExecutor.execute {
                                updateStatus("Voice cancel failed: ${UiOperationFailure.summarize(error)}")
                            }
                        }
                    }
                }
                null -> Unit
            }
        }
    }

    private fun cancelLocalCaptureForLifecycleInvalidation() {
        val wasActiveLocalCapture = captureBackend == CaptureBackend.Local
        if (wasActiveLocalCapture) {
            invocationGate.cancelPress()
            captureBackend = null
        }
        localVoice.cancel(notify = false)
        if (wasActiveLocalCapture) {
            updateStatus("Voice cancelled because the Android Assistant service stopped")
        }
    }

    private fun updateStatus(message: String) {
        statusView?.text = message
    }
}
