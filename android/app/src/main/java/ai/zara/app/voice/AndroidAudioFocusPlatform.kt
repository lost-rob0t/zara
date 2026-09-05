package ai.zara.app.voice

import android.content.Context
import android.media.AudioAttributes
import android.media.AudioFocusRequest
import android.media.AudioManager
import android.os.Handler
import android.os.Looper

class AndroidAudioFocusPlatform(context: Context) : AudioFocusPlatform {
    private val audioManager = context.applicationContext
        .getSystemService(Context.AUDIO_SERVICE) as AudioManager
    private val handler = Handler(Looper.getMainLooper())
    private val lock = Any()
    private var activeRequest: AudioFocusRequest? = null

    override fun request(onLoss: (AudioFocusLoss) -> Unit): Boolean = synchronized(lock) {
        check(activeRequest == null) { "Android audio focus request already active" }
        val listener = AudioManager.OnAudioFocusChangeListener { change ->
            when (change) {
                AudioManager.AUDIOFOCUS_LOSS -> clearAndNotify(AudioFocusLoss.Permanent, onLoss)
                AudioManager.AUDIOFOCUS_LOSS_TRANSIENT,
                AudioManager.AUDIOFOCUS_LOSS_TRANSIENT_CAN_DUCK,
                -> clearAndNotify(AudioFocusLoss.Transient, onLoss)
            }
        }
        val request = AudioFocusRequest.Builder(AudioManager.AUDIOFOCUS_GAIN_TRANSIENT_MAY_DUCK)
            .setAudioAttributes(
                AudioAttributes.Builder()
                    .setUsage(AudioAttributes.USAGE_ASSISTANT)
                    .setContentType(AudioAttributes.CONTENT_TYPE_SPEECH)
                    .build()
            )
            .setOnAudioFocusChangeListener(listener, handler)
            .setAcceptsDelayedFocusGain(false)
            .setWillPauseWhenDucked(true)
            .build()
        val result = audioManager.requestAudioFocus(request)
        if (result != AudioManager.AUDIOFOCUS_REQUEST_GRANTED) return@synchronized false
        activeRequest = request
        true
    }

    override fun abandon() {
        val request = synchronized(lock) {
            val current = activeRequest ?: return
            activeRequest = null
            current
        }
        audioManager.abandonAudioFocusRequest(request)
    }

    private fun clearAndNotify(
        loss: AudioFocusLoss,
        onLoss: (AudioFocusLoss) -> Unit,
    ) {
        val request = synchronized(lock) {
            val current = activeRequest ?: return
            activeRequest = null
            current
        }
        // Zara never auto-resumes old server audio after a transient loss. Abandon the
        // framework request immediately so a later AUDIOFOCUS_GAIN cannot resurrect it.
        audioManager.abandonAudioFocusRequest(request)
        onLoss(loss)
    }
}
