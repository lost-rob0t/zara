package ai.zara.app.voice

import android.annotation.SuppressLint
import android.media.AudioFormat
import android.media.AudioRecord
import android.media.MediaRecorder
import java.util.concurrent.atomic.AtomicBoolean

class AndroidPcmRecorder : PcmRecorder {
    private var active: Capture? = null

    @SuppressLint("MissingPermission")
    @Synchronized
    override fun start(onFrame: (ByteArray) -> Unit, onFailure: (Throwable) -> Unit) {
        check(active == null) { "Android microphone capture is already active" }

        val minimumBuffer = AudioRecord.getMinBufferSize(
            ManualVoiceCapture.PCM_SAMPLE_RATE_HZ,
            AudioFormat.CHANNEL_IN_MONO,
            AudioFormat.ENCODING_PCM_16BIT,
        )
        check(minimumBuffer > 0) { "Android microphone does not support canonical Zara PCM" }

        val recorder = AudioRecord.Builder()
            .setAudioSource(MediaRecorder.AudioSource.VOICE_RECOGNITION)
            .setAudioFormat(
                AudioFormat.Builder()
                    .setEncoding(AudioFormat.ENCODING_PCM_16BIT)
                    .setSampleRate(ManualVoiceCapture.PCM_SAMPLE_RATE_HZ)
                    .setChannelMask(AudioFormat.CHANNEL_IN_MONO)
                    .build()
            )
            .setBufferSizeInBytes(maxOf(minimumBuffer, ManualVoiceCapture.PCM_FRAME_BYTES * 4))
            .build()
        check(recorder.state == AudioRecord.STATE_INITIALIZED) {
            recorder.release()
            "Android microphone failed to initialize"
        }

        try {
            recorder.startRecording()
        } catch (error: Throwable) {
            recorder.release()
            throw error
        }
        check(recorder.recordingState == AudioRecord.RECORDSTATE_RECORDING) {
            recorder.release()
            "Android microphone failed to start"
        }

        val running = AtomicBoolean(true)
        val capture = Capture(
            recorder = recorder,
            running = running,
            thread = Thread(
                { readLoop(recorder, running, onFrame, onFailure) },
                "zara-android-manual-mic",
            ).apply { isDaemon = true },
        )
        active = capture
        capture.thread.start()
    }

    override fun stop() {
        val capture = synchronized(this) {
            val current = active ?: throw IllegalStateException("Android microphone capture is not active")
            active = null
            current.running.set(false)
            current
        }
        stopCapture(capture)
    }

    override fun close() {
        val capture = synchronized(this) {
            val current = active ?: return
            active = null
            current.running.set(false)
            current
        }
        stopCapture(capture)
    }

    private fun readLoop(
        recorder: AudioRecord,
        running: AtomicBoolean,
        onFrame: (ByteArray) -> Unit,
        onFailure: (Throwable) -> Unit,
    ) {
        val frame = ByteArray(ManualVoiceCapture.PCM_FRAME_BYTES)
        var offset = 0
        try {
            while (running.get()) {
                val read = recorder.read(
                    frame,
                    offset,
                    frame.size - offset,
                    AudioRecord.READ_BLOCKING,
                )
                if (!running.get()) break
                if (read <= 0) throw IllegalStateException("Android microphone read failed: $read")
                offset += read
                if (offset == frame.size) {
                    onFrame(frame.copyOf())
                    offset = 0
                }
            }
        } catch (error: Throwable) {
            if (running.compareAndSet(true, false)) {
                synchronized(this) {
                    val current = active
                    if (current != null && current.recorder === recorder) active = null
                }
                runCatching { recorder.stop() }
                recorder.release()
                onFailure(error)
            }
        }
    }

    private fun stopCapture(capture: Capture) {
        var stopFailure: Throwable? = null
        try {
            capture.recorder.stop()
        } catch (error: Throwable) {
            stopFailure = error
        }
        try {
            capture.thread.join(STOP_JOIN_MILLIS)
            check(!capture.thread.isAlive) { "Android microphone thread did not stop" }
        } finally {
            capture.recorder.release()
        }
        if (stopFailure != null) throw stopFailure
    }

    private data class Capture(
        val recorder: AudioRecord,
        val running: AtomicBoolean,
        val thread: Thread,
    )

    companion object {
        private const val STOP_JOIN_MILLIS = 2_000L
    }
}
