package ai.zara.app.voice

import android.media.AudioAttributes
import android.media.AudioFormat
import android.media.AudioTrack

class AndroidPcmOutput : PcmOutput {
    private var track: AudioTrack? = null

    @Synchronized
    override fun start(sampleRate: Int, channels: Int) {
        check(track == null) { "Android voice output is already active" }
        require(sampleRate > 0) { "sample rate must be positive" }
        require(channels == 1) { "canonical Android Zara output must be mono" }

        val minimumBuffer = AudioTrack.getMinBufferSize(
            sampleRate,
            AudioFormat.CHANNEL_OUT_MONO,
            AudioFormat.ENCODING_PCM_16BIT,
        )
        check(minimumBuffer > 0) { "Android speaker does not support negotiated Zara PCM" }

        val created = AudioTrack.Builder()
            .setAudioAttributes(
                AudioAttributes.Builder()
                    .setUsage(AudioAttributes.USAGE_ASSISTANT)
                    .setContentType(AudioAttributes.CONTENT_TYPE_SPEECH)
                    .build()
            )
            .setAudioFormat(
                AudioFormat.Builder()
                    .setEncoding(AudioFormat.ENCODING_PCM_16BIT)
                    .setSampleRate(sampleRate)
                    .setChannelMask(AudioFormat.CHANNEL_OUT_MONO)
                    .build()
            )
            .setTransferMode(AudioTrack.MODE_STREAM)
            .setBufferSizeInBytes(maxOf(minimumBuffer, sampleRate / 5 * 2))
            .build()
        check(created.state == AudioTrack.STATE_INITIALIZED) {
            created.release()
            "Android speaker failed to initialize"
        }
        try {
            created.play()
        } catch (error: Throwable) {
            created.release()
            throw error
        }
        check(created.playState == AudioTrack.PLAYSTATE_PLAYING) {
            created.release()
            "Android speaker failed to start"
        }
        track = created
    }

    override fun write(pcm: ByteArray) {
        require(pcm.isNotEmpty() && pcm.size % 2 == 0) {
            "PCM output must contain whole s16le samples"
        }
        val active = synchronized(this) {
            track ?: throw IllegalStateException("Android voice output is not active")
        }
        var offset = 0
        while (offset < pcm.size) {
            val written = active.write(
                pcm,
                offset,
                pcm.size - offset,
                AudioTrack.WRITE_BLOCKING,
            )
            check(written > 0) { "Android speaker write failed: $written" }
            offset += written
        }
    }

    override fun stop() {
        val active = synchronized(this) {
            val current = track ?: throw IllegalStateException("Android voice output is not active")
            track = null
            current
        }
        stopAndRelease(active)
    }

    override fun close() {
        val active = synchronized(this) {
            val current = track ?: return
            track = null
            current
        }
        stopAndRelease(active)
    }

    private fun stopAndRelease(active: AudioTrack) {
        try {
            if (active.playState == AudioTrack.PLAYSTATE_PLAYING) active.stop()
            active.flush()
        } finally {
            active.release()
        }
    }
}
