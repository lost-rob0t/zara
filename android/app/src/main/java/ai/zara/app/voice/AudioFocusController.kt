package ai.zara.app.voice

enum class AudioFocusLoss {
    Transient,
    Permanent,
}

interface AudioFocusPlatform {
    fun request(onLoss: (AudioFocusLoss) -> Unit): Boolean
    fun abandon()
}

class AudioFocusController(
    private val platform: AudioFocusPlatform,
    private val onLoss: (AudioFocusLoss) -> Unit,
) {
    private val lock = Any()
    private var acquiring = false
    private var held = false

    fun acquire(): Boolean = synchronized(lock) {
        if (held) return@synchronized true
        check(!acquiring) { "Android audio focus acquisition already active" }
        acquiring = true
        try {
            val granted = platform.request(::handleLoss)
            if (!granted || !acquiring) {
                acquiring = false
                return@synchronized false
            }
            acquiring = false
            held = true
            true
        } catch (error: Throwable) {
            acquiring = false
            throw error
        }
    }

    fun release() {
        val abandon = synchronized(lock) {
            if (!held) return@synchronized false
            held = false
            true
        }
        if (abandon) platform.abandon()
    }

    fun isHeld(): Boolean = synchronized(lock) { held }

    private fun handleLoss(loss: AudioFocusLoss) {
        val notify = synchronized(lock) {
            if (acquiring) {
                acquiring = false
                return@synchronized false
            }
            if (!held) return@synchronized false
            held = false
            true
        }
        if (notify) onLoss(loss)
    }
}
