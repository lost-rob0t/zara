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
    private var held = false

    fun acquire(): Boolean = synchronized(lock) {
        if (held) return@synchronized true
        if (!platform.request(::handleLoss)) return@synchronized false
        held = true
        true
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
            if (!held) return@synchronized false
            held = false
            true
        }
        if (notify) onLoss(loss)
    }
}
