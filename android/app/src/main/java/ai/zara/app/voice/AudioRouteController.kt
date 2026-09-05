package ai.zara.app.voice

enum class AudioRouteKind {
    BuiltIn,
    Wired,
    Bluetooth,
    Usb,
    Other,
}

data class AudioRouteSnapshot(val outputs: Set<AudioRouteKind>)

interface AudioRoutePlatform {
    fun snapshot(): AudioRouteSnapshot
    fun start(onChanged: (AudioRouteSnapshot) -> Unit)
    fun stop()
}

class AudioRouteController(
    private val platform: AudioRoutePlatform,
    private val onChanged: (AudioRouteSnapshot) -> Unit,
    private val onRouteInterrupted: (AudioRouteSnapshot, AudioRouteSnapshot) -> Unit,
) {
    private val lock = Any()
    private var started = false
    private var current: AudioRouteSnapshot? = null

    fun start() {
        val initial = synchronized(lock) {
            check(!started) { "audio route controller already started" }
            started = true
            platform.snapshot().also { current = it }
        }
        platform.start(::handleChanged)
        onChanged(initial)
    }

    fun stop() {
        val shouldStop = synchronized(lock) {
            if (!started) return@synchronized false
            started = false
            current = null
            true
        }
        if (shouldStop) platform.stop()
    }

    fun current(): AudioRouteSnapshot? = synchronized(lock) { current }

    private fun handleChanged(next: AudioRouteSnapshot) {
        val previous = synchronized(lock) {
            if (!started) return
            val old = current ?: return
            if (old == next) return
            current = next
            old
        }
        onRouteInterrupted(previous, next)
        onChanged(next)
    }
}
