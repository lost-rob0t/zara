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
    private var starting = false
    private var started = false
    private var stopping = false
    private var current: AudioRouteSnapshot? = null

    fun start() {
        synchronized(lock) {
            check(!starting && !started && !stopping) { "audio route controller already started" }
            starting = true
        }

        try {
            platform.start(::handleChanged)
            val initial = platform.snapshot()
            synchronized(lock) {
                starting = false
                started = true
                current = initial
            }
            onChanged(initial)
            handleChanged(platform.snapshot())
        } catch (failure: Throwable) {
            synchronized(lock) {
                starting = false
                started = false
                current = null
            }
            val rollbackFailure = runCatching { platform.stop() }.exceptionOrNull()
            if (rollbackFailure != null && rollbackFailure !== failure) {
                failure.addSuppressed(rollbackFailure)
            }
            throw failure
        }
    }

    fun stop() {
        val shouldStop = synchronized(lock) {
            if (!starting && !started) return@synchronized false
            check(!stopping) { "audio route controller stop already active" }
            stopping = true
            true
        }
        if (!shouldStop) return

        try {
            platform.stop()
        } catch (failure: Throwable) {
            synchronized(lock) { stopping = false }
            throw failure
        }

        synchronized(lock) {
            starting = false
            started = false
            stopping = false
            current = null
        }
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
