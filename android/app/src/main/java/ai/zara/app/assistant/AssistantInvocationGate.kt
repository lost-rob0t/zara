package ai.zara.app.assistant

internal class AssistantInvocationGate {
    private val lock = Any()
    private var shown = false

    fun beginShow(): Boolean = synchronized(lock) {
        if (shown) return@synchronized false
        shown = true
        true
    }

    fun endShow(): Boolean = synchronized(lock) {
        val wasShown = shown
        shown = false
        wasShown
    }

    fun shouldCancelLateStart(): Boolean = synchronized(lock) { !shown }
}
