package ai.zara.app.assistant

internal class AssistantInvocationGate {
    private val lock = Any()
    private var shown = false
    private var cancellationClaimed = false

    fun beginShow(): Boolean = synchronized(lock) {
        if (shown) return@synchronized false
        shown = true
        cancellationClaimed = false
        true
    }

    fun endShow(): Boolean = synchronized(lock) {
        val wasShown = shown
        shown = false
        wasShown
    }

    fun shouldCancelLateStart(): Boolean = synchronized(lock) { !shown }

    fun claimCancellation(): Boolean = synchronized(lock) {
        if (cancellationClaimed) return@synchronized false
        cancellationClaimed = true
        true
    }
}
