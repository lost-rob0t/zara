package ai.zara.app.assistant

internal class AssistantLifecycleFence {
    private val lock = Any()
    private var generation = 0L

    fun beginStart(): Long = synchronized(lock) {
        generation
    }

    fun isCurrent(token: Long): Boolean = synchronized(lock) {
        token == generation
    }

    fun invalidate() = synchronized(lock) {
        generation += 1
    }
}
