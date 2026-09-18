package ai.zara.app.assistant

internal class AssistantLifecycleFence {
    private val lock = Any()
    private var generation = 0L
    private val invalidationListeners = mutableSetOf<() -> Unit>()

    fun beginStart(): Long = synchronized(lock) {
        generation
    }

    fun isCurrent(token: Long): Boolean = synchronized(lock) {
        token == generation
    }

    fun onInvalidate(listener: () -> Unit): AutoCloseable {
        synchronized(lock) {
            invalidationListeners += listener
        }
        return AutoCloseable {
            synchronized(lock) {
                invalidationListeners -= listener
            }
        }
    }

    fun invalidate() {
        val listeners = synchronized(lock) {
            generation += 1
            invalidationListeners.toList()
        }
        var firstFailure: Throwable? = null
        listeners.forEach { listener ->
            try {
                listener()
            } catch (error: Throwable) {
                if (firstFailure == null) {
                    firstFailure = error
                } else {
                    firstFailure.addSuppressed(error)
                }
            }
        }
        firstFailure?.let { throw it }
    }
}
