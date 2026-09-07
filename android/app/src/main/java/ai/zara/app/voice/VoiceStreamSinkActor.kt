package ai.zara.app.voice

import java.util.concurrent.ArrayBlockingQueue
import java.util.concurrent.CompletableFuture
import java.util.concurrent.RejectedExecutionException
import java.util.concurrent.ThreadPoolExecutor
import java.util.concurrent.TimeUnit

class VoiceStreamBackpressureException(message: String) : IllegalStateException(message)

class VoiceStreamSinkActor(
    private val playbackFactory: (String) -> VoicePlaybackController,
    private val stateObserver: ((VoiceStreamState) -> Unit)? = null,
    private val failureObserver: ((Throwable) -> Unit)? = null,
    capacity: Int = 64,
) : AutoCloseable {
    private val executor: ThreadPoolExecutor
    private var playback: VoicePlaybackController? = null
    private var sessionId: String? = null
    @Volatile private var closed = false

    init {
        require(capacity > 0) { "voice stream mailbox capacity must be positive" }
        executor = ThreadPoolExecutor(
            1,
            1,
            0L,
            TimeUnit.MILLISECONDS,
            ArrayBlockingQueue(capacity),
            { runnable -> Thread(runnable, "zara-android-voice-stream").apply { isDaemon = true } },
            ThreadPoolExecutor.AbortPolicy(),
        )
    }

    fun accept(event: VoiceStreamEvent): CompletableFuture<VoiceStreamState> {
        check(!closed) { "voice stream sink is closed" }
        val future = CompletableFuture<VoiceStreamState>()
        try {
            executor.execute {
                try {
                    val owner = ownerFor(event.sessionId)
                    owner.accept(event)
                    val state = owner.state()
                    stateObserver?.invoke(state)
                    future.complete(state)
                } catch (error: Throwable) {
                    publishFailureState(error)
                    future.completeExceptionally(error)
                }
            }
        } catch (_: RejectedExecutionException) {
            throwBackpressure()
        }
        return future
    }

    fun interrupt(): CompletableFuture<ActiveAudioOutput?> {
        check(!closed) { "voice stream sink is closed" }
        val future = CompletableFuture<ActiveAudioOutput?>()
        try {
            executor.execute {
                try {
                    val owner = playback
                    val interrupted = owner?.interrupt()
                    if (owner != null) stateObserver?.invoke(owner.state())
                    future.complete(interrupted)
                } catch (error: Throwable) {
                    publishFailureState(error)
                    future.completeExceptionally(error)
                }
            }
        } catch (_: RejectedExecutionException) {
            throwBackpressure()
        }
        return future
    }

    fun reset(): CompletableFuture<Unit> {
        check(!closed) { "voice stream sink is closed" }
        val future = CompletableFuture<Unit>()
        try {
            executor.execute {
                val owner = detachPlayback()
                try {
                    owner?.close()
                    future.complete(Unit)
                } catch (error: Throwable) {
                    notifyFailure(error)
                    future.completeExceptionally(error)
                }
            }
        } catch (_: RejectedExecutionException) {
            throwBackpressure()
        }
        return future
    }

    private fun ownerFor(eventSessionId: String): VoicePlaybackController {
        val current = playback
        if (current != null && sessionId == eventSessionId) return current
        val stale = detachPlayback()
        stale?.close()
        val replacement = playbackFactory(eventSessionId)
        playback = replacement
        sessionId = eventSessionId
        return replacement
    }

    private fun publishFailureState(error: Throwable) {
        val owner = playback
        if (owner != null) {
            val stateFailure = runCatching { stateObserver?.invoke(owner.state()) }.exceptionOrNull()
            if (stateFailure != null && stateFailure !== error) error.addSuppressed(stateFailure)
        }
        notifyFailure(error)
    }

    private fun notifyFailure(error: Throwable) {
        val observerFailure = runCatching { failureObserver?.invoke(error) }.exceptionOrNull()
        if (observerFailure != null && observerFailure !== error) error.addSuppressed(observerFailure)
    }

    private fun throwBackpressure(): Nothing {
        val failure = VoiceStreamBackpressureException("voice stream mailbox is full")
        notifyFailure(failure)
        throw failure
    }

    private fun detachPlayback(): VoicePlaybackController? {
        val owner = playback
        playback = null
        sessionId = null
        return owner
    }

    override fun close() {
        if (closed) return
        closed = true
        val future = CompletableFuture<Unit>()
        try {
            executor.execute {
                val owner = detachPlayback()
                try {
                    owner?.close()
                    future.complete(Unit)
                } catch (error: Throwable) {
                    notifyFailure(error)
                    future.completeExceptionally(error)
                }
            }
            future.get()
        } catch (_: RejectedExecutionException) {
            detachPlayback()?.close()
        } finally {
            executor.shutdownNow()
        }
    }
}
