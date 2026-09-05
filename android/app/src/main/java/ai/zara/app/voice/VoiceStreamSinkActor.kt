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
                    future.completeExceptionally(error)
                }
            }
        } catch (error: RejectedExecutionException) {
            throw VoiceStreamBackpressureException("voice stream mailbox is full")
        }
        return future
    }

    private fun ownerFor(eventSessionId: String): VoicePlaybackController {
        val current = playback
        if (current != null && sessionId == eventSessionId) return current
        current?.close()
        val replacement = playbackFactory(eventSessionId)
        playback = replacement
        sessionId = eventSessionId
        return replacement
    }

    override fun close() {
        if (closed) return
        closed = true
        val future = CompletableFuture<Unit>()
        try {
            executor.execute {
                try {
                    playback?.close()
                    playback = null
                    sessionId = null
                    future.complete(Unit)
                } catch (error: Throwable) {
                    future.completeExceptionally(error)
                }
            }
            future.get()
        } catch (_: RejectedExecutionException) {
            playback?.close()
            playback = null
            sessionId = null
        } finally {
            executor.shutdownNow()
        }
    }
}
