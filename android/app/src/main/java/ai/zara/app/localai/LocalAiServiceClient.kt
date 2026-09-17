package ai.zara.app.localai

import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.content.ServiceConnection
import android.os.IBinder
import java.util.concurrent.CompletableFuture

class LocalAiServiceClient(
    context: Context,
) : AutoCloseable {
    private val appContext = context.applicationContext
    private val lock = Any()

    @Volatile
    private var closed = false

    private var binder: LocalAiService.LocalBinder? = null
    private var pendingBind: CompletableFuture<LocalAiService.LocalBinder>? = null
    private var bound = false

    private val connection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName?, service: IBinder?) {
            val local = service as? LocalAiService.LocalBinder
            synchronized(lock) {
                if (closed) return
                if (local == null) {
                    pendingBind?.completeExceptionally(
                        IllegalStateException("Local AI service returned an incompatible binder")
                    )
                    pendingBind = null
                    return
                }
                binder = local
                pendingBind?.complete(local)
                pendingBind = null
            }
        }

        override fun onServiceDisconnected(name: ComponentName?) {
            synchronized(lock) {
                binder = null
            }
        }

        override fun onBindingDied(name: ComponentName?) {
            synchronized(lock) {
                binder = null
                pendingBind?.completeExceptionally(IllegalStateException("Local AI service binding died"))
                pendingBind = null
                bound = false
            }
        }

        override fun onNullBinding(name: ComponentName?) {
            synchronized(lock) {
                binder = null
                pendingBind?.completeExceptionally(IllegalStateException("Local AI service returned a null binding"))
                pendingBind = null
                bound = false
            }
        }
    }

    fun state(): CompletableFuture<LocalAiState> = service().thenApply { it.state() }

    fun ttsState(): CompletableFuture<LocalTtsState> = service().thenApply { it.ttsState() }

    fun generate(
        request: LocalGenerationRequest,
        onChunk: (String) -> Unit = {},
    ): CompletableFuture<LocalGenerationResult> =
        service().thenCompose { it.generate(request, onChunk) }

    fun cancelGeneration(): CompletableFuture<LocalAiState> =
        service().thenCompose { it.cancelGeneration() }

    fun speak(text: String): CompletableFuture<Unit> =
        service().thenCompose { it.speak(text) }

    fun stopSpeech() {
        synchronized(lock) {
            binder?.stopSpeech()
        }
    }

    private fun service(): CompletableFuture<LocalAiService.LocalBinder> {
        synchronized(lock) {
            if (closed) return failed(IllegalStateException("Local AI service client is closed"))
            binder?.let { return CompletableFuture.completedFuture(it) }
            pendingBind?.let { return it }

            val future = CompletableFuture<LocalAiService.LocalBinder>()
            pendingBind = future
            val accepted = appContext.bindService(
                Intent(appContext, LocalAiService::class.java),
                connection,
                Context.BIND_AUTO_CREATE,
            )
            if (!accepted) {
                pendingBind = null
                future.completeExceptionally(IllegalStateException("Android refused the local AI service binding"))
            } else {
                bound = true
            }
            return future
        }
    }

    override fun close() {
        synchronized(lock) {
            if (closed) return
            closed = true
            pendingBind?.completeExceptionally(IllegalStateException("Local AI service client closed"))
            pendingBind = null
            binder = null
            if (bound) {
                runCatching { appContext.unbindService(connection) }
                bound = false
            }
        }
    }

    private fun <T> failed(error: Throwable): CompletableFuture<T> =
        CompletableFuture<T>().also { it.completeExceptionally(error) }
}
