package ai.zara.app.localai

import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.content.ServiceConnection
import android.net.Uri
import android.os.Bundle
import android.os.Handler
import android.os.IBinder
import android.os.Looper
import android.os.Message
import android.os.Messenger
import android.os.RemoteException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicLong

class LocalAiRemoteClient(
    context: Context,
) : AutoCloseable {
    private data class Pending(
        val future: CompletableFuture<Bundle>,
        val onChunk: (String) -> Unit,
        val generation: LocalAiRemoteGenerationTicket?,
    )

    private val appContext = context.applicationContext
    private val lock = Any()
    private val requestIds = AtomicLong(1L)
    private val pending = linkedMapOf<Long, Pending>()

    @Volatile
    private var closed = false

    private var remote: Messenger? = null
    private var pendingBind: CompletableFuture<Messenger>? = null
    private var bound = false

    private val replyMessenger = Messenger(
        Handler(Looper.getMainLooper()) { message ->
            handleReply(message)
            true
        }
    )

    private val connection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName?, service: IBinder?) {
            val messenger = service?.let(::Messenger)
            synchronized(lock) {
                if (closed) return
                if (messenger == null) {
                    pendingBind?.completeExceptionally(
                        LocalAiRemoteUnavailableException("Canonical local AI service returned no binder")
                    )
                    pendingBind = null
                    bound = false
                    return
                }
                remote = messenger
                pendingBind?.complete(messenger)
                pendingBind = null
            }
        }

        override fun onServiceDisconnected(name: ComponentName?) {
            disconnect("Canonical local AI service disconnected")
        }

        override fun onBindingDied(name: ComponentName?) {
            disconnect("Canonical local AI service binding died")
        }

        override fun onNullBinding(name: ComponentName?) {
            disconnect("Canonical local AI service returned a null binding")
        }
    }

    fun state(): CompletableFuture<LocalAiState> =
        request(LocalAiRemoteProtocol.MSG_STATE).thenApply { payload ->
            LocalAiRemoteProtocol.stateFromBundle(requireBundle(payload, LocalAiRemoteProtocol.KEY_STATE))
        }

    fun models(): CompletableFuture<List<LocalModelSpec>> =
        request(LocalAiRemoteProtocol.MSG_MODELS).thenApply(LocalAiRemoteProtocol::modelsFromBundle)

    fun activeModel(): CompletableFuture<LocalModelSpec?> =
        request(LocalAiRemoteProtocol.MSG_ACTIVE_MODEL).thenApply { payload ->
            payload.getBundle(LocalAiRemoteProtocol.KEY_MODEL)?.let(LocalAiRemoteProtocol::modelFromBundle)
        }

    fun installModel(
        uri: Uri,
        metadata: LocalModelMetadata,
    ): CompletableFuture<LocalModelSpec> {
        val descriptor = appContext.contentResolver.openFileDescriptor(uri, "r")
            ?: return failed(LocalAiRemoteUnavailableException("Selected model cannot be opened"))
        val payload = Bundle().apply {
            putBundle(LocalAiRemoteProtocol.KEY_METADATA, LocalAiRemoteProtocol.metadataToBundle(metadata))
            putParcelable(LocalAiRemoteProtocol.KEY_MODEL_FD, descriptor)
        }
        return request(LocalAiRemoteProtocol.MSG_INSTALL_MODEL, payload)
            .whenComplete { _, _ -> runCatching { descriptor.close() } }
            .thenApply { reply ->
                LocalAiRemoteProtocol.modelFromBundle(
                    requireBundle(reply, LocalAiRemoteProtocol.KEY_MODEL)
                )
            }
    }

    fun selectModel(
        id: String,
        version: String,
    ): CompletableFuture<LocalAiState> =
        request(
            LocalAiRemoteProtocol.MSG_SELECT_MODEL,
            Bundle().apply {
                putString(LocalAiRemoteProtocol.KEY_ID, id)
                putString(LocalAiRemoteProtocol.KEY_VERSION, version)
            },
        ).thenApply { payload ->
            LocalAiRemoteProtocol.stateFromBundle(requireBundle(payload, LocalAiRemoteProtocol.KEY_STATE))
        }

    fun generate(
        request: LocalGenerationRequest,
        onChunk: (String) -> Unit = {},
    ): CompletableFuture<LocalGenerationResult> =
        request(
            LocalAiRemoteProtocol.MSG_GENERATE,
            Bundle().apply {
                putString(LocalAiRemoteProtocol.KEY_PROMPT, request.prompt)
                putInt(LocalAiRemoteProtocol.KEY_MAX_OUTPUT_TOKENS, request.maxOutputTokens)
            },
            onChunk,
            generation = true,
        ).thenApply { payload ->
            LocalAiRemoteProtocol.generationResultFromBundle(
                requireBundle(payload, LocalAiRemoteProtocol.KEY_RESULT)
            )
        }

    fun cancelGeneration(): CompletableFuture<LocalAiState> {
        cancelPendingGenerations()
        return request(LocalAiRemoteProtocol.MSG_CANCEL).thenApply { payload ->
            LocalAiRemoteProtocol.stateFromBundle(requireBundle(payload, LocalAiRemoteProtocol.KEY_STATE))
        }
    }

    fun unloadModel(): CompletableFuture<LocalAiState> =
        request(LocalAiRemoteProtocol.MSG_UNLOAD).thenApply { payload ->
            LocalAiRemoteProtocol.stateFromBundle(requireBundle(payload, LocalAiRemoteProtocol.KEY_STATE))
        }

    private fun request(
        what: Int,
        payload: Bundle = Bundle(),
        onChunk: (String) -> Unit = {},
        generation: Boolean = false,
    ): CompletableFuture<Bundle> =
        service().thenCompose { messenger ->
            val requestId = requestIds.getAndIncrement()
            val future = CompletableFuture<Bundle>()
            val generationTicket = if (generation) LocalAiRemoteGenerationTicket() else null
            synchronized(lock) {
                if (closed) {
                    return@thenCompose failed(LocalAiRemoteUnavailableException("Local AI remote client is closed"))
                }
                pending[requestId] = Pending(future, onChunk, generationTicket)
            }
            payload.putLong(LocalAiRemoteProtocol.KEY_REQUEST_ID, requestId)
            val message = Message.obtain(null, what).apply {
                data = payload
                replyTo = replyMessenger
            }
            try {
                messenger.send(message)
            } catch (error: RemoteException) {
                val failedPending = synchronized(lock) { pending.remove(requestId) }
                failedPending?.generation?.terminate()
                future.completeExceptionally(
                    LocalAiRemoteUnavailableException("Canonical local AI service send failed", error)
                )
            }
            future
        }

    private fun handleReply(message: Message) {
        val payload = message.data ?: Bundle()
        val requestId = payload.getLong(LocalAiRemoteProtocol.KEY_REQUEST_ID, -1L)
        if (requestId < 0L) return
        when (message.what) {
            LocalAiRemoteProtocol.EVENT_CHUNK -> {
                val current = synchronized(lock) { pending[requestId] } ?: return
                val chunk = payload.getString(LocalAiRemoteProtocol.KEY_CHUNK).orEmpty()
                if (chunk.isEmpty()) return
                val generation = current.generation
                if (generation == null) {
                    current.onChunk(chunk)
                } else {
                    generation.deliver { current.onChunk(chunk) }
                }
            }

            LocalAiRemoteProtocol.RESULT_OK -> {
                val current = synchronized(lock) { pending.remove(requestId) } ?: return
                if (current.generation?.terminate() == false) {
                    current.future.cancel(false)
                    return
                }
                current.future.complete(payload)
            }

            LocalAiRemoteProtocol.RESULT_ERROR -> {
                val current = synchronized(lock) { pending.remove(requestId) } ?: return
                if (current.generation?.terminate() == false) {
                    current.future.cancel(false)
                    return
                }
                val type = payload.getString(LocalAiRemoteProtocol.KEY_ERROR_TYPE).orEmpty()
                val detail = payload.getString(LocalAiRemoteProtocol.KEY_ERROR_MESSAGE)
                    ?.takeIf(String::isNotBlank)
                    ?: "Canonical local AI request failed"
                current.future.completeExceptionally(
                    LocalAiRemoteOperationException(type.take(96), detail.take(512))
                )
            }
        }
    }

    private fun cancelPendingGenerations() {
        val cancelled = synchronized(lock) {
            val generationIds = pending.entries
                .asSequence()
                .filter { it.value.generation != null }
                .map { it.key }
                .toList()
            generationIds.mapNotNull(pending::remove)
        }
        cancelled.forEach { current ->
            current.generation?.terminate()
            current.future.cancel(false)
        }
    }

    private fun service(): CompletableFuture<Messenger> {
        synchronized(lock) {
            if (closed) return failed(LocalAiRemoteUnavailableException("Local AI remote client is closed"))
            remote?.let { return CompletableFuture.completedFuture(it) }
            pendingBind?.let { return it }

            val future = CompletableFuture<Messenger>()
            pendingBind = future
            val intent = Intent(LocalAiRemoteProtocol.ACTION_BIND).setComponent(
                ComponentName(
                    LocalAiRemoteProtocol.HOST_PACKAGE,
                    LocalAiRemoteProtocol.HOST_SERVICE,
                )
            )
            val accepted = try {
                appContext.bindService(intent, connection, Context.BIND_AUTO_CREATE)
            } catch (error: SecurityException) {
                pendingBind = null
                future.completeExceptionally(
                    LocalAiRemoteUnavailableException("Canonical local AI service permission denied", error)
                )
                return future
            }
            if (!accepted) {
                pendingBind = null
                future.completeExceptionally(
                    LocalAiRemoteUnavailableException("Canonical local AI service is unavailable")
                )
            } else {
                bound = true
            }
            return future
        }
    }

    private fun disconnect(message: String) {
        val failure = LocalAiRemoteUnavailableException(message)
        val waiting: List<Pending>
        synchronized(lock) {
            remote = null
            pendingBind?.completeExceptionally(failure)
            pendingBind = null
            bound = false
            waiting = pending.values.toList()
            pending.clear()
        }
        waiting.forEach { current ->
            current.generation?.terminate()
            current.future.completeExceptionally(failure)
        }
    }

    override fun close() {
        val waiting: List<Pending>
        val shouldUnbind: Boolean
        synchronized(lock) {
            if (closed) return
            closed = true
            remote = null
            pendingBind?.completeExceptionally(
                LocalAiRemoteUnavailableException("Local AI remote client closed")
            )
            pendingBind = null
            waiting = pending.values.toList()
            pending.clear()
            shouldUnbind = bound
            bound = false
        }
        waiting.forEach { current ->
            current.generation?.terminate()
            current.future.completeExceptionally(
                LocalAiRemoteUnavailableException("Local AI remote client closed")
            )
        }
        if (shouldUnbind) runCatching { appContext.unbindService(connection) }
    }

    private fun requireBundle(payload: Bundle, key: String): Bundle =
        payload.getBundle(key) ?: throw LocalAiRemoteOperationException(
            "MalformedReply",
            "Canonical local AI reply omitted $key",
        )

    private fun <T> failed(error: Throwable): CompletableFuture<T> =
        CompletableFuture<T>().also { it.completeExceptionally(error) }
}

internal class LocalAiRemoteGenerationTicket {
    private var active = true

    @Synchronized
    fun deliver(block: () -> Unit): Boolean {
        if (!active) return false
        block()
        return true
    }

    @Synchronized
    fun terminate(): Boolean {
        if (!active) return false
        active = false
        return true
    }
}

class LocalAiRemoteUnavailableException(
    message: String,
    cause: Throwable? = null,
) : IllegalStateException(message, cause)

class LocalAiRemoteOperationException(
    val remoteType: String,
    message: String,
) : IllegalStateException(if (remoteType.isBlank()) message else "$remoteType: $message")
