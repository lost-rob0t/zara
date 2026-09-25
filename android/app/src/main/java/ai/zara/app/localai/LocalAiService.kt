package ai.zara.app.localai

import android.app.Service
import android.content.Intent
import android.os.Binder
import android.os.Bundle
import android.os.Handler
import android.os.IBinder
import android.os.Looper
import android.os.Message
import android.os.Messenger
import android.os.ParcelFileDescriptor
import android.os.RemoteException
import java.io.File
import java.io.FileInputStream
import java.io.InputStream
import java.util.concurrent.CancellationException
import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

class LocalAiService : Service() {
    private val binder = LocalBinder()
    private val modelIo: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-local-model-store").apply { isDaemon = true }
    }

    private lateinit var modelStore: LocalModelStore
    private lateinit var runtime: LocalAiRuntime
    private lateinit var ttsRegistry: LocalTtsProviderRegistry
    private lateinit var remoteMessenger: Messenger
    private val remoteGenerationLock = Any()
    private var activeRemoteGeneration: RemoteGenerationRegistration? = null

    @Volatile
    private lateinit var tts: LocalTtsProvider

    @Volatile
    private var loading: CompletableFuture<LocalAiState>? = null

    override fun onCreate() {
        super.onCreate()
        modelStore = LocalModelStore(File(filesDir, "zara/models"))
        runtime = LocalAiRuntime(LiteRtLocalLlmBackend(this))
        ttsRegistry = LocalTtsProviderRegistry(
            listOf(
                AndroidOfflineTtsBackend(this),
            )
        )
        tts = ttsRegistry.default()
        tts.initialize()
        remoteMessenger = Messenger(RemoteHandler(Looper.getMainLooper()))
        loadActiveModel()
    }

    override fun onBind(intent: Intent?): IBinder =
        if (intent?.action == LocalAiRemoteProtocol.ACTION_BIND) {
            remoteMessenger.binder
        } else {
            binder
        }

    override fun onDestroy() {
        loading?.cancel(true)
        clearRemoteGeneration()?.lease?.finish()
        runtime.close()
        ttsRegistry.close()
        modelIo.shutdownNow()
        super.onDestroy()
    }

    inner class LocalBinder : Binder() {
        fun state(): LocalAiState = runtime.state()

        fun models(): CompletableFuture<List<LocalModelSpec>> = this@LocalAiService.models()

        fun activeModel(): CompletableFuture<LocalModelSpec?> = this@LocalAiService.activeModel()

        fun ttsState(): LocalTtsState = tts.state()

        fun ttsProviders(): List<LocalTtsProviderCapabilities> = ttsRegistry.capabilities()

        fun selectTtsProvider(id: String): CompletableFuture<LocalTtsState> =
            this@LocalAiService.selectTtsProvider(id)

        fun loadActiveModel(): CompletableFuture<LocalAiState> = this@LocalAiService.loadActiveModel()

        fun installModel(
            source: InputStream,
            metadata: LocalModelMetadata,
        ): CompletableFuture<LocalModelSpec> = this@LocalAiService.installModel(source, metadata)

        fun selectModel(
            id: String,
            version: String,
        ): CompletableFuture<LocalAiState> = this@LocalAiService.selectModel(id, version)

        fun generate(
            request: LocalGenerationRequest,
            onChunk: (String) -> Unit = {},
        ): CompletableFuture<LocalGenerationResult> =
            this@LocalAiService.generate(request, onChunk)

        fun cancelGeneration(): CompletableFuture<LocalAiState> = runtime.cancel()

        fun unloadModel(): CompletableFuture<LocalAiState> = runtime.unload()

        fun speak(text: String): CompletableFuture<Unit> = tts.speak(text)

        fun stopSpeech() = tts.stop()
    }

    private inner class RemoteHandler(looper: Looper) : Handler(looper) {
        override fun handleMessage(message: Message) {
            val requestId = message.data.getLong(LocalAiRemoteProtocol.KEY_REQUEST_ID, -1L)
            val replyTo = message.replyTo
            if (requestId < 0L || replyTo == null) return
            try {
                when (message.what) {
                    LocalAiRemoteProtocol.MSG_STATE -> replyFuture(
                        requestId,
                        replyTo,
                        CompletableFuture.completedFuture(runtime.state()),
                    ) { state ->
                        Bundle().apply {
                            putBundle(LocalAiRemoteProtocol.KEY_STATE, LocalAiRemoteProtocol.stateToBundle(state))
                        }
                    }

                    LocalAiRemoteProtocol.MSG_MODELS -> replyFuture(
                        requestId,
                        replyTo,
                        models(),
                        LocalAiRemoteProtocol::modelsToBundle,
                    )

                    LocalAiRemoteProtocol.MSG_ACTIVE_MODEL -> replyFuture(
                        requestId,
                        replyTo,
                        activeModel(),
                    ) { model ->
                        Bundle().apply {
                            putBundle(LocalAiRemoteProtocol.KEY_MODEL, model?.let(LocalAiRemoteProtocol::modelToBundle))
                        }
                    }

                    LocalAiRemoteProtocol.MSG_INSTALL_MODEL -> handleRemoteInstall(
                        requestId,
                        replyTo,
                        message.data,
                    )

                    LocalAiRemoteProtocol.MSG_SELECT_MODEL -> {
                        val id = requireRemoteString(message.data, LocalAiRemoteProtocol.KEY_ID)
                        val version = requireRemoteString(message.data, LocalAiRemoteProtocol.KEY_VERSION)
                        replyFuture(requestId, replyTo, selectModel(id, version)) { state ->
                            Bundle().apply {
                                putBundle(LocalAiRemoteProtocol.KEY_STATE, LocalAiRemoteProtocol.stateToBundle(state))
                            }
                        }
                    }

                    LocalAiRemoteProtocol.MSG_GENERATE -> {
                        val request = LocalGenerationRequest(
                            prompt = requireRemoteString(message.data, LocalAiRemoteProtocol.KEY_PROMPT),
                            maxOutputTokens = message.data.getInt(LocalAiRemoteProtocol.KEY_MAX_OUTPUT_TOKENS),
                        )
                        val registration = registerRemoteGeneration(requestId, replyTo)
                        val generation = generateRemote(registration, request)
                        replyRemoteGeneration(registration, replyTo, generation)
                    }

                    LocalAiRemoteProtocol.MSG_CANCEL -> replyFuture(
                        requestId,
                        replyTo,
                        cancelRemoteGeneration(replyTo),
                    ) { state ->
                        Bundle().apply {
                            putBundle(LocalAiRemoteProtocol.KEY_STATE, LocalAiRemoteProtocol.stateToBundle(state))
                        }
                    }

                    LocalAiRemoteProtocol.MSG_UNLOAD -> replyFuture(
                        requestId,
                        replyTo,
                        runtime.unload(),
                    ) { state ->
                        Bundle().apply {
                            putBundle(LocalAiRemoteProtocol.KEY_STATE, LocalAiRemoteProtocol.stateToBundle(state))
                        }
                    }

                    else -> sendRemoteError(
                        requestId,
                        replyTo,
                        IllegalArgumentException("Unsupported local AI IPC operation"),
                    )
                }
            } catch (error: Throwable) {
                sendRemoteError(requestId, replyTo, error)
            }
        }
    }

    @Suppress("DEPRECATION")
    private fun handleRemoteInstall(
        requestId: Long,
        replyTo: Messenger,
        payload: Bundle,
    ) {
        val descriptor = payload.getParcelable<ParcelFileDescriptor>(LocalAiRemoteProtocol.KEY_MODEL_FD)
            ?: throw IllegalArgumentException("Local AI IPC model descriptor is missing")
        val metadata = LocalAiRemoteProtocol.metadataFromBundle(
            payload.getBundle(LocalAiRemoteProtocol.KEY_METADATA)
                ?: throw IllegalArgumentException("Local AI IPC model metadata is missing")
        )
        val input = FileInputStream(descriptor.fileDescriptor)
        installModel(input, metadata).whenComplete { model, error ->
            runCatching { descriptor.close() }
            if (error != null) {
                sendRemoteError(requestId, replyTo, rootCause(error))
            } else {
                sendRemote(
                    replyTo,
                    LocalAiRemoteProtocol.RESULT_OK,
                    Bundle().apply {
                        putLong(LocalAiRemoteProtocol.KEY_REQUEST_ID, requestId)
                        putBundle(LocalAiRemoteProtocol.KEY_MODEL, LocalAiRemoteProtocol.modelToBundle(model))
                    },
                )
            }
        }
    }

    private fun <T> replyFuture(
        requestId: Long,
        replyTo: Messenger,
        future: CompletableFuture<T>,
        encode: (T) -> Bundle,
    ) {
        future.whenComplete { value, error ->
            if (error != null) {
                sendRemoteError(requestId, replyTo, rootCause(error))
            } else {
                val payload = encode(value)
                payload.putLong(LocalAiRemoteProtocol.KEY_REQUEST_ID, requestId)
                sendRemote(replyTo, LocalAiRemoteProtocol.RESULT_OK, payload)
            }
        }
    }

    private fun sendRemoteError(
        requestId: Long,
        replyTo: Messenger,
        error: Throwable,
    ) {
        sendRemote(
            replyTo,
            LocalAiRemoteProtocol.RESULT_ERROR,
            LocalAiRemoteProtocol.errorToBundle(requestId, rootCause(error)),
        )
    }

    private fun sendRemote(
        replyTo: Messenger,
        what: Int,
        payload: Bundle,
    ): Boolean = try {
        replyTo.send(Message.obtain(null, what).apply { data = payload })
        true
    } catch (_: RemoteException) {
        false
    }

    private fun registerRemoteGeneration(
        requestId: Long,
        replyTo: Messenger,
    ): RemoteGenerationRegistration {
        val replyBinder = replyTo.binder
        lateinit var deathRecipient: IBinder.DeathRecipient
        deathRecipient = IBinder.DeathRecipient {
            remoteGenerationCallerDied(requestId, replyBinder)
        }
        val lease = LocalAiRemoteGenerationLease(
            cancel = { runtime.cancel() },
            unlink = { replyBinder.unlinkToDeath(deathRecipient, 0) },
        )
        val registration = RemoteGenerationRegistration(
            requestId = requestId,
            replyBinder = replyBinder,
            lease = lease,
        )
        synchronized(remoteGenerationLock) {
            check(activeRemoteGeneration == null) { "A remote local generation is already active" }
            activeRemoteGeneration = registration
        }
        try {
            replyBinder.linkToDeath(deathRecipient, 0)
        } catch (error: RemoteException) {
            if (removeRemoteGeneration(registration)) lease.finish()
            throw LocalAiUnavailableException("Local AI IPC caller disconnected")
        }
        return registration
    }

    private fun generateRemote(
        registration: RemoteGenerationRegistration,
        request: LocalGenerationRequest,
    ): CompletableFuture<LocalGenerationResult> =
        loadActiveModel().thenCompose {
            registration.lease.runIfActive {
                runtime.generate(request) { chunk ->
                    registration.lease.runIfActive {
                        val sent = sendRemote(
                            Messenger(registration.replyBinder),
                            LocalAiRemoteProtocol.EVENT_CHUNK,
                            Bundle().apply {
                                putLong(LocalAiRemoteProtocol.KEY_REQUEST_ID, registration.requestId)
                                putString(LocalAiRemoteProtocol.KEY_CHUNK, chunk)
                            },
                        )
                        if (!sent) {
                            remoteGenerationCallerDied(registration.requestId, registration.replyBinder)
                        }
                    }
                }
            } ?: failedRemoteGeneration()
        }

    private fun replyRemoteGeneration(
        registration: RemoteGenerationRegistration,
        replyTo: Messenger,
        future: CompletableFuture<LocalGenerationResult>,
    ) {
        future.whenComplete { result, error ->
            if (!finishRemoteGeneration(registration)) return@whenComplete
            if (error != null) {
                sendRemoteError(registration.requestId, replyTo, rootCause(error))
            } else {
                sendRemote(
                    replyTo,
                    LocalAiRemoteProtocol.RESULT_OK,
                    Bundle().apply {
                        putLong(LocalAiRemoteProtocol.KEY_REQUEST_ID, registration.requestId)
                        putBundle(
                            LocalAiRemoteProtocol.KEY_RESULT,
                            LocalAiRemoteProtocol.generationResultToBundle(result),
                        )
                    },
                )
            }
        }
    }

    private fun cancelRemoteGeneration(replyTo: Messenger): CompletableFuture<LocalAiState> {
        val registration = synchronized(remoteGenerationLock) {
            val current = activeRemoteGeneration
                ?: return CompletableFuture.completedFuture(runtime.state())
            if (current.replyBinder != replyTo.binder) {
                return failedFuture(SecurityException("Local AI IPC cancel caller does not own active generation"))
            }
            activeRemoteGeneration = null
            current
        }
        registration.lease.finish()
        return runtime.cancel()
    }

    private fun remoteGenerationCallerDied(
        requestId: Long,
        replyBinder: IBinder,
    ) {
        val registration = synchronized(remoteGenerationLock) {
            val current = activeRemoteGeneration
            if (current?.requestId != requestId || current.replyBinder != replyBinder) {
                null
            } else {
                activeRemoteGeneration = null
                current
            }
        }
        registration?.lease?.callerDied()
    }

    private fun finishRemoteGeneration(registration: RemoteGenerationRegistration): Boolean {
        if (!removeRemoteGeneration(registration)) return false
        return registration.lease.finish()
    }

    private fun removeRemoteGeneration(registration: RemoteGenerationRegistration): Boolean =
        synchronized(remoteGenerationLock) {
            if (activeRemoteGeneration !== registration) {
                false
            } else {
                activeRemoteGeneration = null
                true
            }
        }

    private fun clearRemoteGeneration(): RemoteGenerationRegistration? =
        synchronized(remoteGenerationLock) {
            activeRemoteGeneration.also { activeRemoteGeneration = null }
        }

    private fun failedRemoteGeneration(): CompletableFuture<LocalGenerationResult> =
        failedFuture(CancellationException("Local AI IPC caller disconnected"))

    private fun <T> failedFuture(error: Throwable): CompletableFuture<T> =
        CompletableFuture<T>().also { it.completeExceptionally(error) }

    private data class RemoteGenerationRegistration(
        val requestId: Long,
        val replyBinder: IBinder,
        val lease: LocalAiRemoteGenerationLease,
    )

    private fun requireRemoteString(payload: Bundle, key: String): String =
        payload.getString(key)?.takeIf(String::isNotBlank)
            ?: throw IllegalArgumentException("Missing local AI IPC field: $key")

    private fun rootCause(error: Throwable): Throwable {
        var current = error
        while (current.cause != null && current.cause !== current) {
            current = checkNotNull(current.cause)
        }
        return current
    }

    private fun models(): CompletableFuture<List<LocalModelSpec>> =
        CompletableFuture.supplyAsync(modelStore::installedModels, modelIo)

    private fun activeModel(): CompletableFuture<LocalModelSpec?> =
        CompletableFuture.supplyAsync(modelStore::activeModel, modelIo)

    @Synchronized
    private fun selectTtsProvider(id: String): CompletableFuture<LocalTtsState> {
        val selected = ttsRegistry.provider(id)
        if (selected === tts) return selected.initialize()
        tts.stop()
        tts = selected
        return selected.initialize()
    }

    @Synchronized
    private fun loadActiveModel(): CompletableFuture<LocalAiState> {
        if (runtime.state().phase == LocalAiPhase.READY) {
            return CompletableFuture.completedFuture(runtime.state())
        }
        loading?.let { return it }
        val future = CompletableFuture.supplyAsync(
            {
                modelStore.activeModel()
                    ?.also(::requireEmbeddedFormat)
                    ?: throw LocalAiUnavailableException("No verified local model is installed")
            },
            modelIo,
        ).thenCompose(runtime::load)
        loading = future
        future.whenComplete { _, _ ->
            synchronized(this) {
                if (loading === future) loading = null
            }
        }
        return future
    }

    private fun installModel(
        source: InputStream,
        metadata: LocalModelMetadata,
    ): CompletableFuture<LocalModelSpec> {
        require(metadata.format == LocalModelFormat.LITERT_LM) {
            "Embedded provider accepts only ${LocalModelFormat.LITERT_LM.wireName} models"
        }
        return CompletableFuture.supplyAsync(
            { source.use { modelStore.install(it, metadata) } },
            modelIo,
        ).thenCompose { spec ->
            runtime.load(spec).thenApply { spec }
        }
    }

    private fun selectModel(
        id: String,
        version: String,
    ): CompletableFuture<LocalAiState> =
        CompletableFuture.supplyAsync(
            {
                modelStore.model(id, version)
                    ?.also(::requireEmbeddedFormat)
                    ?: throw LocalAiUnavailableException("Local model is not installed: $id@$version")
            },
            modelIo,
        ).thenCompose { spec ->
            runtime.load(spec).thenCompose { state ->
                CompletableFuture.supplyAsync(
                    {
                        modelStore.activate(spec)
                        state
                    },
                    modelIo,
                )
            }
        }

    private fun generate(
        request: LocalGenerationRequest,
        onChunk: (String) -> Unit,
    ): CompletableFuture<LocalGenerationResult> =
        loadActiveModel().thenCompose {
            runtime.generate(request, onChunk)
        }

    private fun requireEmbeddedFormat(spec: LocalModelSpec) {
        require(spec.format == LocalModelFormat.LITERT_LM) {
            "Embedded provider cannot execute ${spec.format.wireName} models"
        }
    }
}

class LocalAiUnavailableException(message: String) : IllegalStateException(message)
