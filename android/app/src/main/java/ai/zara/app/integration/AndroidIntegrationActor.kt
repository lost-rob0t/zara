package ai.zara.app.integration

import java.util.concurrent.CompletableFuture
import java.util.concurrent.ExecutorService
import java.util.concurrent.Executors

data class AndroidOperationRequest(
    val backend: String = "auto",
    val operation: String,
    val arguments: Map<String, String> = emptyMap(),
) {
    init {
        require(backend == "auto" || backend.matches(ATOM)) { "Android backend is invalid" }
        require(operation.matches(OPERATION)) { "Android operation is invalid" }
        require(arguments.size <= MAX_ARGUMENTS) { "Android operation has too many arguments" }
        arguments.forEach { (key, value) ->
            require(key.matches(ARGUMENT_KEY)) { "Android argument key is invalid" }
            require(value.encodeToByteArray().size <= MAX_ARGUMENT_BYTES) {
                "Android argument value is too large"
            }
        }
    }

    companion object {
        private val ATOM = Regex("[a-z][a-z0-9_]{0,63}")
        private val OPERATION = Regex("[a-z][a-z0-9_.:-]{0,127}")
        private val ARGUMENT_KEY = Regex("[a-zA-Z][a-zA-Z0-9_.:-]{0,127}")
        private const val MAX_ARGUMENTS = 128
        private const val MAX_ARGUMENT_BYTES = 256 * 1024
    }
}

enum class AndroidOperationError {
    INVALID_BACKEND,
    AUTHORITY_DENIED,
    BACKEND_UNAVAILABLE,
    UNSUPPORTED_OPERATION,
    FAILED,
    CANCELLED,
}

data class AndroidOperationResult(
    val success: Boolean,
    val backend: String? = null,
    val identity: String? = null,
    val output: String? = null,
    val error: AndroidOperationError? = null,
    val message: String? = null,
) {
    companion object {
        fun completed(
            backend: String,
            identity: String,
            output: String? = null,
        ): AndroidOperationResult = AndroidOperationResult(
            success = true,
            backend = backend,
            identity = identity,
            output = output,
        )

        fun failed(
            error: AndroidOperationError,
            backend: String? = null,
            identity: String? = null,
            message: String? = null,
        ): AndroidOperationResult = AndroidOperationResult(
            success = false,
            backend = backend,
            identity = identity,
            error = error,
            message = message,
        )
    }
}

interface AndroidOperationBackend {
    val backend: AndroidBackend
    val minimumAuthority: AndroidAuthorityLevel
    val identity: String

    fun isAvailable(): Boolean
    fun supports(operation: String): Boolean
    fun execute(request: AndroidOperationRequest): AndroidOperationResult
    fun cancel() = Unit
}

class AndroidIntegrationActor(
    private val policy: AndroidAuthorityPolicy,
    backends: List<AndroidOperationBackend>,
    private val executor: ExecutorService = Executors.newSingleThreadExecutor { runnable ->
        Thread(runnable, "zara-android-integration").apply { isDaemon = true }
    },
) : AutoCloseable {
    private val backendsByKind: Map<AndroidBackend, AndroidOperationBackend>
    @Volatile private var closed = false

    init {
        val duplicate = backends
            .groupingBy(AndroidOperationBackend::backend)
            .eachCount()
            .entries
            .firstOrNull { it.value > 1 }
        require(duplicate == null) { "duplicate Android operation backend" }
        backendsByKind = backends.associateBy(AndroidOperationBackend::backend)
    }

    fun authority(): AndroidAuthoritySnapshot = policy.snapshot()

    fun backendStatus(): List<AndroidBackendStatus> {
        val snapshot = policy.snapshot()
        return AndroidBackend.entries.map { kind ->
            val adapter = backendsByKind[kind]
            AndroidBackendStatus(
                backend = kind,
                authority = snapshot.levelFor(kind),
                installed = adapter != null,
                available = adapter?.isAvailable() == true,
                identity = if (adapter?.isAvailable() == true) adapter.identity else null,
            )
        }
    }

    fun execute(request: AndroidOperationRequest): CompletableFuture<AndroidOperationResult> {
        if (closed) {
            return CompletableFuture.completedFuture(
                AndroidOperationResult.failed(AndroidOperationError.CANCELLED, message = "integration actor is closed"),
            )
        }
        val future = CompletableFuture<AndroidOperationResult>()
        executor.execute {
            if (future.isCancelled || closed) {
                future.complete(AndroidOperationResult.failed(AndroidOperationError.CANCELLED))
                return@execute
            }
            try {
                future.complete(executeNow(request))
            } catch (_: SecurityException) {
                future.complete(
                    AndroidOperationResult.failed(
                        AndroidOperationError.AUTHORITY_DENIED,
                        message = "Android rejected the operation for this process identity",
                    ),
                )
            } catch (error: Throwable) {
                future.complete(
                    AndroidOperationResult.failed(
                        AndroidOperationError.FAILED,
                        message = error::class.java.simpleName,
                    ),
                )
            }
        }
        return future
    }

    private fun executeNow(request: AndroidOperationRequest): AndroidOperationResult {
        val snapshot = policy.snapshot()
        if (request.backend != "auto") {
            val kind = try {
                AndroidBackend.fromAtom(request.backend)
            } catch (_: IllegalArgumentException) {
                return AndroidOperationResult.failed(AndroidOperationError.INVALID_BACKEND)
            }
            val adapter = backendsByKind[kind]
                ?: return AndroidOperationResult.failed(
                    AndroidOperationError.BACKEND_UNAVAILABLE,
                    backend = kind.atom,
                )
            if (!snapshot.allows(kind, adapter.minimumAuthority)) {
                return AndroidOperationResult.failed(
                    AndroidOperationError.AUTHORITY_DENIED,
                    backend = kind.atom,
                    identity = adapter.identity,
                )
            }
            if (!adapter.isAvailable()) {
                return AndroidOperationResult.failed(
                    AndroidOperationError.BACKEND_UNAVAILABLE,
                    backend = kind.atom,
                )
            }
            if (!adapter.supports(request.operation)) {
                return AndroidOperationResult.failed(
                    AndroidOperationError.UNSUPPORTED_OPERATION,
                    backend = kind.atom,
                    identity = adapter.identity,
                )
            }
            return adapter.execute(request)
        }

        val supporting = PRIORITY.asSequence()
            .mapNotNull(backendsByKind::get)
            .filter { adapter -> adapter.supports(request.operation) }
            .toList()
        if (supporting.isEmpty()) {
            return AndroidOperationResult.failed(AndroidOperationError.UNSUPPORTED_OPERATION)
        }
        val authorized = supporting.filter { adapter ->
            snapshot.allows(adapter.backend, adapter.minimumAuthority)
        }
        if (authorized.isEmpty()) {
            return AndroidOperationResult.failed(AndroidOperationError.AUTHORITY_DENIED)
        }
        val available = authorized.firstOrNull(AndroidOperationBackend::isAvailable)
            ?: return AndroidOperationResult.failed(AndroidOperationError.BACKEND_UNAVAILABLE)
        return available.execute(request)
    }

    override fun close() {
        if (closed) return
        closed = true
        backendsByKind.values.forEach(AndroidOperationBackend::cancel)
        executor.shutdownNow()
    }

    private companion object {
        val PRIORITY = listOf(
            AndroidBackend.ROOT,
            AndroidBackend.SHIZUKU,
            AndroidBackend.DEVICE_POLICY,
            AndroidBackend.HIDDEN_API,
            AndroidBackend.ACCESSIBILITY,
            AndroidBackend.NOTIFICATION,
            AndroidBackend.IME,
            AndroidBackend.INTENT,
            AndroidBackend.SHELL,
            AndroidBackend.APP_FUNCTIONS,
        )
    }
}

data class AndroidBackendStatus(
    val backend: AndroidBackend,
    val authority: AndroidAuthorityLevel,
    val installed: Boolean,
    val available: Boolean,
    val identity: String?,
)
