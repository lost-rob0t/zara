package ai.zara.prolog.ipc

import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.content.ServiceConnection
import android.os.IBinder
import org.json.JSONObject
import java.util.UUID
import java.util.concurrent.CompletableFuture

data class PrologQueryReply(
    val requestId: String,
    val generation: Long,
    val terms: List<String>,
    val truncated: Boolean,
)

class PrologCall internal constructor(
    val requestId: String,
    val result: CompletableFuture<PrologQueryReply>,
    private val cancelAction: () -> Unit,
) {
    fun cancel() {
        cancelAction()
    }
}

class PrologClient(
    context: Context,
) : AutoCloseable {
    companion object {
        const val API_VERSION = 1
        const val SERVICE_PACKAGE = "ai.zara.app"
        const val SERVICE_CLASS = "ai.zara.app.prolog.ipc.PrologIpcService"
        const val ACTION_BIND = "ai.zara.prolog.BIND"
        const val PERMISSION = "ai.zara.permission.PROLOG"
        const val MAX_GOAL_CHARS = 8_192
        const val MAX_TIMEOUT_MS = 15_000L
    }

    private val appContext = context.applicationContext
    private val lock = Any()

    @Volatile
    private var closed = false

    private var service: IPrologService? = null
    private var pendingBind: CompletableFuture<IPrologService>? = null
    private var bound = false

    private val connection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName?, binder: IBinder?) {
            val remote = IPrologService.Stub.asInterface(binder)
            synchronized(lock) {
                if (closed) return
                if (remote == null) {
                    pendingBind?.completeExceptionally(
                        IllegalStateException("Zara Prolog service returned an incompatible binder")
                    )
                    pendingBind = null
                    return
                }
                service = remote
                pendingBind?.complete(remote)
                pendingBind = null
            }
        }

        override fun onServiceDisconnected(name: ComponentName?) {
            synchronized(lock) {
                service = null
            }
        }

        override fun onBindingDied(name: ComponentName?) {
            synchronized(lock) {
                service = null
                pendingBind?.completeExceptionally(IllegalStateException("Zara Prolog service binding died"))
                pendingBind = null
                bound = false
            }
        }

        override fun onNullBinding(name: ComponentName?) {
            synchronized(lock) {
                service = null
                pendingBind?.completeExceptionally(IllegalStateException("Zara Prolog service returned null binding"))
                pendingBind = null
                bound = false
            }
        }
    }

    fun capabilities(): CompletableFuture<String> =
        service().thenApply { remote ->
            check(remote.getApiVersion() == API_VERSION) {
                "Unsupported Zara Prolog API version: ${remote.getApiVersion()}"
            }
            remote.getCapabilitiesJson()
        }

    fun query(
        goal: String,
        timeoutMs: Long = 5_000,
    ): PrologCall {
        val normalized = goal.trim()
        require(normalized.isNotEmpty()) { "Prolog goal is required" }
        require(normalized.length <= MAX_GOAL_CHARS) { "Prolog goal is too large" }
        val boundedTimeout = timeoutMs.coerceIn(1, MAX_TIMEOUT_MS)
        val requestId = UUID.randomUUID().toString()
        val future = CompletableFuture<PrologQueryReply>()

        service().whenComplete { remote, bindError ->
            if (bindError != null) {
                future.completeExceptionally(bindError)
                return@whenComplete
            }
            try {
                check(remote.getApiVersion() == API_VERSION) {
                    "Unsupported Zara Prolog API version: ${remote.getApiVersion()}"
                }
                remote.query(
                    requestId,
                    normalized,
                    System.currentTimeMillis() + boundedTimeout,
                    object : IPrologCallback.Stub() {
                        override fun onResult(resultRequestId: String, resultJson: String) {
                            if (resultRequestId != requestId || future.isDone) return
                            runCatching { parseReply(resultJson) }
                                .onSuccess(future::complete)
                                .onFailure(future::completeExceptionally)
                        }

                        override fun onError(
                            resultRequestId: String,
                            errorCode: String,
                            message: String,
                        ) {
                            if (resultRequestId != requestId || future.isDone) return
                            future.completeExceptionally(
                                IllegalStateException("$errorCode: $message")
                            )
                        }
                    },
                )
            } catch (error: Throwable) {
                future.completeExceptionally(error)
            }
        }

        return PrologCall(
            requestId = requestId,
            result = future,
            cancelAction = {
                synchronized(lock) {
                    runCatching { service?.cancel(requestId) }
                }
                future.cancel(false)
            },
        )
    }

    private fun service(): CompletableFuture<IPrologService> {
        synchronized(lock) {
            if (closed) {
                return CompletableFuture<IPrologService>().also {
                    it.completeExceptionally(IllegalStateException("Zara Prolog client is closed"))
                }
            }
            service?.let { return CompletableFuture.completedFuture(it) }
            pendingBind?.let { return it }

            val future = CompletableFuture<IPrologService>()
            pendingBind = future
            val intent = Intent(ACTION_BIND).apply {
                component = ComponentName(SERVICE_PACKAGE, SERVICE_CLASS)
            }
            val accepted = appContext.bindService(intent, connection, Context.BIND_AUTO_CREATE)
            if (!accepted) {
                pendingBind = null
                future.completeExceptionally(
                    IllegalStateException("Zara Prolog service is unavailable or not installed")
                )
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
            pendingBind?.completeExceptionally(IllegalStateException("Zara Prolog client closed"))
            pendingBind = null
            service = null
            if (bound) {
                runCatching { appContext.unbindService(connection) }
                bound = false
            }
        }
    }

    private fun parseReply(json: String): PrologQueryReply {
        val root = JSONObject(json)
        val termsJson = root.getJSONArray("terms")
        val terms = buildList {
            for (index in 0 until termsJson.length()) add(termsJson.getString(index))
        }
        return PrologQueryReply(
            requestId = root.getString("request_id"),
            generation = root.getLong("generation"),
            terms = terms,
            truncated = root.optBoolean("truncated", false),
        )
    }
}
