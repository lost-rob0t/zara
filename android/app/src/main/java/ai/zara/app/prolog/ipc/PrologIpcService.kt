package ai.zara.app.prolog.ipc

import ai.zara.app.ZaraApplication
import ai.zara.prolog.ipc.IPrologCallback
import ai.zara.prolog.ipc.IPrologService
import android.app.Service
import android.content.Intent
import android.os.Handler
import android.os.IBinder
import android.os.Looper
import org.json.JSONArray
import org.json.JSONObject
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicBoolean

class PrologIpcService : Service() {
    companion object {
        private const val API_VERSION = 1
    }

    private data class RequestState(
        val cancelled: AtomicBoolean = AtomicBoolean(false),
        val deadlineEpochMs: Long,
    )

    private val requests = ConcurrentHashMap<String, RequestState>()
    private val mainHandler by lazy { Handler(Looper.getMainLooper()) }

    private val binder = object : IPrologService.Stub() {
        override fun getApiVersion(): Int = API_VERSION

        override fun getCapabilitiesJson(): String = JSONObject()
            .put("protocol", "ZARA-PROLOG/1")
            .put("api_version", API_VERSION)
            .put("runtime", "canonical_local_trealla")
            .put("operations", JSONArray(listOf("query", "cancel")))
            .put("max_goal_chars", PrologIpcPolicy.MAX_GOAL_CHARS)
            .put("max_timeout_ms", PrologIpcPolicy.MAX_TIMEOUT_MS)
            .put("max_terms", PrologIpcPolicy.MAX_TERMS)
            .toString()

        override fun query(
            requestId: String,
            goal: String,
            deadlineEpochMs: Long,
            callback: IPrologCallback,
        ) {
            val id = try {
                PrologIpcPolicy.requireRequestId(requestId)
            } catch (error: Throwable) {
                safeError(callback, requestId.take(96), "invalid_request", error.message ?: "Invalid request")
                return
            }
            val normalizedGoal = try {
                PrologIpcPolicy.requireGoal(goal)
            } catch (error: Throwable) {
                safeError(callback, id, "invalid_goal", error.message ?: "Invalid goal")
                return
            }
            val deadline = try {
                PrologIpcPolicy.requireDeadline(deadlineEpochMs)
            } catch (error: Throwable) {
                safeError(callback, id, "invalid_deadline", error.message ?: "Invalid deadline")
                return
            }

            val state = RequestState(deadlineEpochMs = deadline)
            if (requests.putIfAbsent(id, state) != null) {
                safeError(callback, id, "duplicate_request", "Request id is already active")
                return
            }

            val delay = (deadline - System.currentTimeMillis()).coerceAtLeast(1)
            mainHandler.postDelayed({
                val active = requests[id]
                if (active === state && active.cancelled.compareAndSet(false, true)) {
                    requests.remove(id, state)
                    safeError(callback, id, "deadline_exceeded", "Prolog request exceeded its deadline")
                }
            }, delay)

            val session = (application as ZaraApplication).appSession
            session.queryLocalProlog(normalizedGoal).whenComplete { result, error ->
                val active = requests[id]
                if (active !== state || state.cancelled.get()) return@whenComplete
                requests.remove(id, state)
                if (System.currentTimeMillis() > state.deadlineEpochMs) {
                    state.cancelled.set(true)
                    safeError(callback, id, "deadline_exceeded", "Prolog result arrived after its deadline")
                    return@whenComplete
                }
                if (error != null) {
                    safeError(callback, id, "query_failed", boundedMessage(error))
                    return@whenComplete
                }
                if (result == null) {
                    safeError(callback, id, "query_failed", "Prolog runtime returned no result")
                    return@whenComplete
                }
                val projected = PrologIpcPolicy.projectTerms(result.terms)
                val payload = JSONObject()
                    .put("request_id", id)
                    .put("generation", result.generation)
                    .put("terms", JSONArray(projected.terms))
                    .put("truncated", projected.truncated)
                    .toString()
                runCatching { callback.onResult(id, payload) }
            }
        }

        override fun cancel(requestId: String) {
            val id = runCatching { PrologIpcPolicy.requireRequestId(requestId) }.getOrNull() ?: return
            requests.remove(id)?.cancelled?.set(true)
        }
    }

    override fun onBind(intent: Intent?): IBinder = binder

    override fun onDestroy() {
        requests.values.forEach { it.cancelled.set(true) }
        requests.clear()
        super.onDestroy()
    }

    private fun safeError(
        callback: IPrologCallback,
        requestId: String,
        code: String,
        message: String,
    ) {
        runCatching { callback.onError(requestId, code, message.take(512)) }
    }

    private fun boundedMessage(error: Throwable): String =
        (error.message ?: error::class.java.simpleName).take(512)
}
