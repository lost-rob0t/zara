package ai.zara.app.org

import ai.zara.app.ZaraApplication
import ai.zara.ui.ipc.OrgNotebookIpc
import android.app.Service
import android.content.Intent
import android.os.Bundle
import android.os.Handler
import android.os.IBinder
import android.os.Looper
import android.os.Message
import android.os.Messenger
import java.util.concurrent.ConcurrentHashMap

/**
 * Same-signer bridge for the separate Org Notebook APK.
 *
 * This service runs in Zara's existing :voice process and delegates Prolog work to
 * the singleton AndroidAppSession, so it never creates a second Trealla runtime.
 */
class OrgNotebookExecutionService : Service() {
    private val cancelled = ConcurrentHashMap.newKeySet<String>()
    private val incoming = Messenger(IncomingHandler())

    override fun onBind(intent: Intent?): IBinder = incoming.binder

    private inner class IncomingHandler : Handler(Looper.getMainLooper()) {
        override fun handleMessage(message: Message) {
            when (message.what) {
                OrgNotebookIpc.MSG_RUN -> handleRun(message)
                OrgNotebookIpc.MSG_CANCEL -> {
                    message.data.getString(OrgNotebookIpc.KEY_REQUEST_ID)?.let(cancelled::add)
                }
                else -> super.handleMessage(message)
            }
        }
    }

    private fun handleRun(message: Message) {
        val reply = message.replyTo ?: return
        val data = message.data
        val requestId = data.getString(OrgNotebookIpc.KEY_REQUEST_ID).orEmpty()
        val sourceRevision = data.getLong(OrgNotebookIpc.KEY_SOURCE_REVISION, -1L)
        val blockHash = data.getString(OrgNotebookIpc.KEY_BLOCK_HASH).orEmpty()
        val language = data.getString(OrgNotebookIpc.KEY_LANGUAGE).orEmpty().lowercase()
        val body = data.getString(OrgNotebookIpc.KEY_BODY).orEmpty()
        val principal = data.getString(OrgNotebookIpc.KEY_PRINCIPAL).orEmpty()
        val deadline = data.getLong(OrgNotebookIpc.KEY_DEADLINE_EPOCH_MS, 0L)

        if (requestId.isBlank() || sourceRevision < 0 || blockHash.isBlank() || principal.isBlank()) {
            sendFailure(reply, requestId, sourceRevision, blockHash, "Invalid notebook execution request")
            return
        }
        if (body.isBlank() || body.length > MAX_BODY_CHARS) {
            sendFailure(reply, requestId, sourceRevision, blockHash, "Notebook block is empty or too large")
            return
        }
        if (deadline > 0 && System.currentTimeMillis() > deadline) {
            sendFailure(reply, requestId, sourceRevision, blockHash, "Notebook execution deadline expired")
            return
        }
        cancelled.remove(requestId)

        if (language != "prolog") {
            sendFailure(reply, requestId, sourceRevision, blockHash, "No trusted $language notebook provider is available")
            return
        }

        val started = System.nanoTime()
        val session = (application as ZaraApplication).appSession
        session.queryLocalProlog(body).whenComplete { result, error ->
            val durationMs = (System.nanoTime() - started) / 1_000_000
            if (cancelled.remove(requestId)) {
                sendResult(
                    reply = reply,
                    requestId = requestId,
                    sourceRevision = sourceRevision,
                    blockHash = blockHash,
                    status = OrgNotebookIpc.STATUS_CANCELLED,
                    stderr = "Execution cancelled",
                    durationMs = durationMs,
                    runtimeGeneration = result?.generation ?: session.localServerState().generation,
                )
            } else if (error != null) {
                val root = error.cause ?: error
                sendFailure(
                    reply,
                    requestId,
                    sourceRevision,
                    blockHash,
                    root.message ?: root::class.java.simpleName,
                    durationMs,
                    session.localServerState().generation,
                )
            } else {
                sendResult(
                    reply = reply,
                    requestId = requestId,
                    sourceRevision = sourceRevision,
                    blockHash = blockHash,
                    status = OrgNotebookIpc.STATUS_SUCCEEDED,
                    stdout = result.terms.joinToString("\n"),
                    durationMs = durationMs,
                    runtimeGeneration = result.generation,
                )
            }
        }
    }

    private fun sendFailure(
        reply: Messenger,
        requestId: String,
        sourceRevision: Long,
        blockHash: String,
        error: String,
        durationMs: Long = 0,
        runtimeGeneration: Long = 0,
    ) = sendResult(
        reply = reply,
        requestId = requestId,
        sourceRevision = sourceRevision,
        blockHash = blockHash,
        status = OrgNotebookIpc.STATUS_FAILED,
        stderr = error,
        durationMs = durationMs,
        runtimeGeneration = runtimeGeneration,
    )

    private fun sendResult(
        reply: Messenger,
        requestId: String,
        sourceRevision: Long,
        blockHash: String,
        status: String,
        stdout: String = "",
        stderr: String = "",
        durationMs: Long,
        runtimeGeneration: Long,
    ) {
        val response = Message.obtain(null, OrgNotebookIpc.MSG_RESULT).apply {
            data = Bundle().apply {
                putString(OrgNotebookIpc.KEY_REQUEST_ID, requestId)
                putLong(OrgNotebookIpc.KEY_SOURCE_REVISION, sourceRevision)
                putString(OrgNotebookIpc.KEY_BLOCK_HASH, blockHash)
                putString(OrgNotebookIpc.KEY_STATUS, status)
                putString(OrgNotebookIpc.KEY_STDOUT, stdout)
                putString(OrgNotebookIpc.KEY_STDERR, stderr)
                putLong(OrgNotebookIpc.KEY_DURATION_MS, durationMs)
                putLong(OrgNotebookIpc.KEY_RUNTIME_GENERATION, runtimeGeneration)
            }
        }
        runCatching { reply.send(response) }
    }

    companion object {
        private const val MAX_BODY_CHARS = 64 * 1024
    }
}
