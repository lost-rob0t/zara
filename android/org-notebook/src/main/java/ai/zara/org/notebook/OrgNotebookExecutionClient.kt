package ai.zara.org.notebook

import ai.zara.org.core.OrgExecutionRequest
import ai.zara.org.core.OrgExecutionResult
import ai.zara.org.core.OrgExecutionStatus
import ai.zara.ui.ipc.OrgNotebookIpc
import android.content.ComponentName
import android.content.Context
import android.content.Intent
import android.content.ServiceConnection
import android.os.Bundle
import android.os.Handler
import android.os.IBinder
import android.os.Looper
import android.os.Message
import android.os.Messenger

class OrgNotebookExecutionClient(
    context: Context,
    private val onConnectionChanged: (Boolean) -> Unit,
    private val onResult: (OrgExecutionResult) -> Unit,
    private val onError: (String) -> Unit,
) : AutoCloseable {
    private val appContext = context.applicationContext
    private var remote: Messenger? = null
    private var bound = false

    private val replies = Messenger(object : Handler(Looper.getMainLooper()) {
        override fun handleMessage(message: Message) {
            if (message.what != OrgNotebookIpc.MSG_RESULT) return super.handleMessage(message)
            val data = message.data
            val requestId = data.getString(OrgNotebookIpc.KEY_REQUEST_ID).orEmpty()
            val sourceRevision = data.getLong(OrgNotebookIpc.KEY_SOURCE_REVISION, -1L)
            val blockHash = data.getString(OrgNotebookIpc.KEY_BLOCK_HASH).orEmpty()
            if (requestId.isBlank() || sourceRevision < 0 || blockHash.isBlank()) {
                onError("Malformed notebook execution response")
                return
            }
            val status = when (data.getString(OrgNotebookIpc.KEY_STATUS)) {
                OrgNotebookIpc.STATUS_SUCCEEDED -> OrgExecutionStatus.SUCCEEDED
                OrgNotebookIpc.STATUS_CANCELLED -> OrgExecutionStatus.CANCELLED
                else -> OrgExecutionStatus.FAILED
            }
            onResult(
                OrgExecutionResult(
                    requestId = requestId,
                    sourceRevision = sourceRevision,
                    blockHash = blockHash,
                    status = status,
                    stdout = data.getString(OrgNotebookIpc.KEY_STDOUT).orEmpty(),
                    stderr = data.getString(OrgNotebookIpc.KEY_STDERR).orEmpty(),
                    structuredValue = data.getString(OrgNotebookIpc.KEY_STRUCTURED_VALUE),
                    durationMs = data.getLong(OrgNotebookIpc.KEY_DURATION_MS, 0L),
                    runtimeGeneration = data.getLong(OrgNotebookIpc.KEY_RUNTIME_GENERATION, 0L),
                ),
            )
        }
    })

    private val connection = object : ServiceConnection {
        override fun onServiceConnected(name: ComponentName?, service: IBinder?) {
            remote = service?.let(::Messenger)
            bound = remote != null
            onConnectionChanged(bound)
        }

        override fun onServiceDisconnected(name: ComponentName?) {
            remote = null
            bound = false
            onConnectionChanged(false)
        }

        override fun onBindingDied(name: ComponentName?) {
            remote = null
            bound = false
            onConnectionChanged(false)
        }

        override fun onNullBinding(name: ComponentName?) {
            remote = null
            bound = false
            onConnectionChanged(false)
            onError("Zara notebook execution service refused the connection")
        }
    }

    fun bind(): Boolean {
        if (bound) return true
        val intent = Intent().setComponent(
            ComponentName(OrgNotebookIpc.ZARA_PACKAGE, OrgNotebookIpc.SERVICE_CLASS),
        )
        return runCatching {
            appContext.bindService(intent, connection, Context.BIND_AUTO_CREATE)
        }.onFailure { onError(it.message ?: "Unable to bind Zara runtime") }
            .getOrDefault(false)
    }

    fun run(request: OrgExecutionRequest): Boolean {
        val target = remote ?: return false
        val message = Message.obtain(null, OrgNotebookIpc.MSG_RUN).apply {
            replyTo = replies
            data = Bundle().apply {
                putString(OrgNotebookIpc.KEY_REQUEST_ID, request.requestId)
                putString(OrgNotebookIpc.KEY_DOCUMENT_ID, request.documentId)
                putLong(OrgNotebookIpc.KEY_SOURCE_REVISION, request.sourceRevision)
                putString(OrgNotebookIpc.KEY_BLOCK_HASH, request.blockHash)
                putString(OrgNotebookIpc.KEY_LANGUAGE, request.language)
                putString(OrgNotebookIpc.KEY_BODY, request.body)
                putString(OrgNotebookIpc.KEY_PRINCIPAL, request.principal)
                request.deadlineEpochMs?.let { putLong(OrgNotebookIpc.KEY_DEADLINE_EPOCH_MS, it) }
            }
        }
        return runCatching { target.send(message) }
            .onFailure { onError(it.message ?: "Unable to send notebook execution request") }
            .isSuccess
    }

    fun cancel(requestId: String): Boolean {
        val target = remote ?: return false
        val message = Message.obtain(null, OrgNotebookIpc.MSG_CANCEL).apply {
            data = Bundle().apply { putString(OrgNotebookIpc.KEY_REQUEST_ID, requestId) }
        }
        return runCatching { target.send(message) }
            .onFailure { onError(it.message ?: "Unable to cancel notebook execution") }
            .isSuccess
    }

    override fun close() {
        if (bound) runCatching { appContext.unbindService(connection) }
        remote = null
        bound = false
    }
}
