package ai.zara.app.integration.notification

import ai.zara.app.ZaraApplication
import ai.zara.app.integration.AndroidAuthorityLevel
import ai.zara.app.integration.AndroidBackend
import ai.zara.app.integration.AndroidOperationBackend
import ai.zara.app.integration.AndroidOperationError
import ai.zara.app.integration.AndroidOperationRequest
import ai.zara.app.integration.AndroidOperationResult
import android.app.Notification
import android.app.RemoteInput
import android.content.Intent
import android.os.Bundle
import android.os.Process
import android.service.notification.NotificationListenerService
import android.service.notification.StatusBarNotification
import java.util.concurrent.atomic.AtomicReference

class ZaraNotificationListenerService : NotificationListenerService() {
    override fun onListenerConnected() {
        super.onListenerConnected()
        (application as ZaraApplication).androidIntegration.attachNotifications(this)
    }

    override fun onListenerDisconnected() {
        (application as ZaraApplication).androidIntegration.detachNotifications(this)
        super.onListenerDisconnected()
    }

    override fun onDestroy() {
        (application as ZaraApplication).androidIntegration.detachNotifications(this)
        super.onDestroy()
    }
}

class NotificationBackend : AndroidOperationBackend {
    private val listener = AtomicReference<ZaraNotificationListenerService?>(null)

    override val backend = AndroidBackend.NOTIFICATION
    override val minimumAuthority = AndroidAuthorityLevel.UNRESTRICTED
    override val identity: String
        get() = "notification_listener:app:${Process.myUid()}"

    fun attach(service: ZaraNotificationListenerService) {
        listener.set(service)
    }

    fun detach(service: ZaraNotificationListenerService) {
        listener.compareAndSet(service, null)
    }

    override fun isAvailable(): Boolean = listener.get() != null

    override fun supports(operation: String): Boolean = operation in OPERATIONS

    override fun execute(request: AndroidOperationRequest): AndroidOperationResult {
        val active = listener.get()
            ?: return AndroidOperationResult.failed(
                AndroidOperationError.BACKEND_UNAVAILABLE,
                backend = backend.atom,
            )
        return when (request.operation) {
            "notification.list" -> AndroidOperationResult.completed(
                backend.atom,
                identity,
                snapshot(active.activeNotifications.orEmpty()),
            )
            "notification.cancel" -> {
                val key = requireNotNull(request.arguments["key"]) { "notification key is required" }
                active.cancelNotification(key)
                AndroidOperationResult.completed(backend.atom, identity)
            }
            "notification.action" -> executeAction(active, request, reply = null)
            "notification.reply" -> executeAction(
                active,
                request,
                reply = requireNotNull(request.arguments["reply"]) { "reply text is required" },
            )
            else -> AndroidOperationResult.failed(
                AndroidOperationError.UNSUPPORTED_OPERATION,
                backend.atom,
                identity,
            )
        }
    }

    private fun executeAction(
        active: ZaraNotificationListenerService,
        request: AndroidOperationRequest,
        reply: String?,
    ): AndroidOperationResult {
        val notification = find(active, requireNotNull(request.arguments["key"]) {
            "notification key is required"
        }) ?: return AndroidOperationResult.failed(
            AndroidOperationError.FAILED,
            backend.atom,
            identity,
            "notification not found",
        )
        val actionIndex = requireNotNull(request.arguments["action_index"]?.toIntOrNull()) {
            "notification action_index is required"
        }
        val actions = notification.notification.actions.orEmpty()
        require(actionIndex in actions.indices) { "notification action_index is out of range" }
        val action = actions[actionIndex]
        val fillIn = if (reply == null) {
            null
        } else {
            val remoteInputs = action.remoteInputs.orEmpty()
            require(remoteInputs.isNotEmpty()) { "notification action does not accept remote input" }
            Intent().also { intent ->
                val results = Bundle()
                remoteInputs.forEach { input -> results.putCharSequence(input.resultKey, reply) }
                RemoteInput.addResultsToIntent(remoteInputs, intent, results)
            }
        }
        if (fillIn == null) {
            action.actionIntent.send()
        } else {
            action.actionIntent.send(active, 0, fillIn)
        }
        return AndroidOperationResult.completed(backend.atom, identity)
    }

    private fun find(
        active: ZaraNotificationListenerService,
        key: String,
    ): StatusBarNotification? = active.activeNotifications.orEmpty().firstOrNull { it.key == key }

    private fun snapshot(notifications: Array<StatusBarNotification>): String = buildString {
        notifications.take(MAX_NOTIFICATIONS).forEachIndexed { index, item ->
            if (index > 0) append('\n')
            val extras = item.notification.extras
            append("key=").append(escape(item.key))
            append(" package=").append(escape(item.packageName))
            append(" id=").append(item.id)
            append(" title=").append(quoted(extras.getCharSequence(Notification.EXTRA_TITLE)))
            append(" text=").append(quoted(extras.getCharSequence(Notification.EXTRA_TEXT)))
            append(" big_text=").append(quoted(extras.getCharSequence(Notification.EXTRA_BIG_TEXT)))
            append(" actions=")
            append(
                item.notification.actions.orEmpty().mapIndexed { actionIndex, action ->
                    val remote = action.remoteInputs.orEmpty().joinToString(",") { it.resultKey }
                    "$actionIndex:${escape(action.title?.toString() ?: "")}:remote=[$remote]"
                }.joinToString("|")
            )
        }
    }.take(MAX_SNAPSHOT_CHARS)

    private fun escape(value: String): String = value.replace(' ', '_').replace('\n', '_').take(4096)

    private fun quoted(value: CharSequence?): String {
        val text = value?.toString()?.take(16_384) ?: ""
        return "\"${text.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")}\""
    }

    private companion object {
        val OPERATIONS = setOf(
            "notification.list",
            "notification.cancel",
            "notification.action",
            "notification.reply",
        )
        const val MAX_NOTIFICATIONS = 256
        const val MAX_SNAPSHOT_CHARS = 256 * 1024
    }
}
