package ai.zara.app.integration.accessibility

import ai.zara.app.ZaraApplication
import ai.zara.app.integration.AndroidAuthorityLevel
import ai.zara.app.integration.AndroidBackend
import ai.zara.app.integration.AndroidOperationBackend
import ai.zara.app.integration.AndroidOperationError
import ai.zara.app.integration.AndroidOperationRequest
import ai.zara.app.integration.AndroidOperationResult
import android.accessibilityservice.AccessibilityService
import android.accessibilityservice.GestureDescription
import android.graphics.Path
import android.os.Bundle
import android.os.Handler
import android.os.Looper
import android.os.Process
import android.view.accessibility.AccessibilityEvent
import android.view.accessibility.AccessibilityNodeInfo
import java.util.concurrent.FutureTask
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicReference

class ZaraAccessibilityService : AccessibilityService() {
    private val mainHandler = Handler(Looper.getMainLooper())

    override fun onServiceConnected() {
        super.onServiceConnected()
        (application as ZaraApplication).androidIntegration.attachAccessibility(this)
    }

    override fun onAccessibilityEvent(event: AccessibilityEvent?) = Unit

    override fun onInterrupt() = Unit

    override fun onDestroy() {
        (application as ZaraApplication).androidIntegration.detachAccessibility(this)
        super.onDestroy()
    }

    internal fun <T> callOnServiceThread(block: ZaraAccessibilityService.() -> T): T {
        if (Looper.myLooper() == Looper.getMainLooper()) return block()
        val task = FutureTask { block() }
        mainHandler.post(task)
        return task.get(5, TimeUnit.SECONDS)
    }
}

class AccessibilityBackend : AndroidOperationBackend {
    private val service = AtomicReference<ZaraAccessibilityService?>(null)

    override val backend = AndroidBackend.ACCESSIBILITY
    override val minimumAuthority = AndroidAuthorityLevel.UNRESTRICTED
    override val identity: String
        get() = "accessibility:app:${Process.myUid()}"

    fun attach(value: ZaraAccessibilityService) {
        service.set(value)
    }

    fun detach(value: ZaraAccessibilityService) {
        service.compareAndSet(value, null)
    }

    override fun isAvailable(): Boolean = service.get() != null

    override fun supports(operation: String): Boolean = operation in OPERATIONS

    override fun execute(request: AndroidOperationRequest): AndroidOperationResult {
        val active = service.get()
            ?: return AndroidOperationResult.failed(
                AndroidOperationError.BACKEND_UNAVAILABLE,
                backend = backend.atom,
            )
        return active.callOnServiceThread {
            when (request.operation) {
                "accessibility.snapshot" -> AndroidOperationResult.completed(
                    backend.atom,
                    identity,
                    rootInActiveWindow?.let(::snapshot) ?: "null",
                )
                "accessibility.global_action" -> {
                    val action = parseGlobalAction(request.arguments["action"])
                    booleanResult(performGlobalAction(action), "global action rejected")
                }
                "accessibility.node_action" -> {
                    val node = resolveNode(rootInActiveWindow, request.arguments["path"] ?: "")
                        ?: return@callOnServiceThread AndroidOperationResult.failed(
                            AndroidOperationError.FAILED,
                            backend.atom,
                            identity,
                            "node path not found",
                        )
                    val action = requireNotNull(request.arguments["action"]?.toIntOrNull()) {
                        "numeric accessibility action is required"
                    }
                    val bundle = Bundle()
                    request.arguments.forEach { (key, value) ->
                        when {
                            key == "text" -> bundle.putCharSequence(
                                AccessibilityNodeInfo.ACTION_ARGUMENT_SET_TEXT_CHARSEQUENCE,
                                value,
                            )
                            key.startsWith("arg.") -> bundle.putCharSequence(
                                key.removePrefix("arg."),
                                value,
                            )
                        }
                    }
                    booleanResult(node.performAction(action, bundle), "node action rejected")
                }
                "accessibility.gesture" -> {
                    val path = parseGesturePath(requireNotNull(request.arguments["points"]) {
                        "gesture points are required"
                    })
                    val startMs = request.arguments["start_ms"]?.toLongOrNull()?.coerceAtLeast(0L) ?: 0L
                    val durationMs = request.arguments["duration_ms"]
                        ?.toLongOrNull()
                        ?.coerceIn(1L, 60_000L)
                        ?: 250L
                    val gesture = GestureDescription.Builder()
                        .addStroke(GestureDescription.StrokeDescription(path, startMs, durationMs))
                        .build()
                    booleanResult(dispatchGesture(gesture, null, null), "gesture dispatch rejected")
                }
                else -> AndroidOperationResult.failed(
                    AndroidOperationError.UNSUPPORTED_OPERATION,
                    backend.atom,
                    identity,
                )
            }
        }
    }

    private fun booleanResult(ok: Boolean, message: String): AndroidOperationResult =
        if (ok) {
            AndroidOperationResult.completed(backend.atom, identity)
        } else {
            AndroidOperationResult.failed(
                AndroidOperationError.FAILED,
                backend.atom,
                identity,
                message,
            )
        }

    private fun resolveNode(root: AccessibilityNodeInfo?, path: String): AccessibilityNodeInfo? {
        var node = root ?: return null
        if (path.isBlank()) return node
        path.split('/').filter(String::isNotBlank).forEach { segment ->
            val index = segment.toIntOrNull() ?: return null
            if (index < 0 || index >= node.childCount) return null
            node = node.getChild(index) ?: return null
        }
        return node
    }

    private fun snapshot(root: AccessibilityNodeInfo): String {
        val builder = StringBuilder(16_384)
        var emitted = 0

        fun walk(node: AccessibilityNodeInfo, path: String, depth: Int) {
            if (emitted >= MAX_NODES || builder.length >= MAX_SNAPSHOT_CHARS) return
            if (emitted > 0) builder.append('\n')
            builder.append(path.ifEmpty { "root" })
                .append(" depth=").append(depth)
                .append(" class=").append(safe(node.className))
                .append(" id=").append(safe(node.viewIdResourceName))
                .append(" text=").append(quoted(node.text))
                .append(" desc=").append(quoted(node.contentDescription))
                .append(" clickable=").append(node.isClickable)
                .append(" editable=").append(node.isEditable)
                .append(" actions=")
                .append(node.actionList.joinToString(",") { action -> action.id.toString() })
            emitted += 1
            if (depth >= MAX_DEPTH) return
            for (index in 0 until node.childCount) {
                val child = node.getChild(index) ?: continue
                walk(child, if (path.isEmpty()) "$index" else "$path/$index", depth + 1)
            }
        }

        walk(root, "", 0)
        return builder.take(MAX_SNAPSHOT_CHARS).toString()
    }

    private fun parseGesturePath(encoded: String): Path {
        val points = encoded.split(';').map { point ->
            val pieces = point.split(',')
            require(pieces.size == 2) { "gesture point must be x,y" }
            Pair(
                requireNotNull(pieces[0].trim().toFloatOrNull()) { "invalid gesture x" },
                requireNotNull(pieces[1].trim().toFloatOrNull()) { "invalid gesture y" },
            )
        }
        require(points.isNotEmpty() && points.size <= 256) { "gesture point count is invalid" }
        return Path().apply {
            moveTo(points.first().first, points.first().second)
            points.drop(1).forEach { (x, y) -> lineTo(x, y) }
        }
    }

    private fun parseGlobalAction(value: String?): Int = when (value) {
        "back" -> AccessibilityService.GLOBAL_ACTION_BACK
        "home" -> AccessibilityService.GLOBAL_ACTION_HOME
        "recents" -> AccessibilityService.GLOBAL_ACTION_RECENTS
        "notifications" -> AccessibilityService.GLOBAL_ACTION_NOTIFICATIONS
        "quick_settings" -> AccessibilityService.GLOBAL_ACTION_QUICK_SETTINGS
        "power_dialog" -> AccessibilityService.GLOBAL_ACTION_POWER_DIALOG
        "lock_screen" -> AccessibilityService.GLOBAL_ACTION_LOCK_SCREEN
        "take_screenshot" -> AccessibilityService.GLOBAL_ACTION_TAKE_SCREENSHOT
        else -> requireNotNull(value?.toIntOrNull()) { "unknown global accessibility action" }
    }

    private fun safe(value: CharSequence?): String = value?.toString()?.replace('\n', ' ')?.take(1024) ?: "-"

    private fun quoted(value: CharSequence?): String = "\"${safe(value).replace("\\", "\\\\").replace("\"", "\\\"")}\""

    private companion object {
        val OPERATIONS = setOf(
            "accessibility.snapshot",
            "accessibility.global_action",
            "accessibility.node_action",
            "accessibility.gesture",
        )
        const val MAX_NODES = 512
        const val MAX_DEPTH = 48
        const val MAX_SNAPSHOT_CHARS = 256 * 1024
    }
}
