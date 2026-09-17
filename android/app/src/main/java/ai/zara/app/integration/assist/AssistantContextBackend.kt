package ai.zara.app.integration.assist

import ai.zara.app.integration.AndroidAuthorityLevel
import ai.zara.app.integration.AndroidBackend
import ai.zara.app.integration.AndroidOperationBackend
import ai.zara.app.integration.AndroidOperationError
import ai.zara.app.integration.AndroidOperationRequest
import ai.zara.app.integration.AndroidOperationResult
import android.app.assist.AssistContent
import android.app.assist.AssistStructure
import android.graphics.Bitmap
import android.os.Bundle
import android.os.Process
import android.util.Base64
import java.io.ByteArrayOutputStream
import java.util.concurrent.atomic.AtomicLong
import java.util.concurrent.atomic.AtomicReference

data class AssistantContextSnapshot(
    val generation: Long,
    val capturedAtMs: Long,
    val text: String,
    val screenshotJpeg: ByteArray?,
)

class AssistantContextBackend : AndroidOperationBackend {
    private val generation = AtomicLong(0L)
    private val snapshot = AtomicReference<AssistantContextSnapshot?>(null)

    override val backend = AndroidBackend.ASSIST
    override val minimumAuthority = AndroidAuthorityLevel.ELEVATED
    override val identity: String
        get() = "assistant:app:${Process.myUid()}"

    override fun isAvailable(): Boolean = snapshot.get() != null

    override fun supports(operation: String): Boolean = operation in OPERATIONS

    fun capture(
        data: Bundle?,
        structure: AssistStructure?,
        content: AssistContent?,
    ) {
        val nextGeneration = generation.incrementAndGet()
        val previousScreenshot = snapshot.get()?.screenshotJpeg
        snapshot.set(
            AssistantContextSnapshot(
                generation = nextGeneration,
                capturedAtMs = System.currentTimeMillis(),
                text = encodeContext(data, structure, content),
                screenshotJpeg = previousScreenshot,
            ),
        )
    }

    fun captureScreenshot(bitmap: Bitmap?) {
        val current = snapshot.get()
        val nextGeneration = current?.generation ?: generation.incrementAndGet()
        val bytes = bitmap?.let(::compressScreenshot)
        snapshot.set(
            AssistantContextSnapshot(
                generation = nextGeneration,
                capturedAtMs = current?.capturedAtMs ?: System.currentTimeMillis(),
                text = current?.text.orEmpty(),
                screenshotJpeg = bytes,
            ),
        )
    }

    fun clear() {
        snapshot.set(null)
    }

    override fun execute(request: AndroidOperationRequest): AndroidOperationResult {
        val value = snapshot.get()
            ?: return AndroidOperationResult.failed(
                AndroidOperationError.BACKEND_UNAVAILABLE,
                backend = backend.atom,
            )
        return when (request.operation) {
            "assist.snapshot" -> AndroidOperationResult.completed(
                backend.atom,
                identity,
                "generation=${value.generation}\ncaptured_at_ms=${value.capturedAtMs}\n${value.text}",
            )
            "assist.screenshot_base64" -> {
                val bytes = value.screenshotJpeg
                    ?: return AndroidOperationResult.failed(
                        AndroidOperationError.BACKEND_UNAVAILABLE,
                        backend.atom,
                        identity,
                        "assistant screenshot is unavailable",
                    )
                AndroidOperationResult.completed(
                    backend.atom,
                    identity,
                    Base64.encodeToString(bytes, Base64.NO_WRAP),
                )
            }
            "assist.clear" -> {
                clear()
                AndroidOperationResult.completed(backend.atom, identity)
            }
            else -> AndroidOperationResult.failed(
                AndroidOperationError.UNSUPPORTED_OPERATION,
                backend.atom,
                identity,
            )
        }
    }

    private fun encodeContext(
        data: Bundle?,
        structure: AssistStructure?,
        content: AssistContent?,
    ): String = buildString {
        append("activity=")
            .append(structure?.activityComponent?.flattenToShortString().orEmpty())
            .append('\n')
        append("web_uri=").append(content?.webUri?.toString().orEmpty()).append('\n')
        append("intent=").append(content?.intent?.toUri(0).orEmpty().take(MAX_FIELD_CHARS)).append('\n')
        append("structured_data=")
            .append(content?.structuredData.orEmpty().take(MAX_FIELD_CHARS))
            .append('\n')
        append("assist_data=")
            .append(
                data?.keySet().orEmpty().sorted().joinToString(",") { key ->
                    "$key=${data?.get(key)?.toString().orEmpty().take(MAX_VALUE_CHARS)}"
                },
            )
            .append('\n')
        if (structure != null) {
            var emitted = 0
            for (windowIndex in 0 until structure.windowNodeCount) {
                if (emitted >= MAX_NODES || length >= MAX_CONTEXT_CHARS) break
                val window = structure.getWindowNodeAt(windowIndex)
                append("window[").append(windowIndex).append("] title=")
                    .append(window.title?.toString().orEmpty().take(MAX_VALUE_CHARS))
                    .append('\n')

                fun walk(node: AssistStructure.ViewNode, path: String, depth: Int) {
                    if (emitted >= MAX_NODES || length >= MAX_CONTEXT_CHARS || depth > MAX_DEPTH) return
                    append("node[").append(path).append("]")
                        .append(" class=").append(node.className.orEmpty().take(MAX_VALUE_CHARS))
                        .append(" id=").append(node.idEntry.orEmpty().take(MAX_VALUE_CHARS))
                        .append(" text=").append(quoted(node.text))
                        .append(" hint=").append(quoted(node.hint))
                        .append(" content_desc=").append(quoted(node.contentDescription))
                        .append(" scroll=").append(node.scrollX).append(',').append(node.scrollY)
                        .append('\n')
                    emitted += 1
                    for (childIndex in 0 until node.childCount) {
                        walk(
                            node.getChildAt(childIndex),
                            if (path.isEmpty()) "$childIndex" else "$path/$childIndex",
                            depth + 1,
                        )
                    }
                }

                walk(window.rootViewNode, "$windowIndex", 0)
            }
        }
    }.take(MAX_CONTEXT_CHARS)

    private fun compressScreenshot(bitmap: Bitmap): ByteArray? {
        val output = ByteArrayOutputStream()
        if (!bitmap.compress(Bitmap.CompressFormat.JPEG, 78, output)) return null
        val bytes = output.toByteArray()
        return bytes.takeIf { it.size <= MAX_SCREENSHOT_BYTES }
    }

    private fun quoted(value: CharSequence?): String {
        val text = value?.toString()?.take(MAX_VALUE_CHARS).orEmpty()
        return "\"${text.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")}\""
    }

    private companion object {
        val OPERATIONS = setOf("assist.snapshot", "assist.screenshot_base64", "assist.clear")
        const val MAX_NODES = 2048
        const val MAX_DEPTH = 64
        const val MAX_CONTEXT_CHARS = 512 * 1024
        const val MAX_FIELD_CHARS = 64 * 1024
        const val MAX_VALUE_CHARS = 8192
        const val MAX_SCREENSHOT_BYTES = 2 * 1024 * 1024
    }
}
