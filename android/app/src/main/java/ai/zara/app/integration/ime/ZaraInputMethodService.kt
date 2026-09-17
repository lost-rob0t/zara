package ai.zara.app.integration.ime

import ai.zara.app.ZaraApplication
import ai.zara.app.integration.AndroidAuthorityLevel
import ai.zara.app.integration.AndroidBackend
import ai.zara.app.integration.AndroidOperationBackend
import ai.zara.app.integration.AndroidOperationError
import ai.zara.app.integration.AndroidOperationRequest
import ai.zara.app.integration.AndroidOperationResult
import android.content.Intent
import android.os.Process
import android.view.Gravity
import android.view.KeyEvent
import android.view.View
import android.view.inputmethod.InputMethodManager
import android.widget.Button
import android.widget.LinearLayout
import android.widget.TextView
import android.inputmethodservice.InputMethodService
import java.util.concurrent.atomic.AtomicReference

class ZaraInputMethodService : InputMethodService() {
    override fun onCreate() {
        super.onCreate()
        (application as ZaraApplication).androidIntegration.attachIme(this)
    }

    override fun onDestroy() {
        (application as ZaraApplication).androidIntegration.detachIme(this)
        super.onDestroy()
    }

    override fun onCreateInputView(): View = LinearLayout(this).apply {
        orientation = LinearLayout.HORIZONTAL
        gravity = Gravity.CENTER_VERTICAL
        setPadding(12, 8, 12, 8)
        addView(TextView(context).apply {
            text = "Zara"
            textSize = 16f
            setPadding(12, 0, 24, 0)
        })
        addView(Button(context).apply {
            text = "Ask"
            contentDescription = "Open Zara assistant"
            setOnClickListener {
                runCatching {
                    startActivity(
                        Intent(Intent.ACTION_ASSIST).addFlags(Intent.FLAG_ACTIVITY_NEW_TASK),
                    )
                }
            }
        })
        addView(Button(context).apply {
            text = "Next keyboard"
            contentDescription = "Switch input method"
            setOnClickListener {
                if (!switchToNextInputMethod(false)) {
                    val manager = getSystemService(InputMethodManager::class.java)
                    manager.showInputMethodPicker()
                }
            }
        })
    }
}

class ImeBackend : AndroidOperationBackend {
    private val service = AtomicReference<ZaraInputMethodService?>(null)

    override val backend = AndroidBackend.IME
    override val minimumAuthority = AndroidAuthorityLevel.UNRESTRICTED
    override val identity: String
        get() = "ime:app:${Process.myUid()}"

    fun attach(value: ZaraInputMethodService) {
        service.set(value)
    }

    fun detach(value: ZaraInputMethodService) {
        service.compareAndSet(value, null)
    }

    override fun isAvailable(): Boolean = service.get()?.currentInputConnection != null

    override fun supports(operation: String): Boolean = operation in OPERATIONS

    override fun execute(request: AndroidOperationRequest): AndroidOperationResult {
        val active = service.get()
            ?: return unavailable()
        val input = active.currentInputConnection ?: return unavailable()
        return when (request.operation) {
            "ime.context" -> {
                val before = input.getTextBeforeCursor(MAX_CONTEXT_CHARS, 0)?.toString().orEmpty()
                val selected = input.getSelectedText(0)?.toString().orEmpty()
                val after = input.getTextAfterCursor(MAX_CONTEXT_CHARS, 0)?.toString().orEmpty()
                AndroidOperationResult.completed(
                    backend.atom,
                    identity,
                    "before=${quote(before)}\nselected=${quote(selected)}\nafter=${quote(after)}",
                )
            }
            "ime.commit" -> booleanResult(
                input.commitText(requireNotNull(request.arguments["text"]) { "text is required" }, 1),
            )
            "ime.set_composing" -> booleanResult(
                input.setComposingText(requireNotNull(request.arguments["text"]) { "text is required" }, 1),
            )
            "ime.finish_composing" -> booleanResult(input.finishComposingText())
            "ime.delete_surrounding" -> booleanResult(
                input.deleteSurroundingText(
                    request.arguments["before"]?.toIntOrNull()?.coerceIn(0, MAX_CONTEXT_CHARS) ?: 0,
                    request.arguments["after"]?.toIntOrNull()?.coerceIn(0, MAX_CONTEXT_CHARS) ?: 0,
                ),
            )
            "ime.set_selection" -> booleanResult(
                input.setSelection(
                    requireNotNull(request.arguments["start"]?.toIntOrNull()) { "selection start is required" },
                    requireNotNull(request.arguments["end"]?.toIntOrNull()) { "selection end is required" },
                ),
            )
            "ime.editor_action" -> booleanResult(
                input.performEditorAction(
                    requireNotNull(request.arguments["action"]?.toIntOrNull()) { "editor action is required" },
                ),
            )
            "ime.key" -> {
                val keyCode = requireNotNull(request.arguments["key_code"]?.toIntOrNull()) {
                    "key_code is required"
                }
                val action = when (request.arguments["action"] ?: "down_up") {
                    "down" -> KeyEvent.ACTION_DOWN
                    "up" -> KeyEvent.ACTION_UP
                    "down_up" -> -1
                    else -> throw IllegalArgumentException("invalid key action")
                }
                if (action == -1) {
                    val down = input.sendKeyEvent(KeyEvent(KeyEvent.ACTION_DOWN, keyCode))
                    val up = input.sendKeyEvent(KeyEvent(KeyEvent.ACTION_UP, keyCode))
                    booleanResult(down && up)
                } else {
                    booleanResult(input.sendKeyEvent(KeyEvent(action, keyCode)))
                }
            }
            else -> AndroidOperationResult.failed(
                AndroidOperationError.UNSUPPORTED_OPERATION,
                backend.atom,
                identity,
            )
        }
    }

    private fun unavailable(): AndroidOperationResult =
        AndroidOperationResult.failed(AndroidOperationError.BACKEND_UNAVAILABLE, backend.atom)

    private fun booleanResult(value: Boolean): AndroidOperationResult =
        if (value) {
            AndroidOperationResult.completed(backend.atom, identity)
        } else {
            AndroidOperationResult.failed(
                AndroidOperationError.FAILED,
                backend.atom,
                identity,
                "input connection rejected operation",
            )
        }

    private fun quote(value: String): String =
        "\"${value.take(MAX_CONTEXT_CHARS).replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n")}\""

    private companion object {
        val OPERATIONS = setOf(
            "ime.context",
            "ime.commit",
            "ime.set_composing",
            "ime.finish_composing",
            "ime.delete_surrounding",
            "ime.set_selection",
            "ime.editor_action",
            "ime.key",
        )
        const val MAX_CONTEXT_CHARS = 32 * 1024
    }
}
