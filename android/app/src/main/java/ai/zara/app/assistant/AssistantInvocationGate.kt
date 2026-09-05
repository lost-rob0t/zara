package ai.zara.app.assistant

internal sealed interface AssistantCaptureFinish {
    data object None : AssistantCaptureFinish
    data object Commit : AssistantCaptureFinish
    data object Cancel : AssistantCaptureFinish
}

internal class AssistantInvocationGate {
    private val lock = Any()
    private var shown = false
    private var starting = false
    private var active = false
    private var pendingFinish: AssistantCaptureFinish = AssistantCaptureFinish.None

    fun show(): Boolean = synchronized(lock) {
        if (shown) return@synchronized false
        shown = true
        true
    }

    fun beginPress(): Boolean = synchronized(lock) {
        if (!shown || starting || active) return@synchronized false
        starting = true
        pendingFinish = AssistantCaptureFinish.None
        true
    }

    fun releasePress(): AssistantCaptureFinish = synchronized(lock) {
        when {
            starting -> {
                pendingFinish = AssistantCaptureFinish.Commit
                AssistantCaptureFinish.None
            }
            active -> {
                active = false
                AssistantCaptureFinish.Commit
            }
            else -> AssistantCaptureFinish.None
        }
    }

    fun cancelPress(): AssistantCaptureFinish = synchronized(lock) {
        when {
            starting -> {
                pendingFinish = AssistantCaptureFinish.Cancel
                AssistantCaptureFinish.None
            }
            active -> {
                active = false
                AssistantCaptureFinish.Cancel
            }
            else -> AssistantCaptureFinish.None
        }
    }

    fun startSucceeded(): AssistantCaptureFinish = synchronized(lock) {
        if (!starting) return@synchronized AssistantCaptureFinish.Cancel
        starting = false
        if (!shown) {
            pendingFinish = AssistantCaptureFinish.None
            return@synchronized AssistantCaptureFinish.Cancel
        }
        val finish = pendingFinish
        pendingFinish = AssistantCaptureFinish.None
        if (finish is AssistantCaptureFinish.None) active = true
        finish
    }

    fun startFailed() = synchronized(lock) {
        starting = false
        active = false
        pendingFinish = AssistantCaptureFinish.None
    }

    fun hide(): AssistantCaptureFinish = synchronized(lock) {
        if (!shown && !starting && !active) return@synchronized AssistantCaptureFinish.None
        shown = false
        when {
            starting -> {
                pendingFinish = AssistantCaptureFinish.Cancel
                AssistantCaptureFinish.None
            }
            active -> {
                active = false
                AssistantCaptureFinish.Cancel
            }
            else -> AssistantCaptureFinish.None
        }
    }
}
