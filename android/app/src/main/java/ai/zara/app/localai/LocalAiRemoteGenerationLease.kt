package ai.zara.app.localai

internal class LocalAiRemoteGenerationLease(
    private val cancel: () -> Unit,
    private val unlink: () -> Unit,
) {
    private var active = true

    @Synchronized
    fun <T> runIfActive(block: () -> T): T? {
        if (!active) return null
        return block()
    }

    @Synchronized
    fun callerDied(): Boolean {
        if (!active) return false
        active = false
        runCatching(unlink)
        runCatching(cancel)
        return true
    }

    @Synchronized
    fun finish(): Boolean {
        if (!active) return false
        active = false
        runCatching(unlink)
        return true
    }
}
