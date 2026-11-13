package ai.zara.app.localai

/** Keep native close and engine lifecycle operations under the same ownership monitor. */
internal fun closeOwnedLiteRtConversation(
    owner: Any,
    isCurrent: () -> Boolean,
    close: () -> Unit,
    clear: () -> Unit,
) {
    synchronized(owner) {
        if (!isCurrent()) return
        try {
            close()
        } finally {
            clear()
        }
    }
}
