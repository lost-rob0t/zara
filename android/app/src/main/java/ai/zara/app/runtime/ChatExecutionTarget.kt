package ai.zara.app.runtime

enum class ChatExecutionTarget(val wireName: String) {
    LOCAL("local"),
    DIRECT_PROVIDER("direct_provider"),
    ZARA_SERVER("zara_server");

    companion object {
        fun parse(value: String): ChatExecutionTarget =
            entries.firstOrNull { it.wireName == value }
                ?: throw IllegalArgumentException("Unsupported chat execution target")
    }
}

data class ChatExecutionPrerequisites(
    val localReady: Boolean,
    val directProviderReady: Boolean,
    val zaraServerReady: Boolean,
)

enum class ChatExecutionUnavailableReason {
    LOCAL_UNAVAILABLE,
    DIRECT_PROVIDER_UNAVAILABLE,
    ZARA_SERVER_UNAVAILABLE,
}

data class ChatExecutionTargetStatus(
    val target: ChatExecutionTarget,
    val generation: Long,
    val ready: Boolean,
    val unavailableReason: ChatExecutionUnavailableReason?,
)

data class ChatExecutionSelection(
    val target: ChatExecutionTarget,
    val generation: Long,
) {
    init {
        require(generation >= 0) { "Chat execution generation must be non-negative" }
    }

    fun select(next: ChatExecutionTarget): ChatExecutionSelection {
        if (next == target) return this
        check(generation < Long.MAX_VALUE) { "Chat execution generation exhausted" }
        return ChatExecutionSelection(next, generation + 1)
    }

    fun accepts(resultGeneration: Long): Boolean = resultGeneration == generation

    fun status(prerequisites: ChatExecutionPrerequisites): ChatExecutionTargetStatus {
        val unavailableReason = when (target) {
            ChatExecutionTarget.LOCAL ->
                if (prerequisites.localReady) null else ChatExecutionUnavailableReason.LOCAL_UNAVAILABLE
            ChatExecutionTarget.DIRECT_PROVIDER ->
                if (prerequisites.directProviderReady) null
                else ChatExecutionUnavailableReason.DIRECT_PROVIDER_UNAVAILABLE
            ChatExecutionTarget.ZARA_SERVER ->
                if (prerequisites.zaraServerReady) null
                else ChatExecutionUnavailableReason.ZARA_SERVER_UNAVAILABLE
        }
        return ChatExecutionTargetStatus(
            target = target,
            generation = generation,
            ready = unavailableReason == null,
            unavailableReason = unavailableReason,
        )
    }

    companion object {
        fun initial(): ChatExecutionSelection =
            ChatExecutionSelection(ChatExecutionTarget.LOCAL, generation = 0)
    }
}
