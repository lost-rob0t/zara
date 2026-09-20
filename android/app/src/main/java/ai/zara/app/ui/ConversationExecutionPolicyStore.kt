package ai.zara.app.ui

import java.io.File

enum class ConversationExecutionPolicy {
    STANDARD,
    PURE_SYMBOLIC,
    ;

    val providersEnabled: Boolean
        get() = this != PURE_SYMBOLIC

    val maxModelCalls: Int?
        get() = if (this == PURE_SYMBOLIC) 0 else null

    val maxProviderCalls: Int?
        get() = if (this == PURE_SYMBOLIC) 0 else null
}

/**
 * Persists the conversation execution policy independently of Auto/Local/Remote routing.
 *
 * PURE_SYMBOLIC is a hard zero-provider policy, not another runtime mode. A corrupt persisted
 * record fails closed to PURE_SYMBOLIC so process recreation can never silently re-enable a
 * provider after a user selected zero-model execution.
 */
class ConversationExecutionPolicyStore(private val file: File) {
    fun load(): ConversationExecutionPolicy {
        if (!file.exists()) return ConversationExecutionPolicy.STANDARD
        return when (runCatching { file.readText().trim() }.getOrNull()) {
            STANDARD_RECORD -> ConversationExecutionPolicy.STANDARD
            PURE_SYMBOLIC_RECORD -> ConversationExecutionPolicy.PURE_SYMBOLIC
            else -> ConversationExecutionPolicy.PURE_SYMBOLIC
        }
    }

    fun save(policy: ConversationExecutionPolicy) {
        check(file.parentFile?.mkdirs() != false || file.parentFile?.isDirectory == true) {
            "Conversation execution policy directory is unavailable"
        }
        val temporary = File(file.parentFile, "${file.name}.tmp")
        val record = when (policy) {
            ConversationExecutionPolicy.STANDARD -> STANDARD_RECORD
            ConversationExecutionPolicy.PURE_SYMBOLIC -> PURE_SYMBOLIC_RECORD
        }
        try {
            temporary.writeText(record)
            check(temporary.renameTo(file) || run {
                file.delete()
                temporary.renameTo(file)
            }) { "Conversation execution policy could not be saved" }
        } finally {
            temporary.delete()
        }
    }

    private companion object {
        const val STANDARD_RECORD = "v1:standard"
        const val PURE_SYMBOLIC_RECORD = "v1:pure-symbolic"
    }
}
