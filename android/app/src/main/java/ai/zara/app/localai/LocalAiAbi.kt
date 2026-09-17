package ai.zara.app.localai

object LocalAiAbi {
    const val ABI_VERSION = "local-ai-v1"
    const val OP_STATE = "state"
    const val OP_LOAD_MODEL = "load-model"
    const val OP_GENERATE = "generate"
    const val OP_CANCEL = "cancel"
    const val OP_SPEAK = "speak"
    const val OP_STOP_SPEECH = "stop-speech"

    val operations: Set<String> = setOf(
        OP_STATE,
        OP_LOAD_MODEL,
        OP_GENERATE,
        OP_CANCEL,
        OP_SPEAK,
        OP_STOP_SPEECH,
    )
}
