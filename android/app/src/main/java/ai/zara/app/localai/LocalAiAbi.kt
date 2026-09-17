package ai.zara.app.localai

object LocalAiAbi {
    const val ABI_VERSION = "local-ai-v1"
    const val PROVIDER_EMBEDDED = EmbeddedLocalAiProvider.ID
    const val OP_STATE = "state"
    const val OP_LIST_MODELS = "list-models"
    const val OP_ACTIVE_MODEL = "active-model"
    const val OP_INSTALL_MODEL = "install-model"
    const val OP_SELECT_MODEL = "select-model"
    const val OP_LOAD_MODEL = "load-model"
    const val OP_GENERATE = "generate"
    const val OP_CANCEL = "cancel"
    const val OP_UNLOAD_MODEL = "unload-model"
    const val OP_SPEAK = "speak"
    const val OP_STOP_SPEECH = "stop-speech"

    val operations: Set<String> = setOf(
        OP_STATE,
        OP_LIST_MODELS,
        OP_ACTIVE_MODEL,
        OP_INSTALL_MODEL,
        OP_SELECT_MODEL,
        OP_LOAD_MODEL,
        OP_GENERATE,
        OP_CANCEL,
        OP_UNLOAD_MODEL,
        OP_SPEAK,
        OP_STOP_SPEECH,
    )
}
