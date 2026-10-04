package ai.zara.app.runtime

import ai.zara.app.ui.ConversationExecutionPolicy

data class RuntimeStartupPolicy(
    val mode: RuntimeMode,
    val execution: ConversationExecutionPolicy,
) {
    val restoreRemote: Boolean
        get() = mode != RuntimeMode.Local && execution.providersEnabled

    val loadLocalModel: Boolean
        get() = mode != RuntimeMode.Remote && execution.providersEnabled
}
