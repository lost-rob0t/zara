package ai.zara.app.email

import ai.zara.app.localai.LocalPromptContexts
import ai.zara.app.prolog.PrologWorkspace
import android.content.Context

/** Installs the canonical generated email_rules.pl asset into the editable Android workspace. */
object EmailPrologPlugin {
    const val SOURCE_NAME = "email_rules.pl"
    const val ASSET_PATH = "prolog/shared/kb/email_rules.pl"
    const val MODEL_CONTEXT_NAME = "email-prolog-api"

    fun install(context: Context, workspace: PrologWorkspace) {
        if (workspace.listSources().none { it.name == SOURCE_NAME }) {
            val source = context.assets.open(ASSET_PATH).bufferedReader(Charsets.UTF_8).use { it.readText() }
            require(source.contains("email_tool(email_search")) { "email Prolog asset is incomplete" }
            require(source.contains("email_before_send")) { "email Prolog policy API is missing" }
            workspace.saveSource(SOURCE_NAME, source)
        }
        LocalPromptContexts.register(MODEL_CONTEXT_NAME, AndroidEmailPlugin.MODEL_CONTEXT)
    }
}
