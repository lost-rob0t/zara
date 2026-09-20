package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession

/**
 * Binds the zero-model conversation controller to Android's existing Prolog runtime owner.
 *
 * This does not own a runtime, conversation store, expert registry, or provider client. Explicit
 * queries stay on the bounded local-query path. Natural-language turns execute the canonical
 * symbolic_dialogue_turn -> symbolic_dialogue renderer chain through AndroidAppSession's already
 * started LocalZaraServer via queryLocalProlog(). The controller threads the canonical
 * conversation id into this resolver boundary so the durable projection can be composed here
 * without adding controller-local dialogue state.
 */
internal object AndroidPureSymbolicConversationFactory {
    fun create(session: AndroidAppSession): PureSymbolicConversationController =
        PureSymbolicConversationController(
            catalog = { PrologWorkspaceCatalog.from(session.prologSources()) },
            query = session::queryLocalProlog,
            resolve = { utterance, _conversationId ->
                session.queryLocalProlog(dialogueTurnQuery(utterance))
            },
        )

    internal fun dialogueTurnQuery(utterance: String): String {
        val text = utterance.trim()
        require(text.isNotEmpty()) { "Utterance is required" }
        require(text.length <= MAX_UTTERANCE_CHARS) { "Utterance is too large" }
        val escaped = text
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\n", "\\n")
        return "symbolic_dialogue_turn:dialogue_turn(\"$escaped\", conversation, [], " +
            "turn(_Frames, Act, _Context)), symbolic_dialogue:render_response(Act, Result)"
    }

    private const val MAX_UTTERANCE_CHARS = 8_192
}
