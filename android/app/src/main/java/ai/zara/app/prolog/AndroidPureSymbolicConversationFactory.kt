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
                session.queryLocalProlog(
                    dialogueTurnQuery(
                        utterance,
                        SymbolicDialogueContextCodec.emptyContextTerm,
                    ),
                )
            },
        )

    /**
     * Render one canonical dialogue turn from an explicitly supplied continuation term.
     *
     * The term is data, not executable query text: it is quoted as a Prolog string, decoded by
     * term_string/3, and must pass the shared valid_dialogue_context/1 shape fence before the
     * canonical router sees it. Context1 is also validated before a response can escape.
     */
    internal fun dialogueTurnQuery(
        utterance: String,
        contextTerm: String = SymbolicDialogueContextCodec.emptyContextTerm,
    ): String = dialogueTurnPrelude(utterance, contextTerm) +
        ", symbolic_dialogue:render_response(Act, Result)"

    /** Return the canonical Context1 term through Android's existing Result binding ABI. */
    internal fun dialogueContextQuery(
        utterance: String,
        contextTerm: String,
    ): String = dialogueTurnPrelude(utterance, contextTerm) + ", Result = Context1"

    private fun dialogueTurnPrelude(utterance: String, contextTerm: String): String {
        val text = utterance.trim()
        require(text.isNotEmpty()) { "Utterance is required" }
        require(text.length <= MAX_UTTERANCE_CHARS) { "Utterance is too large" }
        val escapedText = prologString(text)
        val canonicalContext = SymbolicDialogueContextCodec.requireContextTerm(contextTerm)
        val escapedContext = SymbolicDialogueContextCodec.prologString(canonicalContext)
        return "term_string(Context0, \"$escapedContext\", [quoted(true)]), " +
            "symbolic_dialogue_turn:valid_dialogue_context(Context0), " +
            "symbolic_dialogue_turn:dialogue_turn(\"$escapedText\", conversation, Context0, " +
            "turn(_Frames, Act, Context1)), " +
            "symbolic_dialogue_turn:valid_dialogue_context(Context1)"
    }

    private fun prologString(raw: String): String = buildString(raw.length + 8) {
        raw.forEach { character ->
            when (character) {
                '\\' -> append("\\\\")
                '"' -> append("\\\"")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> append(character)
            }
        }
    }

    private const val MAX_UTTERANCE_CHARS = 8_192
}
