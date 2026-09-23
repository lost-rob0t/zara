package ai.zara.app.prolog

import ai.zara.app.AndroidAppSession

/**
 * Binds the zero-model conversation controller to Android's existing Prolog runtime owner.
 *
 * This does not own a runtime, conversation store, expert registry, or provider client. Both
 * explicit queries and natural-language frame resolution execute through AndroidAppSession's
 * already-started LocalZaraServer via queryLocalProlog().
 */
internal object AndroidPureSymbolicConversationFactory {
    fun create(session: AndroidAppSession): PureSymbolicConversationController =
        PureSymbolicConversationController(
            catalog = { PrologWorkspaceCatalog.from(session.prologSources()) },
            query = session::queryLocalProlog,
            resolve = { utterance -> session.queryLocalProlog(frameResolverQuery(utterance)) },
        )

    internal fun frameResolverQuery(utterance: String): String {
        val text = utterance.trim()
        require(text.isNotEmpty()) { "Utterance is required" }
        require(text.length <= MAX_UTTERANCE_CHARS) { "Utterance is too large" }
        val escaped = text
            .replace("\\", "\\\\")
            .replace("\"", "\\\"")
            .replace("\n", "\\n")
        return "resolve_frames(\"$escaped\", passive, [], Frames), member(Result, Frames)"
    }

    private const val MAX_UTTERANCE_CHARS = 8_192
}
