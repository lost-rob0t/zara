package ai.zara.org.surfaces

import ai.zara.org.core.OrgPageBlock
import ai.zara.org.core.OrgPageParser

data class OrgEditorSession(
    val path: String,
    val originalText: String,
    val draft: String,
) {
    val dirty: Boolean get() = draft != originalText

    fun withDraft(draft: String): OrgEditorSession = copy(draft = draft)
}

object OrgEditorSessions {
    fun open(path: String, text: String): OrgEditorSession = OrgEditorSession(path, text, text)

    fun staleReason(session: OrgEditorSession, currentDiskText: String): String? =
        if (currentDiskText == session.originalText) {
            null
        } else {
            "${session.path} changed outside this editor; save refused so the newer canonical text is not overwritten"
        }

    fun settle(session: OrgEditorSession, savedText: String): OrgEditorSession =
        OrgEditorSession(session.path, savedText, savedText)

    fun replaceBlock(
        session: OrgEditorSession,
        block: OrgPageBlock,
        replacement: String,
    ): OrgEditorSession = session.withDraft(
        OrgPageParser.replaceBlock(session.draft, block, replacement),
    )
}
