package ai.zara.org.surfaces

import ai.zara.org.core.OrgPageParser
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgEditorSessionTest {
    @Test fun `opening a file starts from an undirty identical draft`() {
        val session = OrgEditorSessions.open("todo.org", "* TODO thing")

        assertEquals("todo.org", session.path)
        assertFalse(session.dirty)
        assertEquals("* TODO thing", session.draft)
    }

    @Test fun `editing marks the session dirty without touching the original`() {
        val session = OrgEditorSessions.open("todo.org", "* TODO thing").withDraft("* DONE thing")

        assertTrue(session.dirty)
        assertEquals("* TODO thing", session.originalText)
    }

    @Test fun `saves are blocked when the disk text moved under the session`() {
        val session = OrgEditorSessions.open("todo.org", "* TODO thing").withDraft("* DONE thing")

        val reason = OrgEditorSessions.staleReason(session, "* TODO changed by emacs")

        assertNotNull(reason)
        assertTrue(reason!!.contains("todo.org"))
        assertTrue(reason.contains("refused"))
    }

    @Test fun `saves proceed when the disk still matches the session original`() {
        val session = OrgEditorSessions.open("todo.org", "* TODO thing")

        assertNull(OrgEditorSessions.staleReason(session, "* TODO thing"))
    }

    @Test fun `settling a save re-bases the session on the saved text`() {
        val settled = OrgEditorSessions.settle(
            OrgEditorSessions.open("todo.org", "* TODO thing").withDraft("* DONE thing"),
            "* DONE thing",
        )

        assertFalse(settled.dirty)
        assertEquals("* DONE thing", settled.originalText)
    }

    @Test fun `editing one rendered block preserves every other source byte`() {
        val source = "#+title: Page\n* One\nBody one\n* Two\nBody two"
        val block = OrgPageParser.parse(source).blocks.first()
        val session = OrgEditorSessions.open("page.org", source)

        val updated = OrgEditorSessions.replaceBlock(session, block, "* One edited\nBody one\n")

        assertEquals("#+title: Page\n* One edited\nBody one\n* Two\nBody two", updated.draft)
        assertEquals(source, updated.originalText)
    }
}
