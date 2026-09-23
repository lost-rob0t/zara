package ai.zara.code.editor

import ai.zara.editor.core.EditorSelection
import ai.zara.editor.core.OperationFence
import ai.zara.editor.core.RevisionedEditorBuffer
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.input.TextFieldValue
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test

class EditorStateSyncTest {
    @Test
    fun selectionOnlyComposeUpdateFencesLateVoiceResult() {
        val buffer = RevisionedEditorBuffer("alpha beta", languageId = "text")
        syncEditorStateToBuffer(
            buffer,
            TextFieldValue("alpha beta", selection = TextRange(0, 5)),
        )
        val fence = OperationFence()
        val token = fence.begin(buffer.snapshot().revision)

        syncEditorStateToBuffer(
            buffer,
            TextFieldValue("alpha beta", selection = TextRange(6, 10)),
        )

        val snapshot = buffer.snapshot()
        assertEquals("alpha beta", snapshot.text)
        assertEquals(EditorSelection(6, 10), snapshot.selection)
        assertFalse(fence.accepts(token, snapshot.revision))
    }
}
