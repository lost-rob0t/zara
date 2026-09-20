package ai.zara.code.workbench

import ai.zara.editor.core.OperationFence
import ai.zara.editor.core.RevisionedEditorBuffer
import androidx.compose.ui.text.TextRange
import androidx.compose.ui.text.input.TextFieldValue
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Test

class CodeWorkbenchCoreTest {
    @Test
    fun languageDetectionKeepsPrologAndPythonFirstClass() {
        assertEquals("prolog", CodeLanguage.fromFileName("rules.pl"))
        assertEquals("prolog", CodeLanguage.fromFileName("expert.pro"))
        assertEquals("python", CodeLanguage.fromFileName("worker.py"))
        assertEquals("lisp", CodeLanguage.fromFileName("init.el"))
        assertEquals("nim", CodeLanguage.fromFileName("bridge.nim"))
        assertEquals("text", CodeLanguage.fromFileName("README"))
    }

    @Test
    fun sharedBufferFencesSelectionChangesBeforeLateVoiceMutation() {
        val buffer = RevisionedEditorBuffer("alpha beta", languageId = "prolog")
        val fence = OperationFence()
        val token = fence.begin(buffer.snapshot().revision)

        syncEditorStateToBuffer(
            buffer,
            TextFieldValue("alpha beta", selection = TextRange(6, 10)),
        )

        assertEquals("alpha beta", buffer.snapshot().text)
        assertFalse(fence.accepts(token, buffer.snapshot().revision))
    }
}
