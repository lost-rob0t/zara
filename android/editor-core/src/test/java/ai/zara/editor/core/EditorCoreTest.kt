package ai.zara.editor.core

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class EditorCoreTest {
    @Test
    fun staleVoicePlanCannotOverwriteNewerUserEdit() {
        val buffer = RevisionedEditorBuffer("hello", languageId = "python")
        val captured = buffer.snapshot()
        val action = VoiceCodePlanner.plan(VoiceCodeIntent.Insert(" world"), captured) as VoiceAction.Edit

        buffer.replaceFromUser("hello!", newCursor = 6)
        val result = buffer.apply(action.plan)

        assertTrue(result is ApplyResult.Stale)
        assertEquals("hello!", buffer.snapshot().text)
        assertEquals(1L, buffer.snapshot().revision)
    }

    @Test
    fun wrapSelectionTouchesOnlyCapturedSelection() {
        val buffer = RevisionedEditorBuffer("alpha beta gamma", languageId = "python")
        val selected = buffer.replaceFromUser(
            "alpha beta gamma",
            newCursor = 10,
            newSelection = EditorSelection(6, 10),
        )

        val action = VoiceCodePlanner.plan(
            VoiceCodeIntent.WrapSelection("(", ")"),
            selected,
        ) as VoiceAction.Edit
        val result = buffer.apply(action.plan)

        assertTrue(result is ApplyResult.Applied)
        assertEquals("alpha (beta) gamma", buffer.snapshot().text)
    }

    @Test
    fun reversedComposeSelectionUsesNormalizedBoundsForVoiceEdits() {
        val buffer = RevisionedEditorBuffer("alpha beta gamma", languageId = "python")
        val selected = buffer.replaceFromUser(
            "alpha beta gamma",
            newCursor = 6,
            newSelection = EditorSelection(10, 6),
        )

        val action = VoiceCodePlanner.plan(
            VoiceCodeIntent.ReplaceSelection("BETA"),
            selected,
        ) as VoiceAction.Edit
        val result = buffer.apply(action.plan)

        assertTrue(result is ApplyResult.Applied)
        assertEquals(TextPatch(6, 10, "BETA"), action.plan.patches.single())
        assertEquals("alpha BETA gamma", buffer.snapshot().text)
    }

    @Test
    fun navigationDoesNotMutateBufferRevision() {
        val buffer = RevisionedEditorBuffer("one\ntwo\nthree")
        val before = buffer.snapshot()

        val action = VoiceCodePlanner.plan(VoiceCodeIntent.GoToLine(2), before)

        assertEquals(VoiceAction.Navigate(NavigationTarget.Line(2)), action)
        assertEquals(before, buffer.snapshot())
    }

    @Test
    fun cancelFencesLateSpeechOrModelResult() {
        val fence = OperationFence()
        val token = fence.begin(baseRevision = 7)
        assertTrue(fence.accepts(token, currentRevision = 7))

        fence.cancel()

        assertFalse(fence.accepts(token, currentRevision = 7))
    }

    @Test
    fun newerBufferRevisionAlsoFencesResult() {
        val fence = OperationFence()
        val token = fence.begin(baseRevision = 7)

        assertFalse(fence.accepts(token, currentRevision = 8))
    }

    @Test
    fun overlappingPatchesAreRejectedWithoutMutation() {
        val buffer = RevisionedEditorBuffer("abcdef")
        val plan = EditorEditPlan(
            baseRevision = 0,
            patches = listOf(
                TextPatch(1, 4, "x"),
                TextPatch(3, 5, "y"),
            ),
            summary = "bad overlap",
            origin = EditOrigin.TOOL,
        )

        val result = buffer.apply(plan)

        assertTrue(result is ApplyResult.Invalid)
        assertEquals("abcdef", buffer.snapshot().text)
        assertEquals(0L, buffer.snapshot().revision)
    }

    @Test
    fun destructiveSelectionReplacementRequiresConfirmation() {
        val snapshot = EditorSnapshot(
            bufferId = "b",
            text = "abcdef",
            revision = 4,
            cursor = 4,
            selection = EditorSelection(1, 4),
            languageId = "text",
        )

        val action = VoiceCodePlanner.plan(
            VoiceCodeIntent.ReplaceSelection("X"),
            snapshot,
        ) as VoiceAction.Edit

        assertTrue(action.plan.requiresConfirmation)
        assertEquals(4L, action.plan.baseRevision)
        assertEquals(TextPatch(1, 4, "X"), action.plan.patches.single())
    }
}
