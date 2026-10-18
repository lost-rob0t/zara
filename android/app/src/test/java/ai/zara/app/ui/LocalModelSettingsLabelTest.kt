package ai.zara.app.ui

import ai.zara.app.localai.LocalAiPhase
import org.junit.Assert.assertEquals
import org.junit.Test

class LocalModelSettingsLabelTest {
    @Test
    fun `selected model action reflects runtime phase`() {
        assertEquals("Loading…", localModelActionLabel(LocalAiPhase.LOADING, true))
        assertEquals("Active", localModelActionLabel(LocalAiPhase.READY, true))
        assertEquals("Responding…", localModelActionLabel(LocalAiPhase.GENERATING, true))
        assertEquals("Failed", localModelActionLabel(LocalAiPhase.FAILED, true))
        assertEquals("Use", localModelActionLabel(LocalAiPhase.STOPPED, false))
    }

    @Test
    fun `other installed models remain available for selection`() {
        LocalAiPhase.entries.forEach { phase ->
            assertEquals("Use", localModelActionLabel(phase, false))
        }
    }
}
