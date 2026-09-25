package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidSymbolicRestartCommitFenceContractTest {
    @Test
    fun `persisted natural turn installs restart-visible pending fence before local evaluation`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()
        val persistedTurn = factory
            .substringAfter("private fun resolvePersistedTurn(")
            .substringBefore("private fun splitDialogueEnvelope(")

        val queryIndex = persistedTurn.indexOf("session.queryLocalProlog(")
        check(queryIndex >= 0) { "persisted symbolic turn no longer uses the canonical local Prolog owner" }

        val beforeQuery = persistedTurn.substring(0, queryIndex)
        assertTrue(
            "a process recreation can only fence late Context1 if a canonical pending projection " +
                "exists before local evaluation starts",
            beforeQuery.contains("pendingProjection") || beforeQuery.contains("beginPendingProjection"),
        )
        assertTrue(
            "the pending projection must be committed through the canonical CAS store before query",
            beforeQuery.contains("saveSymbolicProjection("),
        )
    }

    @Test
    fun `pending projection reuses canonical history turn identity`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()
        val persistedTurn = factory
            .substringAfter("private fun resolvePersistedTurn(")
            .substringBefore("private fun failBeforeAsyncEvaluation(")

        assertTrue(
            "the projection must derive turn identity from the already-persisted canonical history turn",
            persistedTurn.contains("requireRunningTurnId(projectionStore, conversationId)"),
        )
        assertTrue(
            "canonical history turn identity must be threaded into the pending projection",
            persistedTurn.contains("turnId = turnId"),
        )
        assertFalse(
            "symbolic projection must not mint a competing Android-only turn identity",
            factory.contains("UUID.randomUUID"),
        )
    }

    @Test
    fun `terminal Context1 write must CAS against the pending generation not the pre-turn generation`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()
        val persistedTurn = factory
            .substringAfter("private fun resolvePersistedTurn(")
            .substringBefore("private fun splitDialogueEnvelope(")

        val queryIndex = persistedTurn.indexOf("session.queryLocalProlog(")
        val afterQuery = persistedTurn.substring(queryIndex)
        assertTrue(
            "late symbolic completion must use the post-pending CAS generation so " +
                "PortableConversationStore.loadState() can interrupt and invalidate it",
            afterQuery.contains("pendingGeneration") ||
                afterQuery.contains("expectedGeneration = pendingProjection.projectionGeneration"),
        )
    }
}
