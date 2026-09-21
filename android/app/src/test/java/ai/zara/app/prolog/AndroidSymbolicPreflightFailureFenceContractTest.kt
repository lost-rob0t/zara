package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * P0 preflight-failure contract for pure-symbolic Android turns.
 *
 * MainActivity creates the canonical user + pending-assistant rows before the symbolic resolver is
 * entered. A context/project/projection preflight failure must therefore terminalize that exact
 * assistant turn in zara.db; returning a synthetic controller failure while leaving the canonical
 * row pending wedges the conversation and splits outward turn identity from durable history.
 */
class AndroidSymbolicPreflightFailureFenceContractTest {
    @Test
    fun `factory captures canonical turn before fallible symbolic preflight and fails it durably`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()
        val resolver = factory
            .substringAfter("private fun resolvePersistedTurn(", "")
            .substringBefore("private fun requireRunningTurnId(", "")
        val preflightFailure = factory
            .substringAfter("private fun failBeforePendingProjection(", "")
            .substringBefore("private fun failBeforeAsyncEvaluation(", "")

        assertTrue("persisted symbolic resolver disappeared", resolver.isNotEmpty())
        assertTrue(
            "canonical running turn identity must be captured before context/project preflight can fail",
            resolver.indexOf("requireRunningTurnId(") in 0 until resolver.indexOf("SymbolicDialogueContextCodec.decode("),
        )
        assertTrue(
            "preflight failure must leave the resolver through the canonical failure helper",
            resolver.contains("failBeforePendingProjection("),
        )
        assertTrue(
            "the preflight helper must use the canonical PortableConversationStore failure fence instead of " +
                "escaping to the controller where a synthetic turn id can be minted",
            preflightFailure.contains("failSymbolicTurnBeforeProjection("),
        )
    }

    @Test
    fun `portable store preflight failure fence only terminalizes the matching running assistant`() {
        val commit = File(
            "src/main/java/ai/zara/app/history/SymbolicTurnTerminalCommit.kt"
        ).readText()
        val preflight = commit
            .substringAfter("fun PortableConversationStore.failSymbolicTurnBeforeProjection(", "")

        assertTrue("canonical preflight failure fence disappeared", preflight.isNotEmpty())
        assertTrue(
            "preflight failure must stay on the canonical zara.db history owner",
            preflight.contains("desktop_messages") && preflight.contains("desktop_conversations"),
        )
        assertTrue(
            "late/stale preflight failure must not overwrite an already-terminal assistant",
            preflight.contains("HistoryMessageStatus.Pending.wireName") &&
                preflight.contains("HistoryMessageStatus.Streaming.wireName") &&
                preflight.contains("messageChanged == 1"),
        )
        assertTrue(
            "preflight failure must refuse to bypass an already-installed pending projection CAS",
            preflight.contains("symbolic preflight failure cannot bypass pending projection CAS"),
        )
    }
}
