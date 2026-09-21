package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

/**
 * P0 crash-window contract for pure-symbolic Android turns.
 *
 * A deterministic turn is not durably complete until the canonical assistant message and the
 * symbolic Context1 projection become terminal together. If those writes are split, process death
 * can recover history as interrupted while leaving a successful Context1 behind (or vice versa),
 * which breaks restart continuity and stale-generation fencing.
 */
class AndroidSymbolicAtomicTerminalCommitContractTest {
    @Test
    fun `portable store owns one sqlite transaction for assistant plus projection terminal commit`() {
        val atomicCommit = File(
            "src/main/java/ai/zara/app/history/SymbolicTurnTerminalCommit.kt"
        ).readText()

        assertTrue(
            "pure-symbolic terminal commit must be owned by PortableConversationStore rather than " +
                "a UI cache or second history owner",
            atomicCommit.contains("fun PortableConversationStore.completeSymbolicTurnAtomically("),
        )
        assertTrue(
            "assistant history and Context1 projection must share one SQLite transaction",
            atomicCommit.contains("beginTransaction()") &&
                atomicCommit.contains("desktop_messages") &&
                atomicCommit.contains("desktop_symbolic_projections") &&
                atomicCommit.contains("setTransactionSuccessful()"),
        )
        assertTrue(
            "the atomic commit must fence both the canonical running turn and projection generation",
            atomicCommit.contains("turn_id") &&
                atomicCommit.contains("projection_generation") &&
                atomicCommit.contains("expectedGeneration"),
        )
        assertTrue(
            "terminal history must be selected from pending/streaming state instead of overwriting " +
                "an already-terminal assistant row",
            atomicCommit.contains(HistoryStatusTokens.pending) &&
                atomicCommit.contains(HistoryStatusTokens.streaming),
        )
    }

    @Test
    fun `factory publishes rendered Context1 through atomic terminal commit before future completion`() {
        val factory = File(
            "src/main/java/ai/zara/app/prolog/AndroidPureSymbolicConversationFactory.kt"
        ).readText()
        val successPath = factory
            .substringAfter("val envelope = splitDialogueEnvelope(result)", "")
            .substringBefore("} catch (error: Throwable)", "")

        assertTrue("symbolic success path disappeared", successPath.isNotEmpty())
        assertTrue(
            "the terminal projection must consume Context1, canonical dialogue act, and expert " +
                "evidence from the same single dialogue envelope before persistence",
            successPath.contains("contextTerm = envelope.contextTerm") &&
                successPath.contains("dialogueAct = envelope.dialogueAct") &&
                successPath.contains("expertEvidenceRef = envelope.expertEvidenceRef"),
        )
        assertTrue(
            "the same persistence owner that fenced the pending generation must atomically commit " +
                "assistant output plus Context1 before the result future can complete",
            successPath.contains("completeSymbolicTurnAtomically(") &&
                successPath.indexOf("completeSymbolicTurnAtomically(") < successPath.indexOf("output.complete("),
        )
        assertTrue(
            "the canonical rendered response must be the assistant content committed with Context1",
            successPath.contains("assistantContent = envelope.renderedResponse"),
        )
        assertFalse(
            "projection-only success commit leaves a process-death split-brain window",
            successPath.contains("saveSymbolicProjection("),
        )
    }

    @Test
    fun `ui completion facade accepts already terminal canonical success without rewriting it`() {
        val store = File(
            "src/main/java/ai/zara/app/conversations/CanonicalConversationStore.kt"
        ).readText()
        val completion = store
            .substringAfter("fun completeTurn(", "")
            .substringBefore("fun failTurn(", "")

        assertTrue("canonical completion facade disappeared", completion.isNotEmpty())
        assertTrue(
            "UI callback must tolerate a success/error row that the atomic runtime/store boundary " +
                "already committed instead of trying to overwrite an immutable terminal message",
            completion.contains("if (pending != null)") &&
                completion.contains("terminal.status") &&
                completion.contains("Conversation has no matching running or terminal turn"),
        )
    }

    private object HistoryStatusTokens {
        const val pending = "HistoryMessageStatus.Pending"
        const val streaming = "HistoryMessageStatus.Streaming"
    }
}
