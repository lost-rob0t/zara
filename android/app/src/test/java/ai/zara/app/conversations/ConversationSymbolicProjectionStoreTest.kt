package ai.zara.app.conversations

import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class ConversationSymbolicProjectionStoreTest {
    @Test fun `pure symbolic clarification and expert evidence survive restart at zero model calls`() {
        val root = Files.createTempDirectory("zara-symbolic-projection").toFile()
        val file = File(root, "symbolic-conversations.bin")
        val store = ConversationSymbolicProjectionStore(file)

        store.record(
            conversationId = "chat-a",
            turn = SymbolicTurnProjection(
                turnId = "turn-1",
                runtimeGeneration = 7,
                projectId = "dotfiles",
                projectGeneration = 3,
                dialogueAct = "inform",
                intentFrame = "dotfiles_fact(emacs_config)",
                discourseEntityRefs = listOf("entity:dotfiles"),
                expertEvidenceRefs = listOf("evidence:dotfiles-expert:1"),
                verifiedOutcomeRefs = listOf("fact:emacs-config:verified"),
                renderer = "symbolic-nlg/v1",
                status = SymbolicTurnStatus.Complete,
                modelCalls = 0,
                providerCalls = 0,
            ),
        )
        store.record(
            conversationId = "chat-a",
            turn = SymbolicTurnProjection(
                turnId = "turn-2",
                runtimeGeneration = 7,
                projectId = "dotfiles",
                projectGeneration = 3,
                dialogueAct = "clarify",
                intentFrame = "edit_target(Target)",
                discourseEntityRefs = listOf("entity:emacs-config"),
                unresolvedSlots = listOf("target"),
                expertEvidenceRefs = listOf("evidence:dotfiles-expert:1"),
                renderer = "symbolic-nlg/v1",
                status = SymbolicTurnStatus.Clarifying,
            ),
        )

        val restored = ConversationSymbolicProjectionStore(file).conversation("chat-a")!!
        assertEquals(2, restored.turns.size)
        assertEquals(SymbolicTurnStatus.Clarifying, restored.turns.last().status)
        assertEquals(listOf("target"), restored.turns.last().unresolvedSlots)
        assertEquals(listOf("evidence:dotfiles-expert:1"), restored.turns.last().expertEvidenceRefs)
        assertEquals(0, restored.turns.sumOf { it.modelCalls })
        assertEquals(0, restored.turns.sumOf { it.providerCalls })
    }

    @Test fun `pure symbolic projection rejects any model or provider usage`() {
        assertThrows(IllegalArgumentException::class.java) {
            SymbolicTurnProjection(
                turnId = "turn-model",
                runtimeGeneration = 1,
                dialogueAct = "inform",
                renderer = "symbolic-nlg/v1",
                status = SymbolicTurnStatus.Complete,
                modelCalls = 1,
            )
        }
        assertThrows(IllegalArgumentException::class.java) {
            SymbolicTurnProjection(
                turnId = "turn-provider",
                runtimeGeneration = 1,
                dialogueAct = "inform",
                renderer = "symbolic-nlg/v1",
                status = SymbolicTurnStatus.Complete,
                providerCalls = 1,
            )
        }
    }

    @Test fun `stale runtime project and terminal generations cannot publish late effects`() {
        val root = Files.createTempDirectory("zara-symbolic-fence").toFile()
        val store = ConversationSymbolicProjectionStore(File(root, "symbolic-conversations.bin"))

        store.record(
            "chat-a",
            SymbolicTurnProjection(
                turnId = "turn-1",
                runtimeGeneration = 9,
                projectId = "project-a",
                projectGeneration = 4,
                dialogueAct = "request",
                renderer = "symbolic-nlg/v1",
                status = SymbolicTurnStatus.Running,
            ),
        )
        store.record(
            "chat-a",
            SymbolicTurnProjection(
                turnId = "turn-1",
                runtimeGeneration = 9,
                projectId = "project-a",
                projectGeneration = 4,
                dialogueAct = "request",
                renderer = "symbolic-nlg/v1",
                status = SymbolicTurnStatus.Cancelled,
            ),
        )

        assertThrows(IllegalStateException::class.java) {
            store.record(
                "chat-a",
                SymbolicTurnProjection(
                    turnId = "turn-1",
                    runtimeGeneration = 9,
                    projectId = "project-a",
                    projectGeneration = 4,
                    dialogueAct = "inform",
                    renderer = "symbolic-nlg/v1",
                    status = SymbolicTurnStatus.Complete,
                    verifiedOutcomeRefs = listOf("effect:late"),
                ),
            )
        }
        store.record(
            "chat-a",
            SymbolicTurnProjection(
                turnId = "turn-2",
                runtimeGeneration = 10,
                projectId = "project-b",
                projectGeneration = 5,
                dialogueAct = "inform",
                renderer = "symbolic-nlg/v1",
                status = SymbolicTurnStatus.Complete,
            ),
        )
        assertThrows(IllegalStateException::class.java) {
            store.record(
                "chat-a",
                SymbolicTurnProjection(
                    turnId = "turn-3",
                    runtimeGeneration = 9,
                    projectId = "project-a",
                    projectGeneration = 4,
                    dialogueAct = "inform",
                    renderer = "symbolic-nlg/v1",
                    status = SymbolicTurnStatus.Complete,
                ),
            )
        }
        assertThrows(IllegalStateException::class.java) {
            store.record(
                "chat-a",
                SymbolicTurnProjection(
                    turnId = "turn-4",
                    runtimeGeneration = 10,
                    projectId = "project-a",
                    projectGeneration = 4,
                    dialogueAct = "inform",
                    renderer = "symbolic-nlg/v1",
                    status = SymbolicTurnStatus.Complete,
                ),
            )
        }
    }

    @Test fun `conversation projections stay isolated and running work becomes interrupted on restart`() {
        val root = Files.createTempDirectory("zara-symbolic-isolation").toFile()
        val file = File(root, "symbolic-conversations.bin")
        val store = ConversationSymbolicProjectionStore(file)

        store.record(
            "chat-a",
            SymbolicTurnProjection(
                turnId = "turn-a",
                runtimeGeneration = 2,
                dialogueAct = "request",
                unresolvedSlots = listOf("path"),
                renderer = "symbolic-nlg/v1",
                status = SymbolicTurnStatus.Running,
            ),
        )
        store.record(
            "chat-b",
            SymbolicTurnProjection(
                turnId = "turn-b",
                runtimeGeneration = 2,
                dialogueAct = "inform",
                discourseEntityRefs = listOf("entity:other-chat"),
                renderer = "symbolic-nlg/v1",
                status = SymbolicTurnStatus.Complete,
            ),
        )

        val beforeRestart = file.readBytes()
        val restored = ConversationSymbolicProjectionStore(file)
        val a = restored.conversation("chat-a")!!
        val b = restored.conversation("chat-b")!!
        assertEquals(SymbolicTurnStatus.Interrupted, a.turns.single().status)
        assertEquals(listOf("path"), a.turns.single().unresolvedSlots)
        assertTrue(a.turns.single().discourseEntityRefs.isEmpty())
        assertEquals(listOf("entity:other-chat"), b.turns.single().discourseEntityRefs)
        assertFalse(beforeRestart.contentEquals(file.readBytes()))
    }

    @Test fun `corrupt projection fails explicitly without destroying evidence`() {
        val root = Files.createTempDirectory("zara-symbolic-corrupt").toFile()
        val file = File(root, "symbolic-conversations.bin")
        val original = "not a symbolic projection"
        file.writeText(original)

        val store = ConversationSymbolicProjectionStore(file)
        assertNotNull(store.state().loadFailure)
        assertTrue(store.state().conversations.isEmpty())
        assertEquals(original, file.readText())
        assertThrows(IllegalStateException::class.java) {
            store.record(
                "chat-a",
                SymbolicTurnProjection(
                    turnId = "turn-a",
                    runtimeGeneration = 1,
                    dialogueAct = "inform",
                    renderer = "symbolic-nlg/v1",
                    status = SymbolicTurnStatus.Complete,
                ),
            )
        }
    }
}
