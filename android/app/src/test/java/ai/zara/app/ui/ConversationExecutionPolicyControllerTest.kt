package ai.zara.app.ui

import ai.zara.app.prolog.PureSymbolicRoute
import ai.zara.app.prolog.PureSymbolicTurnResult
import ai.zara.app.runtime.TextTurnResult
import java.io.File
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicInteger
import kotlin.io.path.createTempDirectory
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class ConversationExecutionPolicyControllerTest {
    @Test
    fun pureSymbolicSelectionCannotReachStandardProviderPath() {
        val directory = createTempDirectory("zara-execution-controller").toFile()
        val store = ConversationExecutionPolicyStore(File(directory, "conversation-policy.bin"))
        store.save(ConversationExecutionPolicy.PURE_SYMBOLIC)
        val providerCalls = AtomicInteger(0)
        val symbolicCalls = AtomicInteger(0)
        val controller = ConversationExecutionPolicyController(
            store = store,
            pureSymbolicSubmit = { _, conversationId ->
                symbolicCalls.incrementAndGet()
                CompletableFuture.completedFuture(symbolicTurn(conversationId, "symbolic answer"))
            },
            standardSubmit = { _, conversationId ->
                providerCalls.incrementAndGet()
                CompletableFuture.completedFuture(turn(conversationId, "provider answer"))
            },
        )

        val result = controller.submit("hello", "conversation-1").join()

        assertEquals("symbolic answer", result.text)
        assertEquals(1, symbolicCalls.get())
        assertEquals(0, providerCalls.get())
    }

    @Test
    fun pureSymbolicSelectionNeverEvaluatesPerTurnStandardSupplier() {
        val directory = createTempDirectory("zara-execution-controller").toFile()
        val store = ConversationExecutionPolicyStore(File(directory, "conversation-policy.bin"))
        store.save(ConversationExecutionPolicy.PURE_SYMBOLIC)
        val providerCalls = AtomicInteger(0)
        val controller = ConversationExecutionPolicyController(
            store = store,
            pureSymbolicSubmit = { _, conversationId ->
                CompletableFuture.completedFuture(symbolicTurn(conversationId, "symbolic answer"))
            },
        )

        val result = controller.submit("hello", "conversation-1") {
            providerCalls.incrementAndGet()
            CompletableFuture.completedFuture(turn("conversation-1", "provider answer"))
        }.join()

        assertEquals("symbolic answer", result.text)
        assertEquals(0, providerCalls.get())
    }

    @Test
    fun standardSelectionUsesCanonicalPerTurnSupplier() {
        val directory = createTempDirectory("zara-execution-controller").toFile()
        val store = ConversationExecutionPolicyStore(File(directory, "conversation-policy.bin"))
        val symbolicCalls = AtomicInteger(0)
        val standardCalls = AtomicInteger(0)
        val controller = ConversationExecutionPolicyController(
            store = store,
            pureSymbolicSubmit = { _, conversationId ->
                symbolicCalls.incrementAndGet()
                CompletableFuture.completedFuture(symbolicTurn(conversationId, "symbolic answer"))
            },
        )

        val result = controller.submit("hello", "conversation-1") {
            standardCalls.incrementAndGet()
            CompletableFuture.completedFuture(turn("conversation-1", "standard answer"))
        }.join()

        assertEquals("standard answer", result.text)
        assertEquals(1, standardCalls.get())
        assertEquals(0, symbolicCalls.get())
    }

    @Test
    fun processRecreationRestoresPureSymbolicBeforeFirstSubmit() {
        val directory = createTempDirectory("zara-execution-controller").toFile()
        val file = File(directory, "conversation-policy.bin")
        val first = ConversationExecutionPolicyController(
            store = ConversationExecutionPolicyStore(file),
            pureSymbolicSubmit = { _, conversationId ->
                CompletableFuture.completedFuture(symbolicTurn(conversationId, "first"))
            },
            standardSubmit = { _, conversationId ->
                CompletableFuture.completedFuture(turn(conversationId, "standard"))
            },
        )
        first.select(ConversationExecutionPolicy.PURE_SYMBOLIC)

        val providerCalls = AtomicInteger(0)
        val recreated = ConversationExecutionPolicyController(
            store = ConversationExecutionPolicyStore(file),
            pureSymbolicSubmit = { _, conversationId ->
                CompletableFuture.completedFuture(symbolicTurn(conversationId, "restored"))
            },
            standardSubmit = { _, conversationId ->
                providerCalls.incrementAndGet()
                CompletableFuture.completedFuture(turn(conversationId, "provider"))
            },
        )

        val result = recreated.submit("follow up", "conversation-1").join()

        assertEquals(ConversationExecutionPolicy.PURE_SYMBOLIC, recreated.policy())
        assertEquals("restored", result.text)
        assertEquals(0, providerCalls.get())
    }

    @Test
    fun switchingBackToStandardIsExplicitAndPersisted() {
        val directory = createTempDirectory("zara-execution-controller").toFile()
        val file = File(directory, "conversation-policy.bin")
        val symbolicCalls = AtomicInteger(0)
        val controller = ConversationExecutionPolicyController(
            store = ConversationExecutionPolicyStore(file),
            pureSymbolicSubmit = { _, conversationId ->
                symbolicCalls.incrementAndGet()
                CompletableFuture.completedFuture(symbolicTurn(conversationId, "symbolic"))
            },
            standardSubmit = { _, conversationId ->
                CompletableFuture.completedFuture(turn(conversationId, "standard"))
            },
        )
        controller.select(ConversationExecutionPolicy.PURE_SYMBOLIC)
        controller.select(ConversationExecutionPolicy.STANDARD)

        val result = controller.submit("hello", "conversation-1").join()

        assertEquals("standard", result.text)
        assertEquals(0, symbolicCalls.get())
        assertEquals(
            ConversationExecutionPolicy.STANDARD,
            ConversationExecutionPolicyStore(file).load(),
        )
    }

    @Test
    fun failedCanonicalControlCommitCannotPersistProviderEnablingPolicy() {
        val directory = createTempDirectory("zara-execution-controller").toFile()
        val file = File(directory, "conversation-policy.bin")
        val store = ConversationExecutionPolicyStore(file)
        store.save(ConversationExecutionPolicy.PURE_SYMBOLIC)
        val providerCalls = AtomicInteger(0)
        val controller = ConversationExecutionPolicyController(
            store = store,
            pureSymbolicSubmit = { _, conversationId ->
                CompletableFuture.completedFuture(symbolicTurn(conversationId, "symbolic answer"))
            },
            standardSubmit = { _, conversationId ->
                providerCalls.incrementAndGet()
                CompletableFuture.completedFuture(turn(conversationId, "provider answer"))
            },
        )

        val failure = runCatching {
            controller.selectAfterCanonicalCommit(ConversationExecutionPolicy.STANDARD) {
                val duringTransition = controller.submit("probe", "conversation-1").join()
                assertEquals("symbolic answer", duringTransition.text)
                assertEquals(0, providerCalls.get())
                error("terminal persistence failed")
            }
        }.exceptionOrNull()

        assertTrue(failure is IllegalStateException)
        assertEquals(ConversationExecutionPolicy.PURE_SYMBOLIC, controller.policy())
        assertEquals(
            ConversationExecutionPolicy.PURE_SYMBOLIC,
            ConversationExecutionPolicyStore(file).load(),
        )
        assertEquals(0, providerCalls.get())
    }

    @Test
    fun pureSymbolicEvidenceIsHardZeroByConstruction() {
        val evidence = symbolicTurn("conversation-1", "answer")

        assertFalse(evidence.turn.text.isBlank())
        assertTrue(evidence.turn.success)
        assertEquals(0, evidence.maxModelCalls)
        assertEquals(0, evidence.maxProviderCalls)
        assertEquals(0, evidence.modelCalls)
        assertEquals(0, evidence.providerCalls)
    }

    private fun symbolicTurn(conversationId: String, text: String) = PureSymbolicTurnResult(
        turn = turn(conversationId, text),
        route = PureSymbolicRoute.FRAME_RESOLVER,
    )

    private fun turn(conversationId: String, text: String) = TextTurnResult(
        conversationId = conversationId,
        turnId = "turn-1",
        text = text,
        success = true,
    )
}
