package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class TreallaSemanticRuntimeTest {

    @Test(expected = IllegalStateException::class)
    fun evaluationBeforeInitializationFailsClosed() {
        val runtime = TreallaSemanticRuntime(FakeTreallaBridge())

        runtime.evaluate(SemanticFixture("timer", "semantic(timer)"))
    }

    @Test
    fun initializationLoadsExplicitPortableCoreAndEvaluationUsesFrozenContract() {
        val bridge = FakeTreallaBridge(
            results = mapOf("semantic(timer)" to listOf("frame(timer,1200)"))
        )
        val runtime = TreallaSemanticRuntime(bridge)

        runtime.initialize("prolog/portable/semantic_core.pl")
        val result = runtime.evaluate(SemanticFixture("timer", "semantic(timer)"))

        assertEquals(listOf("prolog/portable/semantic_core.pl"), bridge.initializedPaths)
        assertEquals(SemanticResult("ZARA-SEMANTIC/1", listOf("frame(timer,1200)")), result)
    }

    @Test
    fun failedInitializationDoesNotMakeRuntimeReady() {
        val bridge = FakeTreallaBridge(failInitialization = true)
        val runtime = TreallaSemanticRuntime(bridge)

        try {
            runtime.initialize("prolog/portable/semantic_core.pl")
        } catch (_: IllegalStateException) {
        }

        assertFalse(runtime.isReady)
    }

    @Test
    fun closeIsIdempotentAndPreventsFurtherEvaluation() {
        val bridge = FakeTreallaBridge()
        val runtime = TreallaSemanticRuntime(bridge)
        runtime.initialize("prolog/portable/semantic_core.pl")

        runtime.close()
        runtime.close()

        assertEquals(1, bridge.shutdownCount)
        assertTrue(runtime.isClosed)
        try {
            runtime.evaluate(SemanticFixture("timer", "semantic(timer)"))
            throw AssertionError("evaluation after close must fail")
        } catch (_: IllegalStateException) {
        }
    }

    private class FakeTreallaBridge(
        private val results: Map<String, List<String>> = emptyMap(),
        private val failInitialization: Boolean = false
    ) : TreallaBridge {
        val initializedPaths = mutableListOf<String>()
        var shutdownCount = 0

        override fun initialize(coreAssetPath: String) {
            initializedPaths += coreAssetPath
            if (failInitialization) error("native init failed")
        }

        override fun evaluate(query: String): List<String> = results[query].orEmpty()

        override fun shutdown() {
            shutdownCount += 1
        }
    }
}
