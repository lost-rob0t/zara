package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class SemanticFixtureRunnerTest {

    @Test
    fun equivalentRuntimeResultsPassAfterRepresentationNormalization() {
        val fixture = SemanticFixture(id = "timer", query = "semantic(timer)")
        val linux = FakeRuntime(mapOf("timer" to SemanticResult("ZARA-SEMANTIC/1", listOf("frame( timer, 1200 )"))))
        val android = FakeRuntime(mapOf("timer" to SemanticResult("ZARA-SEMANTIC/1", listOf("frame(timer,1200)"))))

        val result = SemanticFixtureRunner(linux, android).run(listOf(fixture)).single()

        assertTrue(result.equivalent)
        assertEquals("timer", result.fixtureId)
        assertEquals(null, result.failure)
    }

    @Test
    fun semanticDriftFailsClosedWithBothNormalizedResults() {
        val fixture = SemanticFixture(id = "open", query = "semantic(open)")
        val linux = FakeRuntime(mapOf("open" to SemanticResult("ZARA-SEMANTIC/1", listOf("frame(open,firefox)"))))
        val android = FakeRuntime(mapOf("open" to SemanticResult("ZARA-SEMANTIC/1", listOf("frame(open,chrome)"))))

        val result = SemanticFixtureRunner(linux, android).run(listOf(fixture)).single()

        assertFalse(result.equivalent)
        assertEquals(SemanticResult("ZARA-SEMANTIC/1", listOf("frame(open,firefox)")), result.linux)
        assertEquals(SemanticResult("ZARA-SEMANTIC/1", listOf("frame(open,chrome)")), result.android)
    }

    @Test
    fun runtimeFailureIsEvidenceNotAFalseParityPass() {
        val fixture = SemanticFixture(id = "search", query = "semantic(search)")
        val linux = FakeRuntime(mapOf("search" to SemanticResult("ZARA-SEMANTIC/1", listOf("frame(search)"))))
        val android = object : SemanticRuntime {
            override fun evaluate(fixture: SemanticFixture): SemanticResult =
                error("trealla unavailable")
        }

        val result = SemanticFixtureRunner(linux, android).run(listOf(fixture)).single()

        assertFalse(result.equivalent)
        assertEquals("android runtime failed", result.failure)
        assertEquals(null, result.android)
    }

    private class FakeRuntime(
        private val results: Map<String, SemanticResult>
    ) : SemanticRuntime {
        override fun evaluate(fixture: SemanticFixture): SemanticResult =
            requireNotNull(results[fixture.id])
    }
}
