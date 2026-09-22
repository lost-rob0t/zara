package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Test

class LocalNaturalLanguageExpertRouterTest {
    @Test
    fun selectionPreservesCanonicalExpertIdentityAndCompatibilityQuery() {
        val catalog = PrologWorkspaceCatalog(
            facts = emptyList(),
            rules = emptyList(),
            schemas = emptyList(),
            experts = listOf(PredicateRef("triage_explain", 2)),
            activations = mapOf("inspect" to "triage"),
        )

        val selected = LocalNaturalLanguageExpertRouter.select("  INSPECT Alice  ", catalog)
        assertNotNull(selected)
        val selection = requireNotNull(selected)

        assertEquals("triage", selection.expertId)
        assertEquals("triage_explain(alice, Result)", selection.query)
        assertEquals(
            selection.query,
            LocalNaturalLanguageExpertRouter.query("inspect alice", catalog),
        )
    }

    @Test
    fun selectionFailsClosedWhenActivationOrExpertBodyIsMissing() {
        val noActivation = PrologWorkspaceCatalog(
            facts = emptyList(),
            rules = emptyList(),
            schemas = emptyList(),
            experts = listOf(PredicateRef("triage_explain", 2)),
            activations = emptyMap(),
        )
        val missingBody = noActivation.copy(activations = mapOf("inspect" to "triage"))

        assertNull(LocalNaturalLanguageExpertRouter.select("inspect alice", noActivation))
        assertNull(LocalNaturalLanguageExpertRouter.select("inspect alice", missingBody.copy(experts = emptyList())))
        assertNull(LocalNaturalLanguageExpertRouter.select("inspect", missingBody))
    }
}
