package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Test

class LocalNaturalLanguageExpertRouterNaturalnessTest {
    private fun catalog() = PrologWorkspaceCatalog(
        facts = emptyList(),
        rules = emptyList(),
        schemas = emptyList(),
        experts = listOf(PredicateRef("triage_explain", 2)),
        activations = mapOf("inspect" to "triage"),
    )

    @Test
    fun typedSelectionCarriesBoundedMultiWordEntityWithoutRawQueryState() {
        val selected = LocalNaturalLanguageExpertRouter.select(
            "  INSPECT   Alice   Smith  ",
            catalog(),
        )
        assertNotNull(selected)
        val selection = requireNotNull(selected)

        assertEquals("triage", selection.expertId)
        assertEquals("explain", selection.expertOperation)
        assertEquals(mapOf("entity" to "alice smith"), selection.input)
        assertEquals(
            "triage_explain('alice smith', Result)",
            LocalNaturalLanguageExpertRouter.query("inspect alice smith", catalog()),
        )
    }

    @Test
    fun terminalConversationPunctuationIsIgnoredWithoutWideningEntityGrammar() {
        val selected = LocalNaturalLanguageExpertRouter.select(
            "Inspect Alice Smith?!",
            catalog(),
        )
        assertNotNull(selected)
        val selection = requireNotNull(selected)

        assertEquals("triage", selection.expertId)
        assertEquals("explain", selection.expertOperation)
        assertEquals(mapOf("entity" to "alice smith"), selection.input)
        assertEquals(
            "triage_explain('alice smith', Result)",
            LocalNaturalLanguageExpertRouter.query("inspect alice smith?", catalog()),
        )

        assertNull(LocalNaturalLanguageExpertRouter.select("inspect alice.smith", catalog()))
        assertNull(LocalNaturalLanguageExpertRouter.select("inspect alice; shell", catalog()))
        assertNull(LocalNaturalLanguageExpertRouter.select("inspect alice!!!!", catalog()))
    }

    @Test
    fun oversizedEntityFailsClosedBeforeExpertSelection() {
        val oversized = "a".repeat(129)

        assertNull(LocalNaturalLanguageExpertRouter.select("inspect $oversized", catalog()))
        assertNull(LocalNaturalLanguageExpertRouter.query("inspect $oversized", catalog()))
    }
}
