package ai.zara.app.prolog

import java.util.Locale
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Test

class LocalNaturalLanguageExpertRouterLocaleTest {
    @Test
    fun selectionIsLocaleInvariantForCanonicalAsciiExpertCommands() {
        val previousLocale = Locale.getDefault()
        val catalog = PrologWorkspaceCatalog(
            facts = emptyList(),
            rules = emptyList(),
            schemas = emptyList(),
            experts = listOf(PredicateRef("triage_explain", 2)),
            activations = mapOf("inspect" to "triage"),
        )

        try {
            Locale.setDefault(Locale.forLanguageTag("tr-TR"))

            val selected = LocalNaturalLanguageExpertRouter.select("INSPECT ALICE", catalog)
            assertNotNull(selected)
            val selection = requireNotNull(selected)
            assertEquals("triage", selection.expertId)
            assertEquals("triage_explain(alice, Result)", selection.query)
        } finally {
            Locale.setDefault(previousLocale)
        }
    }
}
