package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class SemanticResultComparatorTest {

    @Test fun `equivalent ignores solution order and insignificant prolog whitespace`() {
        val left = SemanticResult(
            contractVersion = PortableSemanticCore.contractVersion,
            terms = listOf(
                "frame(intent(ns(app), name(open)), [], complete)",
                "frame(intent(ns(web),name(search)), [], complete)"
            )
        )
        val right = SemanticResult(
            contractVersion = PortableSemanticCore.contractVersion,
            terms = listOf(
                " frame( intent( ns(web), name(search) ), [ ], complete ) ",
                "frame(intent(ns(app),name(open)),[],complete)"
            )
        )

        assertTrue(SemanticResultComparator.equivalent(left, right))
    }

    @Test fun `normalization preserves whitespace inside quoted prolog values`() {
        val normalized = SemanticResultComparator.normalize(
            SemanticResult(
                contractVersion = PortableSemanticCore.contractVersion,
                terms = listOf("value(text('hello  world'))")
            )
        )

        assertEquals(listOf("value(text('hello  world'))"), normalized.terms)
    }

    @Test fun `different semantic contracts never compare equivalent`() {
        val current = SemanticResult(PortableSemanticCore.contractVersion, emptyList())
        val future = SemanticResult("ZARA-SEMANTIC/2", emptyList())

        assertFalse(SemanticResultComparator.equivalent(current, future))
    }

    @Test fun `duplicate solutions remain significant`() {
        val term = "frame(intent(ns(device),name('screen.capture')),[],complete)"
        val once = SemanticResult(PortableSemanticCore.contractVersion, listOf(term))
        val twice = SemanticResult(PortableSemanticCore.contractVersion, listOf(term, term))

        assertFalse(SemanticResultComparator.equivalent(once, twice))
    }

    @Test fun `escaped quotes do not break quoted whitespace preservation`() {
        val term = "value(text('can\\'t  stop'))"
        val normalized = SemanticResultComparator.normalize(
            SemanticResult(PortableSemanticCore.contractVersion, listOf(term))
        )

        assertEquals(listOf(term), normalized.terms)
    }
}
