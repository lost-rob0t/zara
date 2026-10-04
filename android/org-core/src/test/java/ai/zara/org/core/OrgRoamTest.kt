package ai.zara.org.core

import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgRoamTest {
    @Test
    fun indexesNodesAliasesLinksAndBacklinks() {
        val graph = OrgRoam.build(
            mapOf(
                "alpha.org" to """
                    * Project Alpha :project:
                    :PROPERTIES:
                    :ID: alpha
                    :ROAM_ALIASES: "Alpha Project" alpha-work
                    :END:
                    See [[id:beta][Beta node]].
                """.trimIndent(),
                "beta.org" to """
                    * Beta
                    :PROPERTIES:
                    :ID: beta
                    :END:
                """.trimIndent(),
            ),
        )

        assertEquals(setOf("alpha", "beta"), graph.nodes.keys)
        assertEquals(setOf("Alpha Project", "alpha-work"), graph.nodes.getValue("alpha").aliases)
        assertEquals("beta", graph.nodes.getValue("alpha").links.single().targetId)
        assertEquals("alpha", graph.backlinks("beta").single().sourceId)
        assertEquals("alpha", graph.search("alpha project").single().id)
    }

    @Test
    fun duplicateIdsAreReportedAndExcludedFromAuthority() {
        val graph = OrgRoam.build(
            mapOf(
                "a.org" to "* A\n:PROPERTIES:\n:ID: dup\n:END:",
                "b.org" to "* B\n:PROPERTIES:\n:ID: dup\n:END:",
            ),
        )

        assertEquals(setOf("dup"), graph.duplicateIds)
        assertTrue("dup" !in graph.nodes)
    }
}
