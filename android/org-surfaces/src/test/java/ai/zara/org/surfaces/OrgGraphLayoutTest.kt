package ai.zara.org.surfaces

import ai.zara.org.core.OrgRoam
import ai.zara.org.core.OrgRoamGraph
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgGraphLayoutTest {
    private val graph: OrgRoamGraph = OrgRoam.build(
        mapOf(
            "a.org" to """
                * Alpha
                :PROPERTIES:
                :ID: alpha
                :END:
                * Beta
                :PROPERTIES:
                :ID: beta
                :END:
                See [[id:alpha][Alpha]].
                * Gamma
                :PROPERTIES:
                :ID: gamma
                :END:
            """.trimIndent(),
        ),
    )

    @Test fun `arranges every node on a deterministic ring`() {
        val first = OrgGraphLayout.arrange(graph, width = 800f, height = 600f)
        val second = OrgGraphLayout.arrange(graph, width = 800f, height = 600f)

        assertEquals(graph.nodes.keys, first.map { it.id }.toSet())
        assertEquals(first, second)
        assertTrue(first.all { it.x in 0f..800f && it.y in 0f..600f })
    }

    @Test fun `empty graphs arrange to nothing`() {
        val empty = OrgRoamGraph(nodes = emptyMap(), duplicateIds = emptySet())
        assertTrue(OrgGraphLayout.arrange(empty, 800f, 600f).isEmpty())
        assertTrue(OrgGraphLayout.edges(empty).isEmpty())
    }

    @Test fun `edges are deduplicated sorted canonical id links only`() {
        val edges = OrgGraphLayout.edges(graph)

        assertEquals(listOf("beta" to "alpha"), edges)
    }

    @Test fun `dangling id links never produce edges`() {
        val dangling = OrgRoam.build(
            mapOf(
                "dangling.org" to """
                    * Lonely
                    :PROPERTIES:
                    :ID: lonely
                    :END:
                    Missing [[id:ghost][ghost]].
                """.trimIndent(),
            ),
        )

        assertTrue(OrgGraphLayout.edges(dangling).isEmpty())
    }
}
