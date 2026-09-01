package ai.zara.app.prolog

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class PortableSemanticCoreTest {

    @Test fun `portable semantic contract version is explicit`() {
        assertEquals("ZARA-SEMANTIC/1", PortableSemanticCore.contractVersion)
    }

    @Test fun `portable semantic resources are closed and deterministic`() {
        assertEquals(
            listOf(
                "prolog/portable/semantic_core.pl",
                "prolog/portable/semantic_fixtures.json"
            ),
            PortableSemanticCore.resources
        )
    }

    @Test fun `portable semantic resources cannot escape app assets`() {
        PortableSemanticCore.resources.forEach { resource ->
            assertFalse(resource.startsWith("/"))
            assertFalse(resource.contains(".."))
            assertTrue(resource.startsWith("prolog/portable/"))
        }
    }
}
