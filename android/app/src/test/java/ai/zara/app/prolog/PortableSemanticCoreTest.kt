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
                "prolog/portable/semantic_fixtures.json",
                "prolog/shared/modules/intent_frames.pl",
                "prolog/shared/modules/normalizer.pl",
                "prolog/shared/kb/intents.pl"
            ),
            PortableSemanticCore.resources
        )
    }

    @Test fun `portable semantic resources cannot escape approved asset roots`() {
        PortableSemanticCore.resources.forEach { resource ->
            assertFalse(resource.startsWith("/"))
            assertFalse(resource.contains(".."))
            assertTrue(
                resource.startsWith("prolog/portable/") ||
                    resource.startsWith("prolog/shared/")
            )
        }
    }
}
