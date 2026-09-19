package ai.zara.app.ui

import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class StarIntelMapSurfaceTest {
    @Test
    fun embedUrlPreservesExistingQueryAndAddsZaraIdentity() {
        val url = starIntelMapEmbedUrl("https://maps.starintel.actor/view?theme=dark")
        assertTrue(url.startsWith("https://maps.starintel.actor/view?"))
        assertTrue(url.contains("theme=dark"))
        assertTrue(url.contains("embed=1"))
        assertTrue(url.contains("client=zara-android"))
    }

    @Test
    fun onlySameHttpsOriginCanNavigateInsideMapSurface() {
        val base = "https://maps.starintel.actor/"
        assertTrue(isAllowedStarIntelMapNavigation(base, "https://maps.starintel.actor/entity/123"))
        assertFalse(isAllowedStarIntelMapNavigation(base, "https://example.com/"))
        assertFalse(isAllowedStarIntelMapNavigation(base, "http://maps.starintel.actor/"))
        assertFalse(isAllowedStarIntelMapNavigation(base, "https://maps.starintel.actor.evil.example/"))
    }

    @Test
    fun insecureAndCredentialBearingMapUrlsAreRejected() {
        assertTrue(runCatching { starIntelMapEmbedUrl("http://maps.starintel.actor/") }.isFailure)
        assertTrue(
            runCatching { starIntelMapEmbedUrl("https://user:pass@maps.starintel.actor/") }.isFailure,
        )
    }
}
