package ai.zara.app.update

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class ChangelogTest {
    @Test
    fun extractsOnlyRequestedVersion() {
        val markdown = """
            # Zara Changelog

            ## Unreleased

            - Future work.

            ## 0.2.2-alpha

            ### Added

            - Projects.
            - Three menus.

            ### Fixed

            - Audio focus.

            ## 0.1.2-alpha

            - Old baseline.
        """.trimIndent()

        assertEquals(
            "Added\n• Projects.\n• Three menus.\n\nFixed\n• Audio focus.",
            Changelog.notesForVersion(markdown, "0.2.2-alpha"),
        )
        assertNull(Changelog.notesForVersion(markdown, "9.9.9"))
    }

    @Test
    fun releaseNotesShowOncePerVersion() {
        val notes = "• New thing"
        assertTrue(Changelog.shouldShow(null, "0.2.2-alpha", notes))
        assertFalse(Changelog.shouldShow("0.2.2-alpha", "0.2.2-alpha", notes))
        assertTrue(Changelog.shouldShow("0.2.1-alpha", "0.2.2-alpha", notes))
        assertFalse(Changelog.shouldShow(null, "0.2.2-alpha", null))
    }
}
