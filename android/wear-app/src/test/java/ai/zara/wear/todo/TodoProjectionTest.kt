package ai.zara.wear.todo

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class TodoProjectionTest {
    @Test
    fun `next task text is bounded for watch complications`() {
        val value = TodoProjection(
            nextTitle = "Finish the extraordinarily long Zara Android todo synchronization implementation",
            openCount = 7,
            topic = "zara/android",
            syncState = TodoSyncState.SYNCED,
            updatedAtEpochMillis = 1_000L,
        )

        val rendered = TodoProjectionFormatter.next(value)

        assertTrue(rendered.text.length <= TodoProjectionFormatter.MAX_TEXT_LENGTH)
        assertEquals("NEXT", rendered.title)
    }

    @Test
    fun `negative counts and blank topics are normalized`() {
        val value = TodoProjection(
            nextTitle = null,
            openCount = -4,
            topic = "   ",
            syncState = TodoSyncState.LOCAL_ONLY,
            updatedAtEpochMillis = 0L,
        )

        assertEquals("0 open", TodoProjectionFormatter.summary(value).text)
        assertEquals("LOCAL", TodoProjectionFormatter.summary(value).title)
        assertFalse(TodoProjectionFormatter.summary(value).text.contains("-"))
    }

    @Test
    fun `stale projections are explicit`() {
        val value = TodoProjection(
            nextTitle = "Review PR",
            openCount = 3,
            topic = "zara",
            syncState = TodoSyncState.SYNCED,
            updatedAtEpochMillis = 1_000L,
        )

        val rendered = TodoProjectionFormatter.summary(
            projection = value,
            nowEpochMillis = 1_000L + TodoProjectionFormatter.STALE_AFTER_MILLIS + 1L,
        )

        assertEquals("STALE", rendered.title)
    }

    @Test
    fun `projection model has no notes or credential field`() {
        val names = TodoProjection::class.java.declaredFields.map { it.name }.toSet()

        assertFalse("notes" in names)
        assertFalse("token" in names)
        assertFalse("credential" in names)
    }
}
