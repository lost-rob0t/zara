package ai.zara.org.storage

import java.time.LocalDate
import org.junit.Assert.assertEquals
import org.junit.Test

class OrgDailyConfigurationTest {
    @Test
    fun `arbitrary daily layout resolves to workspace relative spec`() {
        val spec = OrgDailyConfiguration(
            relativePathTemplate = "knowledge/journal/{date}.org",
            datePattern = "yyyy_MM_dd",
            zoneId = "America/New_York",
        ).toSpec()

        assertEquals(
            "knowledge/journal/2026_09_19.org",
            spec.pathFor(LocalDate.of(2026, 9, 19)),
        )
    }

    @Test(expected = IllegalArgumentException::class)
    fun `daily layout cannot escape selected workspace`() {
        OrgDailyConfiguration(
            relativePathTemplate = "../journal/{date}.org",
            datePattern = "yyyy-MM-dd",
            zoneId = "UTC",
        ).toSpec()
    }

    @Test(expected = java.time.DateTimeException::class)
    fun `invalid timezone fails closed`() {
        OrgDailyConfiguration(
            relativePathTemplate = "daily/{date}.org",
            datePattern = "yyyy-MM-dd",
            zoneId = "not-a-zone",
        ).toSpec()
    }
}
