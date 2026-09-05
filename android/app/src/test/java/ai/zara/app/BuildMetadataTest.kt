package ai.zara.app

import org.junit.Assert.assertTrue
import org.junit.Test

class BuildMetadataTest {
    @Test fun `diagnostic source sha is an immutable git commit`() {
        assertTrue(
            "SOURCE_SHA must be a 40-character lowercase git SHA, got ${BuildConfig.SOURCE_SHA}",
            BuildConfig.SOURCE_SHA.matches(Regex("[0-9a-f]{40}")),
        )
    }
}
