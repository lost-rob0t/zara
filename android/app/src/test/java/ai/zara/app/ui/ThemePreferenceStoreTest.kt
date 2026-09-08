package ai.zara.app.ui

import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Test

class ThemePreferenceStoreTest {
    @Test fun `absent preference resolves to the frozen default outrun`() {
        val root = Files.createTempDirectory("zara-theme-absent").toFile()
        assertEquals(ZaraTheme.Outrun, ThemePreferenceStore(File(root, "theme.bin")).load())
    }

    @Test fun `every frozen theme survives a new store instance and process restart`() {
        val root = Files.createTempDirectory("zara-theme-roundtrip").toFile()
        val file = File(root, "theme.bin")
        ZaraTheme.entries.forEach { theme ->
            ThemePreferenceStore(file).save(theme)
            assertEquals(theme, ThemePreferenceStore(file).load())
        }
    }

    @Test fun `corrupt or empty preference falls back to outrun instead of crashing startup`() {
        val root = Files.createTempDirectory("zara-theme-corrupt").toFile()
        val file = File(root, "theme.bin")
        file.writeText("not-a-theme")
        assertEquals(ZaraTheme.Outrun, ThemePreferenceStore(file).load())

        file.writeBytes(ByteArray(0))
        assertEquals(ZaraTheme.Outrun, ThemePreferenceStore(file).load())
    }

    @Test fun `saving replaces the previous selection`() {
        val root = Files.createTempDirectory("zara-theme-replace").toFile()
        val file = File(root, "theme.bin")
        val store = ThemePreferenceStore(file)
        store.save(ZaraTheme.Terminal)
        store.save(ZaraTheme.Light)
        assertEquals(ZaraTheme.Light, store.load())
    }
}
