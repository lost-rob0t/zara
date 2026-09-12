package ai.zara.wear.surface

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class WearSurfaceContractTest {
    @Test
    fun manifestDeclaresTileAndComplicationProvidersWithSystemPermissions() {
        val manifest = File("src/main/AndroidManifest.xml").readText()

        assertTrue(manifest.contains("ZaraTileService"))
        assertTrue(manifest.contains("com.google.android.wearable.permission.BIND_TILE_PROVIDER"))
        assertTrue(manifest.contains("androidx.wear.tiles.action.BIND_TILE_PROVIDER"))

        assertTrue(manifest.contains("ZaraStatusComplicationService"))
        assertTrue(manifest.contains("ZaraVoiceComplicationService"))
        assertTrue(manifest.contains("com.google.android.wearable.permission.BIND_COMPLICATION_PROVIDER"))
        assertTrue(manifest.contains("android.support.wearable.complications.ACTION_COMPLICATION_UPDATE_REQUEST"))
    }

    @Test
    fun providersShareOneExplicitLaunchContract() {
        val contractFile = File("src/main/java/ai/zara/wear/surface/ZaraWearLaunchTargets.kt")
        assertTrue("launch target authority must exist", contractFile.isFile)
        val contract = contractFile.readText()
        assertTrue(contract.contains("ai.zara.wear"))
        assertTrue(contract.contains("ai.zara.wear.WearMainActivity"))
        assertTrue(contract.contains("ai.zara.wear.voice"))
        assertTrue(contract.contains("ai.zara.wear.voice.WearVoiceActivity"))
        assertTrue(contract.contains("ai.zara.action.WEAR_VOICE"))

        val tile = File("src/main/java/ai/zara/wear/surface/ZaraTileService.kt").readText()
        val status = File("src/main/java/ai/zara/wear/surface/ZaraStatusComplicationService.kt").readText()
        val voice = File("src/main/java/ai/zara/wear/surface/ZaraVoiceComplicationService.kt").readText()
        listOf(tile, status, voice).forEach { source ->
            assertTrue(source.contains("ZaraWearLaunchTargets"))
            assertFalse(source.contains("CURVE"))
            assertFalse(source.contains("PRIVATE KEY"))
        }
        assertTrue(tile.contains("voicePendingIntent") || tile.contains("voiceComponent"))
        assertTrue(status.contains("mainPendingIntent"))
        assertTrue(voice.contains("voicePendingIntent"))
    }

    @Test
    fun stableSurfaceDependenciesArePinned() {
        val catalog = File("../gradle/libs.versions.toml").readText()
        assertTrue(catalog.contains("wearTiles = \"1.6.2\""))
        assertTrue(catalog.contains("wearProtoLayout = \"1.4.2\""))
        assertTrue(catalog.contains("wearWatchface = \"1.3.0\""))
    }
}
