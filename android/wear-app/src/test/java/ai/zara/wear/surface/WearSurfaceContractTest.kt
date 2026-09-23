package ai.zara.wear.surface

import androidx.wear.watchface.complications.data.ComplicationType
import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
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
        (1..6).forEach { lane ->
            assertTrue(manifest.contains("OrgSchedule${lane}ComplicationService"))
        }
        assertTrue(manifest.contains("OrgNextTodoComplicationService"))
        assertTrue(manifest.contains("android:value=\"RANGED_VALUE\""))
        assertTrue(manifest.contains("com.google.android.wearable.permission.BIND_COMPLICATION_PROVIDER"))
        assertTrue(manifest.contains("android.support.wearable.complications.ACTION_COMPLICATION_UPDATE_REQUEST"))
    }

    @Test
    fun tileDescriptionUsesAndroidStringResource() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        val resources = File("src/main/res/values/wear.xml").readText()

        assertTrue(manifest.contains("android:description=\"@string/zara_tile_description\""))
        assertFalse(manifest.contains("android:description=\"Zara status and actions\""))
        assertTrue(resources.contains("<string name=\"zara_tile_description\">Zara status and actions</string>"))
    }

    @Test
    fun providersShareOneExplicitLaunchContract() {
        val build = File("build.gradle.kts").readText()
        assertTrue(build.contains("applicationId = \"ai.zara.app\""))

        val contractFile = File("src/main/java/ai/zara/wear/surface/ZaraWearLaunchTargets.kt")
        assertTrue("launch target authority must exist", contractFile.isFile)
        val contract = contractFile.readText()
        assertTrue(contract.contains("private const val MAIN_PACKAGE = \"ai.zara.app\""))
        assertFalse(contract.contains("private const val MAIN_PACKAGE = \"ai.zara.wear\""))
        assertTrue(contract.contains("ai.zara.wear.WearMainActivity"))
        assertTrue(contract.contains("ai.zara.wear.voice"))
        assertTrue(contract.contains("ai.zara.wear.voice.WearVoiceActivity"))
        assertTrue(contract.contains("ai.zara.action.WEAR_VOICE"))
        assertTrue(contract.contains("ai.zara.action.OPEN_ORG_TODO"))
        assertTrue(contract.contains("ORG_TODO_ID"))
        assertTrue(contract.contains("require(todoId.isNotBlank())"))
        assertTrue(contract.contains("appendPath(todoId)"))
        assertTrue(contract.contains(".setData("))

        val tile = File("src/main/java/ai/zara/wear/surface/ZaraTileService.kt").readText()
        val status = File("src/main/java/ai/zara/wear/surface/ZaraStatusComplicationService.kt").readText()
        val voice = File("src/main/java/ai/zara/wear/surface/ZaraVoiceComplicationService.kt").readText()
        val orgSchedule = File("src/main/java/ai/zara/wear/surface/OrgScheduleComplicationServices.kt").readText()
        listOf(tile, status, voice, orgSchedule).forEach { source ->
            assertFalse(source.contains("CURVE"))
            assertFalse(source.contains("PRIVATE KEY"))
        }
        assertTrue(tile.contains("ZaraWearLaunchTargets"))
        assertTrue(status.contains("ZaraWearLaunchTargets"))
        assertTrue(voice.contains("ZaraWearLaunchTargets"))
        assertTrue(orgSchedule.contains("orgTodoPendingIntent"))
        assertTrue(tile.contains("voicePendingIntent") || tile.contains("voiceComponent"))
        assertTrue(status.contains("mainPendingIntent"))
        assertTrue(voice.contains("voicePendingIntent"))
    }

    @Test
    fun orgScheduleProviderUsesDurableSerializedPushCacheNotPolling() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        val store = File("src/main/java/ai/zara/wear/surface/OrgScheduleSnapshotStore.kt").readText()
        val provider = File("src/main/java/ai/zara/wear/surface/OrgScheduleComplicationServices.kt").readText()

        val orgProviderBlocks = Regex(
            """(?s)<service\s+android:name="ai\.zara\.wear\.complications\.Org[^"]+".*?</service>""",
        ).findAll(manifest).toList()
        assertEquals(7, orgProviderBlocks.size)
        val pushOnlyPeriod = Regex(
            """android:name="android\.support\.wearable\.complications\.UPDATE_PERIOD_SECONDS"\s+android:value="0""",
        )
        orgProviderBlocks.forEach { block ->
            assertTrue(pushOnlyPeriod.containsMatchIn(block.value))
        }

        assertTrue(store.contains("snapshot_v1"))
        assertTrue(store.contains("Context.MODE_PRIVATE"))
        assertTrue(store.contains("@Synchronized"))
        assertTrue(store.contains(".commit()"))
        assertFalse(store.contains(".apply()"))
        assertTrue(provider.contains("NoDataComplicationData"))
        assertTrue(provider.contains("RangedValueComplicationData.Builder"))
        assertTrue(provider.contains("snapshot.currentOrNextId"))
        assertFalse(provider.contains("snapshot.allocations.firstOrNull()"))
    }

    @Test
    fun stableSurfaceDependenciesArePinned() {
        val catalog = File("../gradle/libs.versions.toml").readText()
        assertTrue(catalog.contains("wearTiles = \"1.6.2\""))
        assertTrue(catalog.contains("wearProtoLayout = \"1.4.2\""))
        assertTrue(catalog.contains("wearWatchface = \"1.3.0\""))
    }

    @Test
    fun tilePreviewUsesProductionRendererForBothRoundProfiles() {
        val catalog = File("../gradle/libs.versions.toml").readText()
        assertTrue(catalog.contains("wear-tiles-tooling-preview"))
        assertTrue(catalog.contains("wear-tiles-renderer"))
        assertTrue(catalog.contains("wear-tooling-preview"))

        val build = File("build.gradle.kts").readText()
        assertTrue(build.contains("implementation(libs.wear.tiles.tooling.preview)"))
        assertTrue(build.contains("debugImplementation(libs.wear.tiles.renderer)"))
        assertTrue(build.contains("implementation(libs.wear.tooling.preview)"))

        val previewFile = File("src/main/java/ai/zara/wear/surface/ZaraTilePreview.kt")
        assertTrue("round Tile preview contract must exist", previewFile.isFile)
        val preview = previewFile.readText()
        assertTrue(preview.contains("TilePreviewData"))
        assertTrue(preview.contains("WearDevices.SMALL_ROUND"))
        assertTrue(preview.contains("WearDevices.LARGE_ROUND"))
        assertTrue(preview.contains("buildZaraTile(context, request)"))

        val tile = File("src/main/java/ai/zara/wear/surface/ZaraTileService.kt").readText()
        assertTrue(tile.contains("internal fun buildZaraTile("))
        assertTrue(tile.contains("buildZaraTile(this, requestParams)"))
    }

    @Test
    fun complicationPreviewPayloadsAreCanonicalAndRejectUnsupportedTypes() {
        assertEquals(
            ZaraShortTextComplicationPayload(
                text = "Zara",
                title = "offline",
                contentDescription = "Open Zara",
            ),
            statusComplicationPayload(ComplicationType.SHORT_TEXT),
        )
        assertEquals(
            ZaraShortTextComplicationPayload(
                text = "Voice",
                title = "Zara",
                contentDescription = "Open Zara Voice",
            ),
            voiceComplicationPayload(ComplicationType.SHORT_TEXT),
        )

        listOf(ComplicationType.LONG_TEXT, ComplicationType.RANGED_VALUE).forEach { unsupported ->
            assertNull(statusComplicationPayload(unsupported))
            assertNull(voiceComplicationPayload(unsupported))
        }
    }
}
