package ai.zara.app.watch

import java.io.File
import org.junit.Assert.assertTrue
import org.junit.Test

class WatchInstallerContractTest {
    @Test
    fun phoneBuildIncludesWearDiscoveryAndWirelessAdbInstallerDependencies() {
        val catalog = File("../gradle/libs.versions.toml").readText()
        val build = File("build.gradle.kts").readText()

        assertTrue(catalog.contains("play-services-wearable"))
        assertTrue(catalog.contains("libadb-android"))
        assertTrue(catalog.contains("conscrypt-android"))
        assertTrue(catalog.contains("bcpkix"))
        assertTrue(build.contains("libs.play.services.wearable"))
        assertTrue(build.contains("libs.libadb.android"))
        assertTrue(build.contains("libs.conscrypt.android"))
        assertTrue(build.contains("libs.bcpkix"))
    }

    @Test
    fun watchAndPhoneUseOneDataLayerIdentity() {
        val wearBuild = File("../wear-app/build.gradle.kts").readText()
        val capability = File("../wear-app/src/main/res/values/wear.xml").readText()

        assertTrue(wearBuild.contains("applicationId = \"ai.zara.app\""))
        assertTrue(capability.contains("zara_watch"))
    }

    @Test
    fun zaraAgendaIsASeparateResourceOnlyWatchFacePackage() {
        val settings = File("../settings.gradle.kts").readText()
        val faceBuild = File("../wear-watchface/build.gradle.kts").readText()
        val faceManifest = File("../wear-watchface/src/main/AndroidManifest.xml").readText()
        val face = File("../wear-watchface/src/main/res/raw/watchface.xml").readText()

        assertTrue(settings.contains("include(\":wear-watchface\")"))
        assertTrue(faceBuild.contains("applicationId = \"ai.zara.agenda\""))
        assertTrue(faceManifest.contains("android:hasCode=\"false\""))
        assertTrue(faceManifest.contains("com.google.wear.watchface.format.version"))
        assertTrue(face.contains("ZaraNextTodoComplicationService"))
        assertTrue(face.contains("ZaraTodoSummaryComplicationService"))
        assertTrue(face.contains("ZaraTodoSyncComplicationService"))
        assertTrue(face.contains("<AnalogClock"))
        assertTrue(face.contains("mode=\"AMBIENT\""))
    }

    @Test
    fun zaraPhoneAppShipsPairedWearAndAgendaInstallLifecycle() {
        val manifest = File("src/main/AndroidManifest.xml").readText()
        val activity = File("src/main/java/ai/zara/app/watch/WatchSetupActivity.kt").readText()
        val bluetooth = File("src/main/java/ai/zara/app/watch/BluetoothWatchScanner.kt").readText()
        val controller = File("src/main/java/ai/zara/app/watch/WatchSetupController.kt").readText()
        val surface = File("src/main/java/ai/zara/app/ui/WatchSetupSurface.kt").readText()
        val repository = File("src/main/java/ai/zara/app/watch/WearApkRepository.kt").readText()

        assertTrue(manifest.contains(".watch.WatchSetupActivity"))
        assertTrue(manifest.contains("Zara Watch Setup"))
        assertTrue(manifest.contains("android.permission.BLUETOOTH_CONNECT"))
        assertTrue(activity.contains("RequestMultiplePermissions"))
        assertTrue(activity.contains("BLUETOOTH_CONNECT"))
        assertTrue(activity.contains("onUninstall = controller::uninstallZara"))
        assertTrue(bluetooth.contains("bondedDevices"))
        assertTrue(activity.contains("WatchSetupController"))
        assertTrue(surface.contains("WatchInstallPolicy.transportNotice"))
        assertTrue(surface.contains("Install Zara + Agenda"))
        assertTrue(surface.contains("Uninstall Zara"))
        assertTrue(repository.contains("zara-wear-latest.apk"))
        assertTrue(repository.contains("zara-wear-latest.apk.sha256"))
        assertTrue(repository.contains("zara-agenda-latest.apk"))
        assertTrue(repository.contains("zara-agenda-latest.apk.sha256"))
        assertTrue(controller.contains("pm uninstall ai.zara.wear"))
        assertTrue(controller.contains("pm uninstall ai.zara.agenda"))
    }
}
