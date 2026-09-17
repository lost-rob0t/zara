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
    fun phoneUiExposesWatchSetupAndLatestWearApkChannel() {
        val shell = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val surface = File("src/main/java/ai/zara/app/ui/WatchSetupSurface.kt").readText()
        val repository = File("src/main/java/ai/zara/app/watch/WearApkRepository.kt").readText()

        assertTrue(shell.contains("Watch(\"Watch\""))
        assertTrue(shell.contains("AppSurface.Watch -> WatchSetupSurface"))
        assertTrue(surface.contains("WatchInstallPolicy.transportNotice"))
        assertTrue(repository.contains("zara-wear-latest.apk"))
        assertTrue(repository.contains("zara-wear-latest.apk.sha256"))
    }
}
