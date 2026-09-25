package ai.zara.app.samsunghealth

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class SamsungHealthApplicationWiringContractTest {
    @Test
    fun applicationInstallsPrologSourceBeforeSessionAndExposesController() {
        val source = projectFile("app/src/main/java/ai/zara/app/ZaraApplication.kt").readText()
        val install = source.indexOf("SamsungHealthPrologPlugin.install")
        val session = source.indexOf("AndroidAppSession(")
        assertTrue(install >= 0)
        assertTrue(session > install)
        assertTrue(
            source.contains(
                "canonicalExpertInvocationPortProvider = canonicalExpertInvocationPortProvider",
            ),
        )
        assertTrue(source.contains("val samsungHealthPlugin: SamsungHealthAndroidPlugin"))
    }

    @Test
    fun vendorSdkIsOptionalAndNeverCommittedAsAnApplicationRequirement() {
        val build = projectFile("app/build.gradle.kts").readText()
        val ignore = projectFile(".gitignore").readText()
        assertTrue(build.contains("HAS_SAMSUNG_HEALTH_SDK"))
        assertTrue(build.contains("samsung-health-data-api-*.aar"))
        assertTrue(build.contains("src/samsungHealthSdk/java"))
        assertTrue(ignore.contains("app/libs/*.aar"))
    }

    @Test
    fun newSdkDoesNotReintroduceLegacySamsungHealthManifestPermissions() {
        val manifest = projectFile("app/src/main/AndroidManifest.xml").readText()
        assertFalse(manifest.contains("com.samsung.android.health.permission.read"))
        assertFalse(manifest.contains("com.samsung.android.health.permission.write"))
        assertFalse(manifest.contains("com.sec.android.app.shealth"))
    }

    private fun projectFile(path: String): File {
        var base: File? = File(System.getProperty("user.dir")).absoluteFile
        while (base != null) {
            val candidates = listOf(
                File(base, path),
                File(base, "android/$path"),
            )
            candidates.firstOrNull(File::exists)?.let { return it }
            base = base.parentFile
        }
        error("Unable to resolve project file '$path' from ${System.getProperty("user.dir")}")
    }
}
