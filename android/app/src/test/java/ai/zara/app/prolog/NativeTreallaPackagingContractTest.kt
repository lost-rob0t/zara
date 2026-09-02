package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class NativeTreallaPackagingContractTest {
    @Test
    fun androidBuildPackagesTheNativeTreallaBridgeForSupportedAbis() {
        val appGradle = projectFile("app/build.gradle.kts").readText()

        assertTrue(appGradle.contains("externalNativeBuild"))
        assertTrue(appGradle.contains("src/main/cpp/CMakeLists.txt"))
        assertTrue(appGradle.contains("arm64-v8a"))
        assertTrue(appGradle.contains("x86_64"))
        assertTrue(appGradle.contains("ZARA_ANDROID_NDK_VERSION"))
    }

    @Test
    fun androidOwnedNixToolchainPinsNdkAndTreallaOutsideGradle() {
        val flake = projectFile("flake.nix").readText()
        val androidGate = projectFile("../scripts/test-android.sh").readText()

        assertTrue(flake.contains("includeNDK = true"))
        assertTrue(flake.contains("ndkVersions"))
        assertTrue(flake.contains("b25ccfb8e485a697bb47f1947d6fb8e0ad4e6aaf"))
        assertTrue(androidGate.contains("ZARA_TREALLA_SOURCE_DIR"))
        assertTrue(androidGate.contains("ZARA_TREALLA_LIBRARY_ROOT"))
        assertFalse(androidGate.contains("git clone"))
        assertFalse(androidGate.contains("curl "))
    }

    private fun projectFile(relativePath: String): File {
        val cwd = File(System.getProperty("user.dir"))
        val candidates = listOf(
            File(cwd, relativePath),
            File(cwd, "android/$relativePath"),
            File(cwd.parentFile ?: cwd, relativePath),
            File(cwd.parentFile ?: cwd, "android/$relativePath")
        )
        return candidates.firstOrNull(File::exists) ?: candidates.first()
    }
}
