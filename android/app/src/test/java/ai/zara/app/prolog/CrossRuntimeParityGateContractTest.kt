package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class CrossRuntimeParityGateContractTest {
    @Test
    fun androidGateRunsExecutableSwiTreallaParity() {
        val androidGate = projectFile("../scripts/test-android.sh")
        val parityGate = projectFile("../scripts/test-android-semantic-parity.sh")

        assertTrue("Android gate must exist", androidGate.isFile)
        assertTrue(
            "Android gate must execute cross-runtime semantic parity",
            androidGate.readText().contains("test-android-semantic-parity.sh")
        )
        assertTrue("cross-runtime parity gate is required", parityGate.isFile)

        val text = parityGate.readText()
        assertTrue("parity gate must execute SWI-Prolog", text.contains("swipl"))
        assertTrue(
            "parity gate must use the pinned Trealla source supplied by the Android Nix shell",
            text.contains("ZARA_TREALLA_SOURCE_DIR")
        )
        assertTrue(
            "parity gate must consume the canonical semantic corpus",
            text.contains("kb/semantic_corpus.pl")
        )
        assertTrue(
            "parity gate must execute the canonical resolver",
            text.contains("resolve_frames")
        )
        assertTrue(
            "parity gate must validate corpus expectations independently",
            text.contains("ExpectedNormalized")
        )
        listOf("curl ", "wget ", "git clone", "https://", "http://").forEach { forbidden ->
            assertFalse("parity gate must not fetch moving network inputs: $forbidden", text.contains(forbidden))
        }
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
