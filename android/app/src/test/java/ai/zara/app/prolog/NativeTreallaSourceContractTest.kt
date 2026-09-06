package ai.zara.app.prolog

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class NativeTreallaSourceContractTest {
    @Test
    fun nativeAdapterDefinesTheFrozenJniSurfaceAndTreallaLifecycle() {
        val source = projectFile("app/src/main/cpp/zara_trealla_jni.c")
        assertTrue("native Trealla adapter source is required", source.isFile)

        val text = source.readText()
        listOf(
            "Java_ai_zara_app_prolog_JniTreallaNativeApi_initialize",
            "Java_ai_zara_app_prolog_JniTreallaNativeApi_evaluate",
            "Java_ai_zara_app_prolog_JniTreallaNativeApi_shutdown",
            "pl_create(",
            "pl_consult(",
            "pl_query(",
            "pl_binding(",
            "\"Result\"",
            "pl_term_text(",
            "pl_free(",
            "pl_destroy("
        ).forEach { required ->
            assertTrue("native adapter must contain $required", text.contains(required))
        }

        listOf("curl", "http://", "https://", "socket(", "connect(").forEach { forbidden ->
            assertFalse("native semantic adapter must not contain $forbidden", text.contains(forbidden))
        }
    }

    @Test
    fun nativeAdapterFailsClosedInsteadOfSilentlyTruncatingSemanticResults() {
        val source = projectFile("app/src/main/cpp/zara_trealla_jni.c")
        assertTrue("native Trealla adapter source is required", source.isFile)

        val text = source.readText()
        assertTrue(text.contains("ZARA_MAX_SEMANTIC_RESULTS 256"))
        assertTrue(
            "native semantic adapter must detect a result beyond the bounded result set",
            text.contains("result_overflow = pl_redo(query)"),
        )
        assertTrue(
            "native semantic adapter must reject overflow instead of returning a truncated success",
            text.contains("Trealla semantic result limit exceeded"),
        )
    }

    @Test
    fun cmakeRequiresAnExplicitPinnedTreallaSourceTree() {
        val cmake = projectFile("app/src/main/cpp/CMakeLists.txt")
        assertTrue("native Trealla CMake contract is required", cmake.isFile)

        val text = cmake.readText()
        assertTrue(text.contains("ZARA_TREALLA_SOURCE_DIR"))
        assertTrue(text.contains("trealla.h"))
        assertTrue(text.contains("zara_trealla"))
        assertFalse("native build must not fetch moving source", text.contains("FetchContent"))
        assertFalse("native build must not download source", text.contains("http://") || text.contains("https://"))
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
