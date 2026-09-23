package ai.zara.app.diagnostics

import java.io.File
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class LocalRuntimeDiagnosticsTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun exportContainsStructuredFailureWithoutRawSecrets() {
        val file = File(temporary.newFolder("diag"), "local-runtime.log")
        val diagnostics = LocalRuntimeDiagnostics(file)

        diagnostics.record(
            event = "local_model.generate.failed",
            fields = mapOf("phase" to "failed", "prompt_length" to 42),
            error = IllegalStateException("token=super-secret backend unavailable"),
        )

        val exported = diagnostics.export(
            mapOf(
                "version" to "0.2.2-alpha",
                "source_sha" to "0123456789abcdef",
            )
        )

        assertTrue(exported.contains("ZARA-LOCAL-DIAGNOSTICS/1"))
        assertTrue(exported.contains("event=local_model.generate.failed"))
        assertTrue(exported.contains("java.lang.IllegalStateException"))
        assertTrue(exported.contains("token=<redacted>"))
        assertFalse(exported.contains("super-secret"))
        assertTrue(exported.contains("version=0.2.2-alpha"))
    }

    @Test
    fun clearRemovesPersistedEvents() {
        val file = File(temporary.newFolder("clear"), "local-runtime.log")
        val diagnostics = LocalRuntimeDiagnostics(file)
        diagnostics.record("local_server.ready", mapOf("generation" to 1))

        diagnostics.clear()

        val exported = diagnostics.export(emptyMap())
        assertTrue(exported.contains("(no recorded events)"))
    }
}
