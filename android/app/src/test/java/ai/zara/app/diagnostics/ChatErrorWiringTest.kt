package ai.zara.app.diagnostics

import java.io.File
import ai.zara.app.prolog.PrologQueryPolicy
import org.junit.Assert.*
import org.junit.Test

class ChatErrorWiringTest {
    @Test fun failedBubbleCopiesItsStoredSnapshotRatherThanLiveGlobalExport() {
        val ui = File("src/main/java/ai/zara/app/ui/ZaraApp.kt").readText()
        val host = File("src/main/java/ai/zara/app/MainActivity.kt").readText()
        assertTrue(ui.contains("Copy error details"))
        assertTrue(ui.contains("turn.errorDetails"))
        assertTrue(ui.contains("liveRegion = LiveRegionMode.Polite"))
        assertTrue(host.contains("errorDetails = capturedErrorDetails"))
        val copy = host.substringAfter("private fun copyChatErrorDetails(").substringBefore("private fun copyDiagnostics()")
        assertTrue(copy.contains("ClipData.newPlainText(\"Zara error details\", details)"))
        assertFalse(copy.contains("exportDiagnostics"))
    }

    @Test fun installedFailureFixturePassesTheRealBoundedQueryPolicy() {
        val query = "?- Result is 1/0, Result = zara_diagnostic_private_fixture_1523."
        assertTrue(PrologQueryPolicy.requireSafe(query).contains("Result is 1/0"))
        val acceptance = File("../integration/chat_error_acceptance.py").readText()
        assertTrue(acceptance.contains("f'?- Result is 1/0, Result = {PRIVATE_MARKER}.'"))
    }

    @Test fun pairingEnabledEntryPointForwardsTheSameCopyCallback() {
        val wrapper = File("src/main/java/ai/zara/app/ui/PairingZaraApp.kt").readText()
        assertTrue(wrapper.contains("onCopyChatErrorDetails: (String) -> Boolean"))
        assertTrue(wrapper.contains("onCopyChatErrorDetails = onCopyChatErrorDetails"))
    }

    @Test fun localFailureIsCapturedBeforeItBecomesAnUnsuccessfulTurnResult() {
        val session = File("src/main/java/ai/zara/app/AndroidAppSession.kt").readText()
        assertTrue(session.contains("errorDetails = captureChatError("))
        assertTrue(session.contains("secondaryError = if (symbolicFailure != null) failure else null"))
        assertTrue(session.contains("telemetry.onLocalFailure("))
        assertTrue(session.contains("runtimeModeName = runtimeModeAtSubmit"))
        assertTrue("Auto route failures must carry a local snapshot", session.contains("fun captureLocalFailure("))
        assertTrue(session.contains("return captureLocalFailure(CompletableFuture.failedFuture(error))"))
        assertTrue(session.contains("return captureLocalFailure(future.thenApply"))
        assertTrue(session.contains("ChatErrorDetails.retainedFailure(error)?.let { return it }"))
    }
}
