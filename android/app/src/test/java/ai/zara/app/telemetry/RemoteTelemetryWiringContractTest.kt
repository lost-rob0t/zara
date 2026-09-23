package ai.zara.app.telemetry

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class RemoteTelemetryWiringContractTest {
    @Test fun `app session wires typed failures telemetry diagnostics and restore`() {
        val source = appSessionSource()

        for (required in listOf(
            "SessionTelemetry()",
            "controller.setConnectionLossListener",
            "actor.setConnectionFailureObserver",
            "controller.clientReportedFailure",
            "telemetry.onRuntimeStateChanged",
            "telemetry.onClientFailure",
            "DiagnosticsV2.render",
            "RemoteUnavailableException(",
            "ClientEventNames.SESSION_RESTORE_BEGIN",
            "ClientEventNames.SESSION_RESTORE_COMPLETE",
            "ClientEventNames.SESSION_RESTORE_FAILED",
            "telemetry.voiceStage(VoiceStage.CAPTURE, VoiceStageProgress.RUNNING)",
            "telemetry.voiceStage(VoiceStage.STT, VoiceStageProgress.COMPLETE)",
            "telemetry.voiceStage(VoiceStage.TTS, VoiceStageProgress.RUNNING)",
            "telemetry.voiceStage(VoiceStage.PLAYBACK, VoiceStageProgress.COMPLETE)",
            "telemetry.onTurnCompleted",
        )) {
            assertTrue("AndroidAppSession must wire: $required", source.contains(required))
        }
    }

    @Test fun `remote mode preconditions fail with typed unavailability not generic state`() {
        val source = appSessionSource()
        assertTrue(
            "remote fast-fail must use the typed RemoteUnavailableException",
            !source.contains("IllegalStateException(\"Remote mode requires an authenticated Zara server\")"),
        )
    }

    private fun appSessionSource(): String {
        val sessionFile = projectFile("app/src/main/java/ai/zara/app/AndroidAppSession.kt")
        assertTrue("AndroidAppSession.kt is required", sessionFile.isFile)
        return sessionFile.readText()
    }

    private fun projectFile(relativePath: String): File {
        val cwd = File(System.getProperty("user.dir"))
        val candidates = listOf(
            File(cwd, relativePath),
            File(cwd, "android/$relativePath"),
            File(cwd.parentFile ?: cwd, relativePath),
            File(cwd.parentFile ?: cwd, "android/$relativePath"),
        )
        return candidates.firstOrNull(File::exists) ?: candidates.first()
    }
}
