package ai.zara.app.diagnostics

import org.junit.Assert.*
import org.junit.Test

class ChatErrorDetailsTest {
    private fun capture(error: Throwable = IllegalStateException("symbolic predicate unavailable")) =
        ChatErrorDetails.capture(
            version = "0.3.1-alpha", versionCode = 6, sourceSha = "370834cc",
            runtimeMode = "local", conversationId = "local-chat-a", turnId = "turn-a",
            requestId = "request-a", code = "local.symbolic.failed", error = error,
            events = listOf("local_symbolic.failed", "local_model.generate.failed"),
            localServerPhase = "ready", localServerGeneration = 1, capturedAtMillis = 1,
        )

    @Test fun capturesActualFailureWhenGlobalTelemetryIsEmpty() {
        val details = capture()
        assertTrue(details.contains("ZARA-CHAT-ERROR/1"))
        for (field in listOf("version=0.3.1-alpha", "version_code=6", "source_sha=370834cc",
            "runtime_mode=local", "turn_id=turn-a", "request_id=request-a", "conversation_id=local-chat-a",
            "code=local.symbolic.failed", "java.lang.IllegalStateException", "cause_code=invalid_state",
            "local_server_phase=ready", "local_symbolic.failed")) assertTrue(field, details.contains(field))
    }

    @Test fun redactsSecretsQuotedInputsAndBoundsUntrustedCauses() {
        val details = capture(IllegalStateException(
            "Authorization: Bearer abc.def.secret api_key=\"two words secret\" token=abc123 " +
            "query=\"unrelated private utterance\" https://alice:password@example.test " + "x".repeat(30_000)))
        for (secret in listOf("abc.def.secret", "two words secret", "abc123", "unrelated private utterance", "alice:password")) {
            assertFalse(secret, details.contains(secret))
        }
        assertFalse(details.contains("cause_message="))
        assertTrue(details.toByteArray(Charsets.UTF_8).size <= ChatErrorDetails.MAX_BYTES)
    }

    @Test fun excludesUnquotedPrivateInputAndNonAssignmentSecrets() {
        val details = capture(IllegalStateException("failed input: private utterance marker; password is hunter2"))
        for (secret in listOf("private utterance marker", "hunter2", "failed input:")) {
            assertFalse(secret, details.contains(secret))
        }
        assertTrue(details.contains("cause_code=invalid_state"))
        assertTrue(details.contains("cause_class=java.lang.IllegalStateException"))
    }

    @Test fun retainedLocalFailureSurvivesCompletionWrappers() {
        val details = capture()
        val error = java.util.concurrent.CompletionException(ChatErrorDetails.RetainedFailure(details, IllegalStateException("private")))
        assertEquals(details, ChatErrorDetails.retainedFailure(error))
        assertNull(ChatErrorDetails.retainedFailure(IllegalStateException("unrelated")))
    }

    @Test fun missingCauseIsExplicitAndDoesNotInventAModelFailure() {
        val details = ChatErrorDetails.capture(version="test", versionCode=1, sourceSha="sha", runtimeMode="symbolic",
            conversationId="chat", turnId=null, requestId="request", code="chat.turn.failed", error=null)
        assertTrue(details.contains("cause_class=unavailable"))
        assertTrue(details.contains("turn_id=unknown"))
        assertFalse(details.contains("remote.not_connected"))
    }
}
