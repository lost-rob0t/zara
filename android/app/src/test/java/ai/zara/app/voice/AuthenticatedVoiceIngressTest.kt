package ai.zara.app.voice

import java.util.concurrent.CompletableFuture
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class AuthenticatedVoiceIngressTest {
    @Test fun `ingress forwards canonical context and waits for typed acknowledgements`() {
        val client = RecordingVoiceCommandClient()
        val ingress = AuthenticatedVoiceIngress(client)
        val context = VoiceCaptureContext(
            sessionId = "session-1",
            conversationId = "conversation-1",
            streamId = "mic-1",
        )
        val pcm = ByteArray(ManualVoiceCapture.PCM_FRAME_BYTES) { index -> index.toByte() }

        ingress.start(context)
        ingress.chunk(context, 0, pcm)
        ingress.commit(context)

        assertEquals(
            listOf(
                VoiceCall.Start(context),
                VoiceCall.Chunk(context, 0, pcm),
                VoiceCall.Commit(context),
            ),
            client.calls,
        )
        assertArrayEquals(pcm, (client.calls[1] as VoiceCall.Chunk).pcm)
    }

    @Test fun `remote rejection propagates instead of advancing local voice lifecycle`() {
        val failure = IllegalStateException("audio input rejected")
        val client = RecordingVoiceCommandClient(startFailure = failure)
        val capture = ManualVoiceCapture(AuthenticatedVoiceIngress(client))
        val context = VoiceCaptureContext("session-1", null, "mic-1")

        val thrown = assertThrows(Exception::class.java) {
            capture.begin(context, permissionGranted = true, connected = true)
        }

        assertEquals(failure, rootCause(thrown))
        assertEquals(ManualVoiceState.Idle, capture.state())
    }

    @Test fun `cancel uses authenticated command owner and remains terminal`() {
        val client = RecordingVoiceCommandClient()
        val capture = ManualVoiceCapture(AuthenticatedVoiceIngress(client))
        val context = VoiceCaptureContext("session-1", "conversation-1", "mic-1")

        capture.begin(context, permissionGranted = true, connected = true)
        capture.cancel()

        assertEquals(
            listOf(VoiceCall.Start(context), VoiceCall.Cancel(context)),
            client.calls,
        )
        assertEquals(ManualVoiceState.Idle, capture.state())
    }

    private fun rootCause(error: Throwable): Throwable {
        var current = error
        while (current.cause != null && current.cause !== current) current = current.cause!!
        return current
    }
}

private sealed interface VoiceCall {
    data class Start(val context: VoiceCaptureContext) : VoiceCall
    data class Chunk(
        val context: VoiceCaptureContext,
        val sequence: Long,
        val pcm: ByteArray,
    ) : VoiceCall {
        override fun equals(other: Any?): Boolean =
            other is Chunk && context == other.context && sequence == other.sequence && pcm.contentEquals(other.pcm)

        override fun hashCode(): Int = 31 * (31 * context.hashCode() + sequence.hashCode()) + pcm.contentHashCode()
    }
    data class Commit(val context: VoiceCaptureContext) : VoiceCall
    data class Cancel(val context: VoiceCaptureContext) : VoiceCall
}

private class RecordingVoiceCommandClient(
    private val startFailure: Throwable? = null,
) : VoiceCommandClient {
    val calls = mutableListOf<VoiceCall>()

    override fun startVoice(context: VoiceCaptureContext): CompletableFuture<Unit> {
        calls += VoiceCall.Start(context)
        return completion(startFailure)
    }

    override fun sendVoiceChunk(
        context: VoiceCaptureContext,
        sequence: Long,
        pcm: ByteArray,
    ): CompletableFuture<Unit> {
        calls += VoiceCall.Chunk(context, sequence, pcm.copyOf())
        return CompletableFuture.completedFuture(Unit)
    }

    override fun commitVoice(context: VoiceCaptureContext): CompletableFuture<Unit> {
        calls += VoiceCall.Commit(context)
        return CompletableFuture.completedFuture(Unit)
    }

    override fun cancelVoice(context: VoiceCaptureContext): CompletableFuture<Unit> {
        calls += VoiceCall.Cancel(context)
        return CompletableFuture.completedFuture(Unit)
    }

    private fun completion(failure: Throwable?): CompletableFuture<Unit> =
        if (failure == null) CompletableFuture.completedFuture(Unit)
        else CompletableFuture.failedFuture(failure)
}
