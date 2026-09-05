package ai.zara.app.voice

object ZaraVoiceCodec {
    private val marker = "ZARA/1".encodeToByteArray()
    private const val maxIdBytes = 128
    private const val contentType = "audio/pcm;codec=pcm_s16le"

    fun encodeStart(
        requestId: String,
        sessionId: String,
        conversationId: String?,
        streamId: String,
        timestampNs: Long,
    ): List<ByteArray> = envelope(
        type = "audio.input.start",
        requestId = requestId,
        sessionId = sessionId,
        conversationId = conversationId,
        streamId = streamId,
        timestampNs = timestampNs,
        extraBeforeId = "\"body\":{\"channels\":1,\"codec\":\"pcm_s16le\",\"frame_samples\":512,\"sample_rate\":16000},",
    )

    fun encodeChunk(
        requestId: String,
        sessionId: String,
        conversationId: String?,
        streamId: String,
        sequence: Long,
        timestampNs: Long,
        pcm: ByteArray,
    ): List<ByteArray> {
        require(sequence >= 0) { "voice sequence must be non-negative" }
        require(pcm.size == ManualVoiceCapture.PCM_FRAME_BYTES) {
            "voice PCM frame must be exactly ${ManualVoiceCapture.PCM_FRAME_BYTES} bytes"
        }
        val id = token("id", requestId)
        val session = token("session_id", sessionId)
        val stream = token("stream_id", streamId)
        val conversation = conversationId?.let { token("conversation_id", it) }
        require(timestampNs >= 0) { "timestamp must be non-negative" }
        val json = buildString {
            append('{')
            append("\"content_type\":\"").append(contentType).append("\",")
            if (conversation != null) append("\"conversation_id\":").append(jsonString(conversation)).append(',')
            append("\"id\":").append(jsonString(id)).append(',')
            append("\"payload_count\":1,")
            append("\"seq\":").append(sequence).append(',')
            append("\"session_id\":").append(jsonString(session)).append(',')
            append("\"stream_id\":").append(jsonString(stream)).append(',')
            append("\"timestamp_ns\":").append(timestampNs).append(',')
            append("\"type\":\"audio.input.chunk\"")
            append('}')
        }
        return listOf(marker.copyOf(), json.encodeToByteArray(), pcm.copyOf())
    }

    fun encodeCommit(
        requestId: String,
        sessionId: String,
        conversationId: String?,
        streamId: String,
        timestampNs: Long,
    ): List<ByteArray> = envelope(
        type = "audio.input.commit",
        requestId = requestId,
        sessionId = sessionId,
        conversationId = conversationId,
        streamId = streamId,
        timestampNs = timestampNs,
    )

    fun encodeCancel(
        requestId: String,
        sessionId: String,
        conversationId: String?,
        streamId: String,
        timestampNs: Long,
    ): List<ByteArray> = envelope(
        type = "audio.input.cancel",
        requestId = requestId,
        sessionId = sessionId,
        conversationId = conversationId,
        streamId = streamId,
        timestampNs = timestampNs,
    )

    private fun envelope(
        type: String,
        requestId: String,
        sessionId: String,
        conversationId: String?,
        streamId: String,
        timestampNs: Long,
        extraBeforeId: String = "",
    ): List<ByteArray> {
        val id = token("id", requestId)
        val session = token("session_id", sessionId)
        val stream = token("stream_id", streamId)
        val conversation = conversationId?.let { token("conversation_id", it) }
        require(timestampNs >= 0) { "timestamp must be non-negative" }
        val json = buildString {
            append('{')
            append(extraBeforeId)
            if (conversation != null) append("\"conversation_id\":").append(jsonString(conversation)).append(',')
            append("\"id\":").append(jsonString(id)).append(',')
            append("\"payload_count\":0,")
            append("\"session_id\":").append(jsonString(session)).append(',')
            append("\"stream_id\":").append(jsonString(stream)).append(',')
            append("\"timestamp_ns\":").append(timestampNs).append(',')
            append("\"type\":").append(jsonString(type))
            append('}')
        }
        return listOf(marker.copyOf(), json.encodeToByteArray())
    }

    private fun token(name: String, value: String): String {
        val bytes = value.encodeToByteArray()
        require(value.isNotBlank() && bytes.size <= maxIdBytes) { "$name is invalid" }
        require(value.all { it.code in 0x21..0x7e }) { "$name must be printable ASCII" }
        return value
    }

    private fun jsonString(value: String): String = buildString {
        append('"')
        value.forEach { character ->
            when (character) {
                '"' -> append("\\\"")
                '\\' -> append("\\\\")
                else -> append(character)
            }
        }
        append('"')
    }
}
