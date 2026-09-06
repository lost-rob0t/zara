package ai.zara.app.voice

import ai.zara.app.runtime.ZaraWireException
import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets

sealed interface VoiceStreamEvent {
    val sessionId: String
    val streamId: String

    data class Transcript(
        override val sessionId: String,
        val conversationId: String,
        override val streamId: String,
        val sequence: Long,
        val text: String,
        val final: Boolean,
    ) : VoiceStreamEvent

    data class AudioStarted(
        override val sessionId: String,
        val turnId: String,
        override val streamId: String,
        val sampleRate: Int,
        val channels: Int,
    ) : VoiceStreamEvent

    data class AudioChunk(
        override val sessionId: String,
        val turnId: String,
        override val streamId: String,
        val sequence: Long,
        val pcm: ByteArray,
    ) : VoiceStreamEvent

    data class AudioDone(
        override val sessionId: String,
        val turnId: String,
        override val streamId: String,
    ) : VoiceStreamEvent
}

object ZaraVoiceStreamCodec {
    private val marker = "ZARA/1".encodeToByteArray()
    private const val maxEnvelopeBytes = 64 * 1024
    private const val maxAudioPayloadBytes = 1024 * 1024
    private const val maxTokenBytes = 128
    private const val maxTextBytes = 1024 * 1024
    private val envelopeKeys = setOf(
        "type", "id", "reply_to", "session_id", "conversation_id", "turn_id",
        "stream_id", "seq", "timestamp_ns", "trace_id", "content_type",
        "payload_count", "flags", "body",
    )

    fun decode(frames: List<ByteArray>): VoiceStreamEvent {
        if (frames.size !in 2..3 || !frames[0].contentEquals(marker)) {
            throw ZaraWireException("invalid ZARA/1 voice stream frame")
        }
        if (frames[1].size > maxEnvelopeBytes) {
            throw ZaraWireException("voice stream envelope exceeds byte limit")
        }
        val envelope = VoiceStreamJsonParser(decodeUtf8(frames[1])).parseObject()
        rejectUnknown(envelope, envelopeKeys, "voice stream envelope")
        requireLong(envelope, "timestamp_ns", minimum = 0)
        token("id", requireString(envelope, "id", maxTokenBytes))
        val type = requireString(envelope, "type", 64)
        val session = token("session_id", requireString(envelope, "session_id", maxTokenBytes))
        val stream = token("stream_id", requireString(envelope, "stream_id", maxTokenBytes))
        val payloadCount = requireLong(envelope, "payload_count", minimum = 0)
        val conversation = optionalToken(envelope, "conversation_id")
        val turn = optionalToken(envelope, "turn_id")
        val sequence = optionalLong(envelope, "seq")
        val contentType = optionalString(envelope, "content_type", 128)
        val body = optionalObject(envelope, "body")

        return when (type) {
            "voice.transcript.partial", "voice.transcript.final" -> {
                requireNoPayloads(frames, payloadCount, type)
                if (turn != null) throw ZaraWireException("$type must not carry turn_id")
                if (contentType != null) throw ZaraWireException("$type must not carry content_type")
                rejectUnknown(body, setOf("text"), "$type body")
                VoiceStreamEvent.Transcript(
                    sessionId = session,
                    conversationId = conversation
                        ?: throw ZaraWireException("$type requires conversation_id"),
                    streamId = stream,
                    sequence = sequence?.takeIf { it >= 0 }
                        ?: throw ZaraWireException("$type requires non-negative seq"),
                    text = requireString(body, "text", maxTextBytes),
                    final = type == "voice.transcript.final",
                )
            }
            "audio.output.start" -> {
                requireNoPayloads(frames, payloadCount, type)
                if (sequence != null) throw ZaraWireException("audio.output.start must not carry seq")
                if (contentType != null) throw ZaraWireException("audio.output.start must not carry content_type")
                rejectUnknown(body, setOf("codec", "sample_rate", "channels"), "audio.output.start body")
                if (requireString(body, "codec", 32) != "pcm_s16le") {
                    throw ZaraWireException("unsupported audio output codec")
                }
                val sampleRate = positiveInt(body, "sample_rate")
                val channels = positiveInt(body, "channels")
                if (channels != 1) throw ZaraWireException("Android audio output requires mono")
                VoiceStreamEvent.AudioStarted(
                    sessionId = session,
                    turnId = turn ?: throw ZaraWireException("audio.output.start requires turn_id"),
                    streamId = stream,
                    sampleRate = sampleRate,
                    channels = channels,
                )
            }
            "audio.output.chunk" -> {
                if (payloadCount != 1L || frames.size != 3) {
                    throw ZaraWireException("audio.output.chunk requires one payload frame")
                }
                if (body.isNotEmpty()) throw ZaraWireException("audio.output.chunk must not carry body")
                if (contentType != "audio/pcm;codec=pcm_s16le") {
                    throw ZaraWireException("unsupported audio output content type")
                }
                val pcm = frames[2]
                if (pcm.size > maxAudioPayloadBytes) {
                    throw ZaraWireException("audio output payload exceeds byte limit")
                }
                if (pcm.isEmpty() || pcm.size % 2 != 0) {
                    throw ZaraWireException("audio output payload must contain whole pcm_s16le samples")
                }
                VoiceStreamEvent.AudioChunk(
                    sessionId = session,
                    turnId = turn ?: throw ZaraWireException("audio.output.chunk requires turn_id"),
                    streamId = stream,
                    sequence = sequence?.takeIf { it >= 0 }
                        ?: throw ZaraWireException("audio.output.chunk requires non-negative seq"),
                    pcm = pcm.copyOf(),
                )
            }
            "audio.output.done" -> {
                requireNoPayloads(frames, payloadCount, type)
                if (sequence != null) throw ZaraWireException("audio.output.done must not carry seq")
                if (contentType != null) throw ZaraWireException("audio.output.done must not carry content_type")
                if (body.isNotEmpty()) throw ZaraWireException("audio.output.done must not carry body")
                VoiceStreamEvent.AudioDone(
                    sessionId = session,
                    turnId = turn ?: throw ZaraWireException("audio.output.done requires turn_id"),
                    streamId = stream,
                )
            }
            else -> throw ZaraWireException("unsupported voice stream event type")
        }
    }

    private fun requireNoPayloads(frames: List<ByteArray>, payloadCount: Long, type: String) {
        if (payloadCount != 0L || frames.size != 2) throw ZaraWireException("$type does not accept payload frames")
    }

    private fun positiveInt(value: Map<String, Any?>, key: String): Int {
        val raw = requireLong(value, key, minimum = 1)
        if (raw > Int.MAX_VALUE) throw ZaraWireException("$key is too large")
        return raw.toInt()
    }

    private fun decodeUtf8(bytes: ByteArray): String = try {
        StandardCharsets.UTF_8.newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .decode(ByteBuffer.wrap(bytes))
            .toString()
    } catch (error: Exception) {
        throw ZaraWireException("voice stream envelope is not valid UTF-8", error)
    }

    private fun optionalToken(value: Map<String, Any?>, key: String): String? {
        val raw = value[key] ?: return null
        if (raw !is String) throw ZaraWireException("$key must be a string")
        return token(key, raw)
    }

    private fun token(name: String, value: String): String {
        val bytes = value.encodeToByteArray()
        if (value.isBlank() || bytes.size > maxTokenBytes || value.any { it.code !in 0x21..0x7e }) {
            throw ZaraWireException("$name is invalid")
        }
        return value
    }

    private fun rejectUnknown(value: Map<String, Any?>, allowed: Set<String>, name: String) {
        if ((value.keys - allowed).isNotEmpty()) throw ZaraWireException("$name contains unknown fields")
    }

    private fun optionalObject(value: Map<String, Any?>, key: String): Map<String, Any?> {
        val raw = value[key] ?: return emptyMap()
        @Suppress("UNCHECKED_CAST")
        return raw as? Map<String, Any?> ?: throw ZaraWireException("$key must be an object")
    }

    private fun requireString(value: Map<String, Any?>, key: String, maxBytes: Int): String {
        val raw = value[key] as? String ?: throw ZaraWireException("$key must be a string")
        if (raw.encodeToByteArray().size > maxBytes) throw ZaraWireException("$key exceeds byte limit")
        return raw
    }

    private fun optionalString(value: Map<String, Any?>, key: String, maxBytes: Int): String? {
        if (!value.containsKey(key)) return null
        return requireString(value, key, maxBytes)
    }

    private fun requireLong(
        value: Map<String, Any?>,
        key: String,
        minimum: Long? = null,
    ): Long {
        val raw = value[key] as? Long ?: throw ZaraWireException("$key must be integer")
        if (minimum != null && raw < minimum) throw ZaraWireException("$key is below minimum")
        return raw
    }

    private fun optionalLong(value: Map<String, Any?>, key: String): Long? {
        val raw = value[key] ?: return null
        return raw as? Long ?: throw ZaraWireException("$key must be integer")
    }
}

private class VoiceStreamJsonParser(private val source: String) {
    private var index = 0

    fun parseObject(): Map<String, Any?> {
        skipWhitespace()
        val value = parseValue()
        skipWhitespace()
        if (index != source.length) fail("trailing JSON data")
        @Suppress("UNCHECKED_CAST")
        return value as? Map<String, Any?> ?: fail("JSON value must be an object")
    }

    private fun parseValue(): Any? {
        skipWhitespace()
        if (index >= source.length) fail("unexpected end of JSON")
        return when (source[index]) {
            '{' -> parseObjectValue()
            '[' -> parseArray()
            '"' -> parseString()
            't' -> parseLiteral("true", true)
            'f' -> parseLiteral("false", false)
            'n' -> parseLiteral("null", null)
            '-', in '0'..'9' -> parseInteger()
            else -> fail("invalid JSON value")
        }
    }

    private fun parseObjectValue(): Map<String, Any?> {
        expect('{')
        skipWhitespace()
        if (consume('}')) return emptyMap()
        val result = linkedMapOf<String, Any?>()
        while (true) {
            skipWhitespace()
            if (index >= source.length || source[index] != '"') fail("object key must be string")
            val key = parseString()
            if (result.containsKey(key)) fail("duplicate JSON object key")
            skipWhitespace()
            expect(':')
            result[key] = parseValue()
            skipWhitespace()
            if (consume('}')) return result
            expect(',')
        }
    }

    private fun parseArray(): List<Any?> {
        expect('[')
        skipWhitespace()
        if (consume(']')) return emptyList()
        val result = mutableListOf<Any?>()
        while (true) {
            result += parseValue()
            skipWhitespace()
            if (consume(']')) return result
            expect(',')
        }
    }

    private fun parseString(): String {
        expect('"')
        val result = StringBuilder()
        while (index < source.length) {
            val character = source[index++]
            when (character) {
                '"' -> return result.toString()
                '\\' -> result.append(parseEscape())
                else -> {
                    if (character.code < 0x20) fail("control character in JSON string")
                    result.append(character)
                }
            }
        }
        fail("unterminated JSON string")
    }

    private fun parseEscape(): Char {
        if (index >= source.length) fail("unterminated JSON escape")
        return when (val escaped = source[index++]) {
            '"', '\\', '/' -> escaped
            'b' -> '\b'
            'f' -> '\u000c'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            'u' -> parseUnicodeEscape()
            else -> fail("invalid JSON escape")
        }
    }

    private fun parseUnicodeEscape(): Char {
        if (index + 4 > source.length) fail("short unicode escape")
        val hex = source.substring(index, index + 4)
        index += 4
        val code = hex.toIntOrNull(16) ?: fail("invalid unicode escape")
        return code.toChar()
    }

    private fun parseInteger(): Long {
        val start = index
        if (consume('-') && index >= source.length) fail("invalid number")
        if (consume('0')) {
            if (index < source.length && source[index].isDigit()) fail("leading zero in number")
        } else {
            if (index >= source.length || source[index] !in '1'..'9') fail("invalid number")
            while (index < source.length && source[index].isDigit()) index += 1
        }
        if (index < source.length && source[index] in charArrayOf('.', 'e', 'E')) {
            fail("non-integer JSON number is unsupported")
        }
        return source.substring(start, index).toLongOrNull() ?: fail("integer out of range")
    }

    private fun <T> parseLiteral(text: String, value: T): T {
        if (!source.startsWith(text, index)) fail("invalid JSON literal")
        index += text.length
        return value
    }

    private fun skipWhitespace() {
        while (index < source.length && source[index] in charArrayOf(' ', '\n', '\r', '\t')) index += 1
    }

    private fun expect(character: Char) {
        if (!consume(character)) fail("expected '$character'")
    }

    private fun consume(character: Char): Boolean {
        if (index < source.length && source[index] == character) {
            index += 1
            return true
        }
        return false
    }

    private fun fail(message: String): Nothing = throw ZaraWireException(message)
}
