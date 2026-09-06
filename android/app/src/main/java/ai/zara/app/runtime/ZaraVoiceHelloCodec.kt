package ai.zara.app.runtime

import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets

data class AudioOutputFormat(
    val codec: String,
    val sampleRate: Int,
    val channels: Int,
) {
    init {
        require(codec == "pcm_s16le") { "unsupported audio output codec" }
        require(sampleRate > 0) { "audio output sample rate must be positive" }
        require(channels == 1) { "Android audio output must be mono" }
    }

    companion object {
        fun pcmS16leMono(sampleRate: Int): AudioOutputFormat = AudioOutputFormat("pcm_s16le", sampleRate, 1)
    }
}

data class VoiceHelloOk(val replyTo: String, val sessionId: String, val audioOutputFormat: AudioOutputFormat)

object ZaraVoiceHelloCodec {
    private val marker = "ZARA/1".encodeToByteArray()
    private const val maxEnvelopeBytes = 64 * 1024
    private const val maxTokenBytes = 128
    private val envelopeKeys = setOf(
        "type", "id", "reply_to", "session_id", "conversation_id", "turn_id", "stream_id", "seq",
        "timestamp_ns", "trace_id", "content_type", "payload_count", "flags", "body",
    )

    fun encodeHello(requestId: String, timestampNs: Long, audioOutputFormats: List<AudioOutputFormat>): List<ByteArray> {
        val id = token("id", requestId)
        require(timestampNs >= 0) { "timestamp must be non-negative" }
        require(audioOutputFormats.isNotEmpty()) { "voice hello requires an audio output offer" }
        require(audioOutputFormats.size <= 8) { "audio output offer exceeds format limit" }
        require(audioOutputFormats.distinct() == audioOutputFormats) { "audio output offer contains duplicates" }
        val formats = audioOutputFormats.joinToString(separator = ",", prefix = "[", postfix = "]") { format ->
            "{\"channels\":${format.channels},\"codec\":\"pcm_s16le\",\"sample_rate\":${format.sampleRate}}"
        }
        val envelope = "{\"body\":{\"audio_output_formats\":$formats,\"versions\":[1]}," +
            "\"id\":\"$id\",\"payload_count\":0,\"timestamp_ns\":$timestampNs,\"type\":\"hello\"}"
        return listOf(marker.copyOf(), envelope.encodeToByteArray())
    }

    fun decodeHelloOk(frames: List<ByteArray>): VoiceHelloOk {
        if (frames.size != 2 || !frames[0].contentEquals(marker)) throw ZaraWireException("invalid ZARA/1 voice hello frame")
        if (frames[1].size > maxEnvelopeBytes) throw ZaraWireException("voice hello exceeds byte limit")
        val envelope = VoiceHelloJsonParser(decodeUtf8(frames[1])).parseObject()
        rejectUnknown(envelope, envelopeKeys, "voice hello envelope")
        if (requireString(envelope, "type", 64) != "hello.ok") throw ZaraWireException("expected hello.ok")
        token("id", requireString(envelope, "id", maxTokenBytes))
        requireLong(envelope, "payload_count", exact = 0)
        requireLong(envelope, "timestamp_ns", minimum = 0)
        val replyTo = token("reply_to", requireString(envelope, "reply_to", maxTokenBytes))
        val sessionId = token("session_id", requireString(envelope, "session_id", maxTokenBytes))
        rejectAbsent(envelope, "conversation_id", "turn_id", "stream_id", "seq", "trace_id", "content_type", "flags")
        val body = requireObject(envelope, "body")
        rejectUnknown(body, setOf("version", "max_payload_frames", "max_payload_frame_bytes", "max_payload_bytes", "audio_output_format"), "hello.ok body")
        requireLong(body, "version", exact = 1)
        positiveInt(body, "max_payload_frames")
        positiveInt(body, "max_payload_frame_bytes")
        positiveInt(body, "max_payload_bytes")
        val selected = requireObject(body, "audio_output_format")
        rejectUnknown(selected, setOf("codec", "sample_rate", "channels"), "audio output format")
        val codec = requireString(selected, "codec", 32)
        if (codec != "pcm_s16le") throw ZaraWireException("unsupported audio output codec")
        val sampleRate = positiveInt(selected, "sample_rate")
        val channels = positiveInt(selected, "channels")
        if (channels != 1) throw ZaraWireException("Android audio output requires mono")
        return VoiceHelloOk(replyTo, sessionId, AudioOutputFormat(codec, sampleRate, channels))
    }

    private fun decodeUtf8(bytes: ByteArray): String = try {
        StandardCharsets.UTF_8.newDecoder().onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT).decode(ByteBuffer.wrap(bytes)).toString()
    } catch (error: Exception) {
        throw ZaraWireException("voice hello is not valid UTF-8", error)
    }

    private fun token(name: String, value: String): String {
        val bytes = value.encodeToByteArray()
        if (value.isBlank() || bytes.size > maxTokenBytes || value.any { it.code !in 0x21..0x7e }) throw ZaraWireException("$name is invalid")
        if ('"' in value || '\\' in value) throw ZaraWireException("$name is not JSON-safe")
        return value
    }

    private fun rejectUnknown(value: Map<String, Any?>, allowed: Set<String>, name: String) {
        if ((value.keys - allowed).isNotEmpty()) throw ZaraWireException("$name contains unknown fields")
    }

    private fun rejectAbsent(value: Map<String, Any?>, vararg keys: String) {
        keys.forEach { key -> if (value.containsKey(key)) throw ZaraWireException("hello.ok contains unexpected $key") }
    }

    private fun requireObject(value: Map<String, Any?>, key: String): Map<String, Any?> {
        @Suppress("UNCHECKED_CAST")
        return value[key] as? Map<String, Any?> ?: throw ZaraWireException("$key must be an object")
    }

    private fun requireString(value: Map<String, Any?>, key: String, maxBytes: Int): String {
        val raw = value[key] as? String ?: throw ZaraWireException("$key must be a string")
        if (raw.encodeToByteArray().size > maxBytes) throw ZaraWireException("$key exceeds byte limit")
        return raw
    }

    private fun positiveInt(value: Map<String, Any?>, key: String): Int {
        val raw = requireLong(value, key, minimum = 1)
        if (raw > Int.MAX_VALUE) throw ZaraWireException("$key is too large")
        return raw.toInt()
    }

    private fun requireLong(value: Map<String, Any?>, key: String, minimum: Long? = null, exact: Long? = null): Long {
        val raw = value[key] as? Long ?: throw ZaraWireException("$key must be integer")
        if (minimum != null && raw < minimum) throw ZaraWireException("$key is below minimum")
        if (exact != null && raw != exact) throw ZaraWireException("$key has unsupported value")
        return raw
    }
}

private class VoiceHelloJsonParser(private val source: String) {
    private var index = 0
    private var containerDepth = 0

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
            '{' -> inContainer { parseObjectValue() }
            '[' -> inContainer { parseArray() }
            '"' -> parseString()
            't' -> parseLiteral("true", true)
            'f' -> parseLiteral("false", false)
            'n' -> parseLiteral("null", null)
            '-', in '0'..'9' -> parseInteger()
            else -> fail("invalid JSON value")
        }
    }

    private fun <T> inContainer(parse: () -> T): T {
        if (containerDepth >= 64) fail("JSON nesting depth exceeds limit")
        containerDepth += 1
        return try { parse() } finally { containerDepth -= 1 }
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
            skipWhitespace(); expect(':'); result[key] = parseValue(); skipWhitespace()
            if (consume('}')) return result
            expect(',')
        }
    }

    private fun parseArray(): List<Any?> {
        expect('['); skipWhitespace()
        if (consume(']')) return emptyList()
        val result = mutableListOf<Any?>()
        while (true) {
            result += parseValue(); skipWhitespace()
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
                else -> { if (character.code < 0x20) fail("control character in JSON string"); result.append(character) }
            }
        }
        fail("unterminated JSON string")
    }

    private fun parseEscape(): Char {
        if (index >= source.length) fail("unterminated JSON escape")
        return when (val escaped = source[index++]) {
            '"', '\\', '/' -> escaped; 'b' -> '\b'; 'f' -> '\u000c'; 'n' -> '\n'; 'r' -> '\r'; 't' -> '\t'; 'u' -> parseUnicodeEscape(); else -> fail("invalid JSON escape")
        }
    }

    private fun parseUnicodeEscape(): Char {
        if (index + 4 > source.length) fail("short unicode escape")
        val hex = source.substring(index, index + 4); index += 4
        return (hex.toIntOrNull(16) ?: fail("invalid unicode escape")).toChar()
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
        if (index < source.length && source[index] in charArrayOf('.', 'e', 'E')) fail("non-integer JSON number is unsupported")
        return source.substring(start, index).toLongOrNull() ?: fail("integer out of range")
    }

    private fun <T> parseLiteral(text: String, value: T): T { if (!source.startsWith(text, index)) fail("invalid JSON literal"); index += text.length; return value }
    private fun skipWhitespace() { while (index < source.length && source[index] in charArrayOf(' ', '\n', '\r', '\t')) index += 1 }
    private fun expect(character: Char) { if (!consume(character)) fail("expected '$character'") }
    private fun consume(character: Char): Boolean { if (index < source.length && source[index] == character) { index += 1; return true }; return false }
    private fun fail(message: String): Nothing = throw ZaraWireException(message)
}
