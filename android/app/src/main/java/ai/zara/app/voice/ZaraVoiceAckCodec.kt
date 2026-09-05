package ai.zara.app.voice

import ai.zara.app.runtime.ZaraWireException
import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets

sealed interface VoiceServerReply {
    data class Acknowledged(
        val type: String,
        val replyTo: String,
        val sessionId: String,
        val conversationId: String?,
        val streamId: String,
        val sequence: Long?,
    ) : VoiceServerReply

    data class ProtocolError(
        val replyTo: String?,
        val sessionId: String?,
        val code: String,
        val message: String,
        val retryable: Boolean,
    ) : VoiceServerReply
}

object ZaraVoiceAckCodec {
    private val marker = "ZARA/1".encodeToByteArray()
    private const val maxEnvelopeBytes = 64 * 1024
    private const val maxTokenBytes = 128
    private val envelopeKeys = setOf(
        "type", "id", "reply_to", "session_id", "conversation_id", "stream_id",
        "seq", "timestamp_ns", "payload_count", "body",
    )

    fun decode(frames: List<ByteArray>): VoiceServerReply {
        if (frames.size != 2 || !frames[0].contentEquals(marker)) {
            throw ZaraWireException("invalid ZARA/1 voice acknowledgement frame")
        }
        if (frames[1].size > maxEnvelopeBytes) {
            throw ZaraWireException("voice acknowledgement envelope exceeds byte limit")
        }
        val envelope = VoiceJsonParser(decodeUtf8(frames[1])).parseObject()
        rejectUnknown(envelope, envelopeKeys, "voice acknowledgement")
        requireLong(envelope, "payload_count", exact = 0)
        requireLong(envelope, "timestamp_ns", minimum = 0)
        token("id", requireString(envelope, "id", maxTokenBytes))
        val type = requireString(envelope, "type", 64)
        val replyTo = optionalToken(envelope, "reply_to")
        val sessionId = optionalToken(envelope, "session_id")
        val conversationId = optionalToken(envelope, "conversation_id")
        val streamId = optionalToken(envelope, "stream_id")
        val sequence = optionalLong(envelope, "seq")
        val body = optionalObject(envelope, "body")

        if (type == "protocol.error") {
            rejectUnknown(body, setOf("code", "message", "retryable"), "protocol.error body")
            return VoiceServerReply.ProtocolError(
                replyTo = replyTo,
                sessionId = sessionId,
                code = requireString(body, "code", 128),
                message = requireString(body, "message", 1024),
                retryable = requireBoolean(body, "retryable"),
            )
        }

        if (type !in setOf(
                "audio.input.started",
                "audio.input.accepted",
                "audio.input.committed",
                "audio.input.cancelled",
            )
        ) {
            throw ZaraWireException("unsupported voice acknowledgement type")
        }
        rejectUnknown(body, emptySet(), "$type body")
        val requiredReply = replyTo ?: throw ZaraWireException("$type requires reply_to")
        val requiredSession = sessionId ?: throw ZaraWireException("$type requires session_id")
        val requiredStream = streamId ?: throw ZaraWireException("$type requires stream_id")
        if (type == "audio.input.accepted") {
            if (sequence == null || sequence < 0) {
                throw ZaraWireException("audio.input.accepted requires non-negative seq")
            }
        } else if (sequence != null) {
            throw ZaraWireException("$type must not carry seq")
        }
        return VoiceServerReply.Acknowledged(
            type = type,
            replyTo = requiredReply,
            sessionId = requiredSession,
            conversationId = conversationId,
            streamId = requiredStream,
            sequence = sequence,
        )
    }

    private fun decodeUtf8(bytes: ByteArray): String = try {
        StandardCharsets.UTF_8.newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .decode(ByteBuffer.wrap(bytes))
            .toString()
    } catch (error: Exception) {
        throw ZaraWireException("voice acknowledgement is not valid UTF-8", error)
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

    private fun requireBoolean(value: Map<String, Any?>, key: String): Boolean =
        value[key] as? Boolean ?: throw ZaraWireException("$key must be boolean")

    private fun requireLong(
        value: Map<String, Any?>,
        key: String,
        minimum: Long? = null,
        exact: Long? = null,
    ): Long {
        val raw = value[key] as? Long ?: throw ZaraWireException("$key must be integer")
        if (minimum != null && raw < minimum) throw ZaraWireException("$key is below minimum")
        if (exact != null && raw != exact) throw ZaraWireException("$key has unsupported value")
        return raw
    }

    private fun optionalLong(value: Map<String, Any?>, key: String): Long? {
        val raw = value[key] ?: return null
        return raw as? Long ?: throw ZaraWireException("$key must be integer")
    }
}

private class VoiceJsonParser(private val source: String) {
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
