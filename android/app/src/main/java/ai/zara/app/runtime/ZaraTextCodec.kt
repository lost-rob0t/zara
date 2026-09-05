package ai.zara.app.runtime

import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets

class ZaraWireException(message: String, cause: Throwable? = null) : Exception(message, cause)

sealed interface TextServerMessage {
    val id: String
    val sessionId: String?

    data class HelloOk(
        override val id: String,
        val replyTo: String,
        override val sessionId: String,
        val version: Int,
        val maxPayloadFrames: Int,
        val maxPayloadFrameBytes: Int,
        val maxPayloadBytes: Int,
    ) : TextServerMessage

    data class TurnAccepted(
        override val id: String,
        val replyTo: String,
        override val sessionId: String,
        val conversationId: String?,
        val turnId: String,
    ) : TextServerMessage

    data class Progress(
        override val id: String,
        override val sessionId: String,
        val conversationId: String?,
        val turnId: String,
        val sequence: Long,
        val type: String,
    ) : TextServerMessage

    data class AssistantDelta(
        override val id: String,
        override val sessionId: String,
        val conversationId: String?,
        val turnId: String,
        val sequence: Long,
        val text: String,
    ) : TextServerMessage

    data class AssistantCompleted(
        override val id: String,
        override val sessionId: String,
        val conversationId: String?,
        val turnId: String,
        val sequence: Long,
        val text: String,
        val success: Boolean,
    ) : TextServerMessage

    data class TurnCompleted(
        override val id: String,
        override val sessionId: String,
        val conversationId: String?,
        val turnId: String,
        val sequence: Long,
        val success: Boolean,
    ) : TextServerMessage

    data class AssistantResponse(
        override val id: String,
        override val sessionId: String,
        val conversationId: String?,
        val turnId: String?,
        val sequence: Long?,
        val text: String,
        val truncated: Boolean,
    ) : TextServerMessage

    data class ProtocolError(
        override val id: String,
        val replyTo: String?,
        override val sessionId: String?,
        val code: String,
        val message: String,
        val retryable: Boolean,
    ) : TextServerMessage
}

object ZaraTextCodec {
    private val marker = "ZARA/1".encodeToByteArray()
    private const val maxEnvelopeBytes = 64 * 1024
    private const val maxIdBytes = 128
    private const val maxTextBytes = 1024 * 1024
    private val envelopeKeys = setOf(
        "type", "id", "reply_to", "session_id", "conversation_id", "turn_id",
        "stream_id", "seq", "timestamp_ns", "trace_id", "content_type",
        "payload_count", "flags", "body",
    )

    fun encodeHello(requestId: String, timestampNs: Long): List<ByteArray> {
        val id = token("id", requestId)
        requireTimestamp(timestampNs)
        return frames(
            linkedMapOf(
                "body" to linkedMapOf("versions" to listOf(1L)),
                "id" to id,
                "payload_count" to 0L,
                "timestamp_ns" to timestampNs,
                "type" to "hello",
            )
        )
    }

    fun encodeTurnSubmit(
        requestId: String,
        sessionId: String,
        conversationId: String?,
        text: String,
        timestampNs: Long,
    ): List<ByteArray> {
        val id = token("id", requestId)
        val session = token("session_id", sessionId)
        val conversation = conversationId?.let { token("conversation_id", it) }
        requireTimestamp(timestampNs)
        require(text.isNotBlank()) { "turn text is required" }
        require(text.encodeToByteArray().size <= maxTextBytes) { "turn text exceeds byte limit" }
        val envelope = linkedMapOf<String, Any?>("body" to linkedMapOf("text" to text))
        if (conversation != null) envelope["conversation_id"] = conversation
        envelope["id"] = id
        envelope["payload_count"] = 0L
        envelope["session_id"] = session
        envelope["timestamp_ns"] = timestampNs
        envelope["type"] = "turn.submit"
        return frames(envelope)
    }

    fun decode(frames: List<ByteArray>): TextServerMessage {
        if (frames.size != 2 || !frames[0].contentEquals(marker)) {
            throw ZaraWireException("invalid ZARA/1 text frame")
        }
        if (frames[1].size > maxEnvelopeBytes) throw ZaraWireException("envelope exceeds byte limit")
        val parsed = StrictJsonParser(decodeUtf8(frames[1])).parseObject()
        rejectUnknown(parsed, envelopeKeys, "envelope")
        requireLong(parsed, "payload_count", exact = 0)
        requireLong(parsed, "timestamp_ns", minimum = 0)
        val type = requireString(parsed, "type", 64)
        val id = wireToken("id", requireString(parsed, "id", maxIdBytes))
        val sessionId = optionalToken(parsed, "session_id")
        val replyTo = optionalToken(parsed, "reply_to")
        val conversationId = optionalToken(parsed, "conversation_id")
        val turnId = optionalToken(parsed, "turn_id")
        val sequence = optionalLong(parsed, "seq")
        val body = optionalObject(parsed, "body")

        return when (type) {
            "hello.ok" -> {
                rejectUnknown(
                    body,
                    setOf(
                        "version", "max_payload_frames", "max_payload_frame_bytes",
                        "max_payload_bytes", "audio_output_format",
                    ),
                    "hello.ok body",
                )
                TextServerMessage.HelloOk(
                    id = id,
                    replyTo = wireRequired(replyTo, "hello.ok requires reply_to"),
                    sessionId = wireRequired(sessionId, "hello.ok requires session_id"),
                    version = requireLong(body, "version", exact = 1).toInt(),
                    maxPayloadFrames = requirePositiveInt(body, "max_payload_frames"),
                    maxPayloadFrameBytes = requirePositiveInt(body, "max_payload_frame_bytes"),
                    maxPayloadBytes = requirePositiveInt(body, "max_payload_bytes"),
                )
            }
            "turn.accepted" -> {
                rejectUnknown(body, emptySet(), "turn.accepted body")
                TextServerMessage.TurnAccepted(
                    id = id,
                    replyTo = wireRequired(replyTo, "turn.accepted requires reply_to"),
                    sessionId = wireRequired(sessionId, "turn.accepted requires session_id"),
                    conversationId = conversationId,
                    turnId = wireRequired(turnId, "turn.accepted requires turn_id"),
                )
            }
            "turn.started", "assistant.started" -> {
                rejectUnknown(body, emptySet(), "$type body")
                TextServerMessage.Progress(
                    id = id,
                    sessionId = wireRequired(sessionId, "$type requires session_id"),
                    conversationId = conversationId,
                    turnId = wireRequired(turnId, "$type requires turn_id"),
                    sequence = wireRequired(sequence, "$type requires seq"),
                    type = type,
                )
            }
            "assistant.delta" -> {
                rejectUnknown(body, setOf("text"), "assistant.delta body")
                TextServerMessage.AssistantDelta(
                    id = id,
                    sessionId = wireRequired(sessionId, "assistant.delta requires session_id"),
                    conversationId = conversationId,
                    turnId = wireRequired(turnId, "assistant.delta requires turn_id"),
                    sequence = wireRequired(sequence, "assistant.delta requires seq"),
                    text = requireBoundedText(body, "text"),
                )
            }
            "assistant.completed" -> {
                rejectUnknown(body, setOf("text", "success"), "assistant.completed body")
                TextServerMessage.AssistantCompleted(
                    id = id,
                    sessionId = wireRequired(sessionId, "assistant.completed requires session_id"),
                    conversationId = conversationId,
                    turnId = wireRequired(turnId, "assistant.completed requires turn_id"),
                    sequence = wireRequired(sequence, "assistant.completed requires seq"),
                    text = requireBoundedText(body, "text"),
                    success = requireBoolean(body, "success"),
                )
            }
            "turn.completed" -> {
                rejectUnknown(body, setOf("success"), "turn.completed body")
                TextServerMessage.TurnCompleted(
                    id = id,
                    sessionId = wireRequired(sessionId, "turn.completed requires session_id"),
                    conversationId = conversationId,
                    turnId = wireRequired(turnId, "turn.completed requires turn_id"),
                    sequence = wireRequired(sequence, "turn.completed requires seq"),
                    success = requireBoolean(body, "success"),
                )
            }
            "assistant.response" -> {
                rejectUnknown(body, setOf("text", "truncated"), "assistant.response body")
                TextServerMessage.AssistantResponse(
                    id = id,
                    sessionId = wireRequired(sessionId, "assistant.response requires session_id"),
                    conversationId = conversationId,
                    turnId = turnId,
                    sequence = sequence,
                    text = requireBoundedText(body, "text"),
                    truncated = requireBoolean(body, "truncated"),
                )
            }
            "protocol.error" -> {
                rejectUnknown(body, setOf("code", "message", "retryable"), "protocol.error body")
                TextServerMessage.ProtocolError(
                    id = id,
                    replyTo = replyTo,
                    sessionId = sessionId,
                    code = requireString(body, "code", 128),
                    message = requireString(body, "message", 1024),
                    retryable = requireBoolean(body, "retryable"),
                )
            }
            else -> throw ZaraWireException("unsupported server text message type")
        }
    }

    private fun frames(envelope: Map<String, Any?>): List<ByteArray> =
        listOf(marker.copyOf(), encodeJson(envelope).encodeToByteArray())

    private fun encodeJson(value: Any?): String = when (value) {
        null -> "null"
        is String -> buildString {
            append('"')
            for (character in value) {
                when (character) {
                    '"' -> append("\\\"")
                    '\\' -> append("\\\\")
                    '\b' -> append("\\b")
                    '\u000C' -> append("\\f")
                    '\n' -> append("\\n")
                    '\r' -> append("\\r")
                    '\t' -> append("\\t")
                    else -> if (character.code < 0x20) {
                        append("\\u%04x".format(character.code))
                    } else {
                        append(character)
                    }
                }
            }
            append('"')
        }
        is Boolean -> value.toString()
        is Byte, is Short, is Int, is Long -> value.toString()
        is List<*> -> value.joinToString(prefix = "[", postfix = "]") { encodeJson(it) }
        is Map<*, *> -> value.entries
            .map { (key, item) ->
                (key as? String ?: throw ZaraWireException("JSON object key must be string")) to item
            }
            .sortedBy { it.first }
            .joinToString(prefix = "{", postfix = "}") { (key, item) ->
                "${encodeJson(key)}:${encodeJson(item)}"
            }
        else -> throw ZaraWireException("unsupported JSON value")
    }

    private fun decodeUtf8(bytes: ByteArray): String = try {
        StandardCharsets.UTF_8.newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .decode(ByteBuffer.wrap(bytes))
            .toString()
    } catch (error: Exception) {
        throw ZaraWireException("envelope is not valid UTF-8", error)
    }

    private fun token(name: String, value: String): String {
        val bytes = value.encodeToByteArray()
        require(value.isNotBlank() && bytes.size <= maxIdBytes) { "$name is invalid" }
        require(value.all { it.code in 0x21..0x7e }) { "$name must be printable ASCII" }
        return value
    }

    private fun wireToken(name: String, value: String): String = try {
        token(name, value)
    } catch (error: IllegalArgumentException) {
        throw ZaraWireException("$name is invalid", error)
    }

    private fun optionalToken(objectValue: Map<String, Any?>, key: String): String? {
        val value = objectValue[key] ?: return null
        if (value !is String) throw ZaraWireException("$key must be a string")
        return wireToken(key, value)
    }

    private fun requireTimestamp(value: Long) {
        require(value >= 0) { "timestamp must be non-negative" }
    }

    private fun rejectUnknown(objectValue: Map<String, Any?>, allowed: Set<String>, name: String) {
        if ((objectValue.keys - allowed).isNotEmpty()) {
            throw ZaraWireException("$name contains unknown fields")
        }
    }

    private fun optionalObject(objectValue: Map<String, Any?>, key: String): Map<String, Any?> {
        val value = objectValue[key] ?: return emptyMap()
        @Suppress("UNCHECKED_CAST")
        return value as? Map<String, Any?> ?: throw ZaraWireException("$key must be an object")
    }

    private fun requireString(objectValue: Map<String, Any?>, key: String, maximumBytes: Int): String {
        val value = objectValue[key] as? String ?: throw ZaraWireException("$key must be a string")
        if (value.encodeToByteArray().size > maximumBytes) throw ZaraWireException("$key exceeds byte limit")
        return value
    }

    private fun requireBoundedText(objectValue: Map<String, Any?>, key: String): String =
        requireString(objectValue, key, maxTextBytes)

    private fun requireBoolean(objectValue: Map<String, Any?>, key: String): Boolean =
        objectValue[key] as? Boolean ?: throw ZaraWireException("$key must be boolean")

    private fun requirePositiveInt(objectValue: Map<String, Any?>, key: String): Int {
        val value = requireLong(objectValue, key, minimum = 1)
        if (value > Int.MAX_VALUE) throw ZaraWireException("$key is too large")
        return value.toInt()
    }

    private fun requireLong(
        objectValue: Map<String, Any?>,
        key: String,
        minimum: Long? = null,
        exact: Long? = null,
    ): Long {
        val value = objectValue[key] as? Long ?: throw ZaraWireException("$key must be integer")
        if (minimum != null && value < minimum) throw ZaraWireException("$key is below minimum")
        if (exact != null && value != exact) throw ZaraWireException("$key has unsupported value")
        return value
    }

    private fun optionalLong(objectValue: Map<String, Any?>, key: String): Long? {
        val value = objectValue[key] ?: return null
        return value as? Long ?: throw ZaraWireException("$key must be integer")
    }

    private fun <T : Any> wireRequired(value: T?, message: String): T =
        value ?: throw ZaraWireException(message)
}

private class StrictJsonParser(private val source: String) {
    private var index = 0

    fun parseObject(): Map<String, Any?> {
        skipWhitespace()
        val value = parseValue()
        skipWhitespace()
        if (index != source.length) fail("trailing JSON data")
        @Suppress("UNCHECKED_CAST")
        return value as? Map<String, Any?> ?: fail("envelope must be object")
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
            else -> fail("invalid JSON token")
        }
    }

    private fun parseObjectValue(): Map<String, Any?> {
        expect('{')
        val result = LinkedHashMap<String, Any?>()
        skipWhitespace()
        if (consume('}')) return result
        while (true) {
            skipWhitespace()
            if (index >= source.length || source[index] != '"') fail("object key must be string")
            val key = parseString()
            if (result.containsKey(key)) fail("duplicate JSON key")
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
        val result = ArrayList<Any?>()
        skipWhitespace()
        if (consume(']')) return result
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
                '\\' -> {
                    if (index >= source.length) fail("unterminated escape")
                    when (val escaped = source[index++]) {
                        '"', '\\', '/' -> result.append(escaped)
                        'b' -> result.append('\b')
                        'f' -> result.append('\u000C')
                        'n' -> result.append('\n')
                        'r' -> result.append('\r')
                        't' -> result.append('\t')
                        'u' -> result.append(parseUnicodeEscape())
                        else -> fail("invalid JSON escape")
                    }
                }
                else -> {
                    if (character.code < 0x20) fail("control character in string")
                    result.append(character)
                }
            }
        }
        fail("unterminated string")
    }

    private fun parseUnicodeEscape(): Char {
        if (index + 4 > source.length) fail("truncated unicode escape")
        val text = source.substring(index, index + 4)
        val value = text.toIntOrNull(16) ?: fail("invalid unicode escape")
        index += 4
        return value.toChar()
    }

    private fun parseInteger(): Long {
        val start = index
        if (source[index] == '-') index++
        if (index >= source.length) fail("invalid number")
        if (source[index] == '0') {
            index++
            if (index < source.length && source[index].isDigit()) fail("leading zero")
        } else {
            if (source[index] !in '1'..'9') fail("invalid number")
            while (index < source.length && source[index].isDigit()) index++
        }
        if (index < source.length && source[index] in listOf('.', 'e', 'E')) fail("non-integer number")
        return source.substring(start, index).toLongOrNull() ?: fail("integer out of range")
    }

    private fun <T> parseLiteral(text: String, value: T): T {
        if (!source.startsWith(text, index)) fail("invalid literal")
        index += text.length
        return value
    }

    private fun skipWhitespace() {
        while (index < source.length && source[index] in listOf(' ', '\t', '\r', '\n')) index++
    }

    private fun expect(character: Char) {
        if (!consume(character)) fail("expected $character")
    }

    private fun consume(character: Char): Boolean {
        if (index < source.length && source[index] == character) {
            index++
            return true
        }
        return false
    }

    private fun fail(message: String): Nothing = throw ZaraWireException(message)
}
