package ai.zara.app.runtime

import ai.zara.app.device.DeviceActionArguments
import ai.zara.app.device.DeviceActionErrorCode
import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets

enum class DeviceActionIdempotency(val wireId: String) {
    AtMostOnce("at_most_once"),
    Idempotent("idempotent");

    companion object {
        fun fromWireId(value: String): DeviceActionIdempotency =
            entries.firstOrNull { it.wireId == value }
                ?: throw ZaraWireException("invalid device action idempotency")
    }
}

sealed interface DeviceServerMessage {
    val id: String
    val sessionId: String

    data class Request(
        override val id: String,
        override val sessionId: String,
        val traceId: String?,
        val actionId: String,
        val capability: DeviceCapability,
        val arguments: DeviceActionArguments,
        val deadlineNs: Long,
        val idempotency: DeviceActionIdempotency,
    ) : DeviceServerMessage

    data class Cancel(
        override val id: String,
        override val sessionId: String,
        val actionId: String,
        val reason: String?,
    ) : DeviceServerMessage
}

object ZaraDeviceActionCodec {
    private val marker = "ZARA/1".encodeToByteArray()
    private const val maxEnvelopeBytes = 64 * 1024
    private const val maxIdBytes = 128

    fun decodeServerMessage(frames: List<ByteArray>): DeviceServerMessage {
        if (frames.size != 2 || !frames[0].contentEquals(marker)) {
            throw ZaraWireException("invalid ZARA/1 device action frame")
        }
        if (frames[1].size > maxEnvelopeBytes) {
            throw ZaraWireException("device action envelope exceeds byte limit")
        }
        val envelope = DeviceJsonParser(decodeUtf8(frames[1])).parseObject()
        val type = requiredString(envelope, "type", 64)
        val allowedEnvelope = if (type == "device.action.request") {
            setOf("body", "id", "payload_count", "session_id", "timestamp_ns", "trace_id", "type")
        } else {
            setOf("body", "id", "payload_count", "session_id", "timestamp_ns", "type")
        }
        rejectUnknown(envelope, allowedEnvelope, "device action envelope")
        if (requiredLong(envelope, "payload_count") != 0L) {
            throw ZaraWireException("device action payload_count must be zero")
        }
        if (requiredLong(envelope, "timestamp_ns") < 0) {
            throw ZaraWireException("device action timestamp must be non-negative")
        }
        val id = token("id", requiredString(envelope, "id", maxIdBytes))
        val sessionId = token("session_id", requiredString(envelope, "session_id", maxIdBytes))
        val body = requiredObject(envelope, "body")
        return when (type) {
            "device.action.request" -> decodeRequest(envelope, body, id, sessionId)
            "device.action.cancel" -> decodeCancel(body, id, sessionId)
            else -> throw ZaraWireException("unsupported server device action type")
        }
    }

    fun encodeAccepted(
        requestId: String,
        sessionId: String,
        actionId: String,
        timestampNs: Long,
    ): List<ByteArray> = encodeTerminalLike(
        type = "device.action.accepted",
        requestId = requestId,
        sessionId = sessionId,
        timestampNs = timestampNs,
        body = "{\"action_id\":${jsonString(token("action_id", actionId))}}",
    )

    fun encodeCompleted(
        requestId: String,
        sessionId: String,
        actionId: String,
        timestampNs: Long,
    ): List<ByteArray> = encodeTerminalLike(
        type = "device.action.result",
        requestId = requestId,
        sessionId = sessionId,
        timestampNs = timestampNs,
        body = "{\"action_id\":${jsonString(token("action_id", actionId))},\"outcome\":\"completed\"}",
    )

    fun encodeError(
        requestId: String,
        sessionId: String,
        actionId: String,
        code: DeviceActionErrorCode,
        message: String?,
        timestampNs: Long,
    ): List<ByteArray> {
        val safeMessage = message?.let { boundedText("message", it, 256) }
        val body = buildString {
            append("{\"action_id\":")
            append(jsonString(token("action_id", actionId)))
            append(",\"code\":")
            append(jsonString(code.wireId))
            if (safeMessage != null) {
                append(",\"message\":")
                append(jsonString(safeMessage))
            }
            append('}')
        }
        return encodeTerminalLike(
            type = "device.action.error",
            requestId = requestId,
            sessionId = sessionId,
            timestampNs = timestampNs,
            body = body,
        )
    }

    private fun decodeRequest(
        envelope: Map<String, Any?>,
        body: Map<String, Any?>,
        id: String,
        sessionId: String,
    ): DeviceServerMessage.Request {
        requireExactKeys(
            body,
            setOf("action_id", "capability", "args", "deadline_ns", "idempotency"),
            "device action request body",
        )
        val actionId = token("action_id", requiredString(body, "action_id", maxIdBytes))
        val capability = try {
            DeviceCapability.fromWireId(requiredString(body, "capability", 64))
        } catch (error: IllegalArgumentException) {
            throw ZaraWireException("unknown device capability", error)
        }
        val args = decodeArguments(capability, requiredObject(body, "args"))
        val deadlineNs = requiredLong(body, "deadline_ns")
        if (deadlineNs <= 0) throw ZaraWireException("device action deadline must be positive")
        val idempotency = DeviceActionIdempotency.fromWireId(requiredString(body, "idempotency", 32))
        val traceId = optionalString(envelope, "trace_id")?.let { token("trace_id", it) }
        return DeviceServerMessage.Request(
            id = id,
            sessionId = sessionId,
            traceId = traceId,
            actionId = actionId,
            capability = capability,
            arguments = args,
            deadlineNs = deadlineNs,
            idempotency = idempotency,
        )
    }

    private fun decodeCancel(
        body: Map<String, Any?>,
        id: String,
        sessionId: String,
    ): DeviceServerMessage.Cancel {
        val validKeys = body.keys == setOf("action_id") || body.keys == setOf("action_id", "reason")
        if (!validKeys) throw ZaraWireException("device action cancel body has invalid fields")
        val reason = optionalString(body, "reason")?.let { boundedText("reason", it, 256) }
        return DeviceServerMessage.Cancel(
            id = id,
            sessionId = sessionId,
            actionId = token("action_id", requiredString(body, "action_id", maxIdBytes)),
            reason = reason,
        )
    }

    private fun decodeArguments(
        capability: DeviceCapability,
        args: Map<String, Any?>,
    ): DeviceActionArguments = when (capability) {
        DeviceCapability.OpenUri -> {
            requireExactKeys(args, setOf("uri"), "open_uri args")
            val uri = boundedText("uri", requiredString(args, "uri", 2_048), 2_048)
            if (uri.isEmpty()) throw ZaraWireException("uri must not be empty")
            DeviceActionArguments.OpenUri(uri)
        }
        DeviceCapability.OpenApp -> {
            requireExactKeys(args, setOf("app"), "open_app args")
            val app = boundedText("app", requiredString(args, "app", 128), 128)
            if (app.isEmpty()) throw ZaraWireException("app must not be empty")
            DeviceActionArguments.OpenApp(app)
        }
    }

    private fun encodeTerminalLike(
        type: String,
        requestId: String,
        sessionId: String,
        timestampNs: Long,
        body: String,
    ): List<ByteArray> {
        require(timestampNs >= 0) { "timestamp must be non-negative" }
        val envelope =
            "{\"body\":$body,\"id\":${jsonString(token("id", requestId))},\"payload_count\":0," +
                "\"session_id\":${jsonString(token("session_id", sessionId))}," +
                "\"timestamp_ns\":$timestampNs,\"type\":${jsonString(type)}}"
        return listOf(marker.copyOf(), envelope.encodeToByteArray())
    }

    private fun rejectUnknown(
        value: Map<String, Any?>,
        allowed: Set<String>,
        name: String,
    ) {
        if (value.keys - allowed != emptySet<String>()) {
            throw ZaraWireException("$name has invalid fields")
        }
    }

    private fun requireExactKeys(value: Map<String, Any?>, expected: Set<String>, name: String) {
        if (value.keys != expected) throw ZaraWireException("$name has invalid fields")
    }

    private fun requiredObject(value: Map<String, Any?>, key: String): Map<String, Any?> {
        @Suppress("UNCHECKED_CAST")
        return value[key] as? Map<String, Any?> ?: throw ZaraWireException("$key must be object")
    }

    private fun requiredString(value: Map<String, Any?>, key: String, maxBytes: Int): String {
        val text = value[key] as? String ?: throw ZaraWireException("$key must be string")
        if (text.encodeToByteArray().size > maxBytes) throw ZaraWireException("$key exceeds byte limit")
        return text
    }

    private fun optionalString(value: Map<String, Any?>, key: String): String? {
        val item = value[key] ?: return null
        return item as? String ?: throw ZaraWireException("$key must be string")
    }

    private fun requiredLong(value: Map<String, Any?>, key: String): Long =
        value[key] as? Long ?: throw ZaraWireException("$key must be integer")

    private fun token(name: String, value: String): String {
        val bytes = value.encodeToByteArray()
        if (value.isBlank() || bytes.size > maxIdBytes || value.any { it.code !in 0x21..0x7e }) {
            throw ZaraWireException("$name is invalid")
        }
        return value
    }

    private fun boundedText(name: String, value: String, maxBytes: Int): String {
        if (value.encodeToByteArray().size > maxBytes) throw ZaraWireException("$name exceeds byte limit")
        if (value.any { it.code < 0x20 || it.code == 0x7f }) {
            throw ZaraWireException("$name contains control characters")
        }
        return value
    }

    private fun jsonString(value: String): String = buildString {
        append('"')
        value.forEach { character ->
            when (character) {
                '"' -> append("\\\"")
                '\\' -> append("\\\\")
                '\b' -> append("\\b")
                '\u000C' -> append("\\f")
                '\n' -> append("\\n")
                '\r' -> append("\\r")
                '\t' -> append("\\t")
                else -> if (character.code < 0x20) {
                    append("\\u")
                    append(character.code.toString(16).padStart(4, '0'))
                } else {
                    append(character)
                }
            }
        }
        append('"')
    }

    private fun decodeUtf8(bytes: ByteArray): String = try {
        StandardCharsets.UTF_8.newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .decode(ByteBuffer.wrap(bytes))
            .toString()
    } catch (error: Exception) {
        throw ZaraWireException("device action envelope is not valid UTF-8", error)
    }
}

private class DeviceJsonParser(private val source: String) {
    private var index = 0
    private var containerDepth = 0

    fun parseObject(): Map<String, Any?> {
        skipWhitespace()
        val value = parseValue()
        skipWhitespace()
        if (index != source.length) fail("trailing JSON data")
        @Suppress("UNCHECKED_CAST")
        return value as? Map<String, Any?> ?: fail("JSON root must be object")
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
        return try {
            parse()
        } finally {
            containerDepth -= 1
        }
    }

    private fun parseObjectValue(): Map<String, Any?> {
        expect('{')
        skipWhitespace()
        val result = linkedMapOf<String, Any?>()
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
        skipWhitespace()
        val result = mutableListOf<Any?>()
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
            'f' -> '\u000C'
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            'u' -> parseUnicodeEscape()
            else -> fail("invalid JSON escape")
        }
    }

    private fun parseUnicodeEscape(): Char {
        if (index + 4 > source.length) fail("truncated unicode escape")
        val digits = source.substring(index, index + 4)
        if (!digits.all { it.isDigit() || it.lowercaseChar() in 'a'..'f' }) {
            fail("invalid unicode escape")
        }
        index += 4
        return digits.toInt(16).toChar()
    }

    private fun parseInteger(): Long {
        val start = index
        if (source[index] == '-') index++
        if (index >= source.length) fail("invalid integer")
        if (source[index] == '0') {
            index++
            if (index < source.length && source[index].isDigit()) fail("leading zero")
        } else {
            if (source[index] !in '1'..'9') fail("invalid integer")
            while (index < source.length && source[index].isDigit()) index++
        }
        if (index < source.length && source[index] in listOf('.', 'e', 'E')) fail("non-integer number")
        return source.substring(start, index).toLongOrNull() ?: fail("integer out of range")
    }

    private fun <T> parseLiteral(text: String, value: T): T {
        if (!source.startsWith(text, index)) fail("invalid JSON literal")
        index += text.length
        return value
    }

    private fun skipWhitespace() {
        while (index < source.length && source[index] in listOf(' ', '\n', '\r', '\t')) index++
    }

    private fun expect(character: Char) {
        if (!consume(character)) fail("expected '$character'")
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
