package ai.zara.app.runtime

import java.nio.ByteBuffer
import java.nio.charset.CodingErrorAction
import java.nio.charset.StandardCharsets

enum class DeviceCapability(val wireId: String) {
    OpenApp("open_app"),
    OpenUri("open_uri");

    companion object {
        fun fromWireId(value: String): DeviceCapability =
            entries.firstOrNull { it.wireId == value }
                ?: throw ZaraWireException("unknown device capability")
    }
}

data class CapabilitySnapshotOk(
    val id: String,
    val replyTo: String,
    val sessionId: String,
    val capabilities: Set<DeviceCapability>,
)

object ZaraCapabilityCodec {
    private val marker = "ZARA/1".encodeToByteArray()
    private const val maxEnvelopeBytes = 64 * 1024
    private const val maxIdBytes = 128

    fun encodeSnapshot(
        requestId: String,
        sessionId: String,
        capabilities: Set<DeviceCapability>,
        timestampNs: Long,
    ): List<ByteArray> {
        val id = token("id", requestId)
        val session = token("session_id", sessionId)
        require(timestampNs >= 0) { "timestamp must be non-negative" }
        val entries = capabilities
            .sortedBy(DeviceCapability::wireId)
            .joinToString(",") { capability ->
                "{\"id\":\"${capability.wireId}\",\"version\":1}"
            }
        val envelope =
            "{\"body\":{\"capabilities\":[$entries]},\"id\":\"$id\",\"payload_count\":0," +
                "\"session_id\":\"$session\",\"timestamp_ns\":$timestampNs,\"type\":\"capability.snapshot\"}"
        return listOf(marker.copyOf(), envelope.encodeToByteArray())
    }

    fun decodeSnapshotOk(frames: List<ByteArray>): CapabilitySnapshotOk {
        if (frames.size != 2 || !frames[0].contentEquals(marker)) {
            throw ZaraWireException("invalid ZARA/1 capability frame")
        }
        if (frames[1].size > maxEnvelopeBytes) throw ZaraWireException("envelope exceeds byte limit")
        val envelope = CapabilityJsonParser(decodeUtf8(frames[1])).parseObject()
        requireExactKeys(
            envelope,
            setOf("body", "id", "payload_count", "reply_to", "session_id", "timestamp_ns", "type"),
            "capability envelope",
        )
        if (requiredLong(envelope, "payload_count") != 0L) {
            throw ZaraWireException("capability snapshot payload_count must be zero")
        }
        if (requiredLong(envelope, "timestamp_ns") < 0) {
            throw ZaraWireException("capability timestamp must be non-negative")
        }
        if (requiredString(envelope, "type") != "capability.snapshot.ok") {
            throw ZaraWireException("unexpected capability message type")
        }
        val body = requiredObject(envelope, "body")
        requireExactKeys(body, setOf("capabilities"), "capability snapshot body")
        val capabilities = requiredArray(body, "capabilities")
        if (capabilities.size > DeviceCapability.entries.size) {
            throw ZaraWireException("capability snapshot contains too many entries")
        }
        val decoded = LinkedHashSet<DeviceCapability>()
        capabilities.forEach { value ->
            val entry = value as? Map<*, *> ?: throw ZaraWireException("capability entry must be object")
            @Suppress("UNCHECKED_CAST")
            val typed = entry as Map<String, Any?>
            requireExactKeys(typed, setOf("id", "version"), "capability entry")
            if (requiredLong(typed, "version") != 1L) {
                throw ZaraWireException("unsupported capability version")
            }
            val capability = DeviceCapability.fromWireId(requiredString(typed, "id"))
            if (!decoded.add(capability)) throw ZaraWireException("duplicate device capability")
        }
        return CapabilitySnapshotOk(
            id = wireToken("id", requiredString(envelope, "id")),
            replyTo = wireToken("reply_to", requiredString(envelope, "reply_to")),
            sessionId = wireToken("session_id", requiredString(envelope, "session_id")),
            capabilities = decoded,
        )
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

    private fun decodeUtf8(bytes: ByteArray): String = try {
        StandardCharsets.UTF_8.newDecoder()
            .onMalformedInput(CodingErrorAction.REPORT)
            .onUnmappableCharacter(CodingErrorAction.REPORT)
            .decode(ByteBuffer.wrap(bytes))
            .toString()
    } catch (error: Exception) {
        throw ZaraWireException("envelope is not valid UTF-8", error)
    }

    private fun requireExactKeys(
        value: Map<String, Any?>,
        expected: Set<String>,
        name: String,
    ) {
        if (value.keys != expected) throw ZaraWireException("$name has invalid fields")
    }

    private fun requiredString(value: Map<String, Any?>, key: String): String =
        value[key] as? String ?: throw ZaraWireException("$key must be string")

    private fun requiredLong(value: Map<String, Any?>, key: String): Long =
        value[key] as? Long ?: throw ZaraWireException("$key must be integer")

    private fun requiredObject(value: Map<String, Any?>, key: String): Map<String, Any?> {
        @Suppress("UNCHECKED_CAST")
        return value[key] as? Map<String, Any?> ?: throw ZaraWireException("$key must be object")
    }

    private fun requiredArray(value: Map<String, Any?>, key: String): List<Any?> =
        value[key] as? List<Any?> ?: throw ZaraWireException("$key must be array")
}

private class CapabilityJsonParser(private val source: String) {
    private var index = 0

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
        val result = linkedMapOf<String, Any?>()
        if (consume('}')) return result
        while (true) {
            skipWhitespace()
            if (peek() != '"') fail("object key must be string")
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
        if (consume('-') && index >= source.length) fail("invalid integer")
        if (peek() == '0') {
            index++
            if (peek()?.isDigit() == true) fail("leading zero")
        } else {
            if (peek() !in '1'..'9') fail("invalid integer")
            while (peek()?.isDigit() == true) index++
        }
        if (peek() == '.' || peek() == 'e' || peek() == 'E') fail("non-integer number")
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
        if (peek() != character) return false
        index++
        return true
    }

    private fun peek(): Char? = source.getOrNull(index)

    private fun fail(message: String): Nothing = throw ZaraWireException(message)
}
