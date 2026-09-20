package ai.zara.app.peer

import ai.zara.app.auth.JeroMqCurveKeyCodec
import ai.zara.app.runtime.StrictJsonParser
import ai.zara.app.runtime.ZaraWireException
import java.util.UUID

class PeerWireException(val code: String, message: String) : IllegalArgumentException(message)

enum class PeerDeviceClass(val wireName: String) {
    ANDROID("android"),
    DESKTOP("desktop"),
    SERVER("server"),
    HOSTED("hosted"),
}

data class PeerNodeDocument(
    val nodeId: String,
    val displayName: String,
    val deviceClass: PeerDeviceClass,
    val curvePublicKeyZ85: String,
    val endpoints: List<String>,
    val capabilities: Set<String>,
    val protocolVersions: Set<String>,
    val lastSeen: Long,
    val enrollmentGeneration: Long,
)

sealed interface PeerClientMessage {
    val id: String

    data class Hello(
        override val id: String,
        val timestampNs: Long,
        val versions: List<Long>,
        val node: PeerNodeDocument?,
    ) : PeerClientMessage

    data class Ping(
        override val id: String,
        val timestampNs: Long,
    ) : PeerClientMessage

    data class UnsupportedType(
        override val id: String,
        val type: String,
    ) : PeerClientMessage
}

private const val MAX_ENVELOPE_BYTES = 64 * 1024
private const val MAX_ID_BYTES = 128
private const val MAX_TYPE_BYTES = 64
private const val MAX_NODE_ID_CHARS = 128
private const val MAX_DISPLAY_NAME_BYTES = 256
private const val MAX_ENDPOINT_BYTES = 512
private const val MAX_ENDPOINTS = 32
private const val MAX_PROTOCOL_VERSIONS = 16
private val NODE_ID = Regex("[A-Za-z0-9][A-Za-z0-9._:-]{0,127}")
private val TYPE = Regex("[a-z][a-z0-9]*(\\.[a-z0-9]+)*")
private val ENDPOINT_HOST = Regex("[A-Za-z0-9.:-]+")
private val PROTOCOL_VERSION = Regex("ZARA/[1-9][0-9]*")
private val DEVICE_CAPABILITIES = setOf("open_app", "open_uri")
private val ENVELOPE_KEYS = setOf(
    "type", "id", "reply_to", "session_id", "conversation_id", "turn_id",
    "stream_id", "seq", "timestamp_ns", "trace_id", "content_type",
    "payload_count", "flags", "body",
)
private val HELLO_BODY_KEYS = setOf("versions", "node", "audio_output_formats")
private val NODE_KEYS = setOf(
    "node_id", "display_name", "device_class", "curve_public_key", "endpoints",
    "capabilities", "protocol_versions", "last_seen", "enrollment_generation",
)

/**
 * Strict server-side ZARA/1 codec for the Android peer listener. It mirrors the
 * canonical Python wire grammar: a "ZARA/1" marker frame, one bounded JSON
 * envelope with a closed key set, and node documents that must match the
 * authenticated registry rather than carrying authority of their own.
 */
object PeerNodeWire {
    private val marker = "ZARA/1".encodeToByteArray()
    private val maxPayloadFrames = 16L
    private val maxPayloadFrameBytes = 1024 * 1024L
    private val maxPayloadBytes = 4 * 1024 * 1024L

    fun decode(frames: List<ByteArray>): PeerClientMessage {
        if (frames.size != 2 || !frames[0].contentEquals(marker)) {
            throw invalid("invalid_message", "invalid ZARA/1 frame layout")
        }
        if (frames[1].size > MAX_ENVELOPE_BYTES) {
            throw invalid("invalid_message", "envelope exceeds byte limit")
        }
        val envelope = try {
            StrictJsonParser(frames[1].decodeToString()).parseObject()
        } catch (_: Exception) {
            throw invalid("invalid_message", "envelope is not strict JSON")
        }
        if ((envelope.keys - ENVELOPE_KEYS).isNotEmpty()) {
            throw invalid("invalid_message", "envelope contains unknown fields")
        }
        val type = requireString(envelope, "type", MAX_TYPE_BYTES)
        if (!TYPE.matches(type)) {
            throw invalid("invalid_message", "message type is not grammatical")
        }
        val id = token(envelope, "id")
        requireLong(envelope, "timestamp_ns", minimum = 0)
        requireLong(envelope, "payload_count", exact = 0)
        envelope["session_id"]?.let { token(envelope, "session_id") }
        val body = optionalObject(envelope)

        return when (type) {
            "hello" -> decodeHello(id, requireLong(envelope, "timestamp_ns"), body)
            "ping" -> decodePing(id, requireLong(envelope, "timestamp_ns"), body)
            else -> PeerClientMessage.UnsupportedType(id, type)
        }
    }

    fun encodeHelloOk(requestId: String, sessionId: String): List<ByteArray> = frames(
        linkedMapOf(
            "body" to linkedMapOf(
                "version" to 1L,
                "max_payload_frames" to maxPayloadFrames,
                "max_payload_frame_bytes" to maxPayloadFrameBytes,
                "max_payload_bytes" to maxPayloadBytes,
            ),
            "id" to newId(),
            "reply_to" to requestId,
            "session_id" to sessionId,
            "payload_count" to 0L,
            "timestamp_ns" to now(),
            "type" to "hello.ok",
        ),
    )

    fun encodePong(requestId: String, sessionId: String): List<ByteArray> = frames(
        linkedMapOf(
            "id" to newId(),
            "reply_to" to requestId,
            "session_id" to sessionId,
            "payload_count" to 0L,
            "timestamp_ns" to now(),
            "type" to "pong",
        ),
    )

    fun encodeProtocolError(
        requestId: String?,
        sessionId: String?,
        code: String,
        message: String,
        retryable: Boolean,
    ): List<ByteArray> {
        val envelope = linkedMapOf<String, Any?>(
            "body" to linkedMapOf(
                "code" to code,
                "message" to message,
                "retryable" to retryable,
            ),
            "id" to newId(),
            "payload_count" to 0L,
            "timestamp_ns" to now(),
            "type" to "protocol.error",
        )
        if (requestId != null) envelope["reply_to"] = requestId
        if (sessionId != null) envelope["session_id"] = sessionId
        return frames(envelope)
    }

    private fun decodeHello(id: String, timestampNs: Long, body: Map<String, Any?>): PeerClientMessage.Hello {
        if ((body.keys - HELLO_BODY_KEYS).isNotEmpty()) {
            throw invalid("invalid_message", "hello body contains unknown fields")
        }
        val versions = requireLongList(body, "versions")
        if (versions.isEmpty() || 1L !in versions) {
            throw invalid("unsupported_version", "no supported ZARA protocol version offered")
        }
        val node = body["node"]?.let { decodeNode(optionalObjectValue(it, "node")) }
        return PeerClientMessage.Hello(
            id = id,
            timestampNs = timestampNs,
            versions = versions,
            node = node,
        )
    }

    private fun decodePing(id: String, timestampNs: Long, body: Map<String, Any?>): PeerClientMessage.Ping {
        if (body.isNotEmpty()) {
            throw invalid("invalid_message", "ping body must be empty")
        }
        return PeerClientMessage.Ping(id, timestampNs)
    }

    private fun decodeNode(value: Map<String, Any?>): PeerNodeDocument {
        if (value.keys != NODE_KEYS) {
            throw invalid("invalid_node", "node document must contain exactly the canonical fields")
        }
        val nodeId = requireString(value, "node_id", MAX_NODE_ID_CHARS)
        if (!NODE_ID.matches(nodeId)) {
            throw invalid("invalid_node", "node id is not canonical")
        }
        val displayName = requireString(value, "display_name", MAX_DISPLAY_NAME_BYTES)
        if (displayName.isBlank()) {
            throw invalid("invalid_node", "display name is required")
        }
        val deviceClass = PeerDeviceClass.entries.firstOrNull {
            it.wireName == requireString(value, "device_class", 32)
        } ?: throw invalid("invalid_node", "device class is unknown")
        val curveKey = requireString(value, "curve_public_key", 40)
        try {
            JeroMqCurveKeyCodec.decode(curveKey)
        } catch (_: Exception) {
            throw invalid("invalid_node", "curve public key is not valid Z85")
        }
        val endpoints = requireStringList(value, "endpoints", MAX_ENDPOINTS)
        val seen = LinkedHashSet<String>(endpoints.size)
        endpoints.forEach { endpoint ->
            if (endpoint.encodeToByteArray().size > MAX_ENDPOINT_BYTES) {
                throw invalid("invalid_node", "endpoint exceeds byte limit")
            }
            validateTcpEndpoint(endpoint)
            if (!seen.add(endpoint)) throw invalid("invalid_node", "duplicate endpoint")
        }
        val capabilities = requireStringList(value, "capabilities", 16)
        if ((capabilities.toSet() - DEVICE_CAPABILITIES).isNotEmpty() || capabilities.size != capabilities.toSet().size) {
            throw invalid("invalid_node", "node capabilities must be device capabilities")
        }
        val protocolVersions = requireStringList(value, "protocol_versions", MAX_PROTOCOL_VERSIONS)
        if (protocolVersions.isEmpty() || protocolVersions.any { !PROTOCOL_VERSION.matches(it) } ||
            protocolVersions.size != protocolVersions.toSet().size
        ) {
            throw invalid("invalid_node", "protocol versions are invalid")
        }
        val lastSeen = requireLong(value, "last_seen", minimum = 0, code = "invalid_node")
        val generation = requireLong(value, "enrollment_generation", minimum = 1, code = "invalid_node")

        return PeerNodeDocument(
            nodeId = nodeId,
            displayName = displayName,
            deviceClass = deviceClass,
            curvePublicKeyZ85 = curveKey,
            endpoints = seen.toList(),
            capabilities = capabilities.toSet(),
            protocolVersions = protocolVersions.toSet(),
            lastSeen = lastSeen,
            enrollmentGeneration = generation,
        )
    }

    private fun validateTcpEndpoint(endpoint: String) {
        if (!endpoint.startsWith("tcp://") || endpoint.any(Char::isWhitespace)) {
            throw invalid("invalid_node", "endpoint must be a bounded tcp:// address")
        }
        val remainder = endpoint.removePrefix("tcp://")
        if (remainder.isEmpty() || remainder.any { it == '@' || it == '?' || it == '#' }) {
            throw invalid("invalid_node", "endpoint must not carry credentials, query, or fragment")
        }
        val authority = remainder.substringBefore('/')
        val path = remainder.removePrefix(authority)
        if (path.isNotEmpty() && path != "/") {
            throw invalid("invalid_node", "endpoint must not carry a path")
        }
        val host = authority.substringBeforeLast(':')
        val portText = if (host == authority) null else authority.substringAfterLast(':')
        if (host.isEmpty() || !ENDPOINT_HOST.matches(host)) {
            throw invalid("invalid_node", "endpoint host is invalid")
        }
        val port = portText?.toIntOrNull()
            ?: throw invalid("invalid_node", "endpoint port is required")
        if (port !in 1..65535) throw invalid("invalid_node", "endpoint port is out of range")
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
        is List<*> -> value.joinToString(separator = ",", prefix = "[", postfix = "]") { encodeJson(it) }
        is Map<*, *> -> value.entries
            .map { (key, item) -> (key as? String ?: throw ZaraWireException("JSON object key must be string")) to item }
            .sortedBy { it.first }
            .joinToString(separator = ",", prefix = "{", postfix = "}") { (key, item) ->
                "${encodeJson(key)}:${encodeJson(item)}"
            }
        else -> throw ZaraWireException("unsupported JSON value")
    }

    private fun invalid(code: String, message: String): PeerWireException =
        PeerWireException(code, message)

    private fun newId(): String = UUID.randomUUID().toString()

    private fun now(): Long = System.currentTimeMillis()

    private fun token(envelope: Map<String, Any?>, key: String): String {
        val value = envelope[key] as? String
            ?: throw invalid("invalid_message", "$key must be a string")
        if (value.isBlank() || value.encodeToByteArray().size > MAX_ID_BYTES) {
            throw invalid("invalid_message", "$key is out of bounds")
        }
        if (value.any { it.code !in 0x21..0x7e }) {
            throw invalid("invalid_message", "$key must be printable ASCII")
        }
        return value
    }

    private fun optionalObject(envelope: Map<String, Any?>): Map<String, Any?> =
        optionalObjectValue(envelope["body"], "body")

    private fun optionalObjectValue(value: Any?, key: String): Map<String, Any?> {
        if (value == null) return emptyMap()
        @Suppress("UNCHECKED_CAST")
        return value as? Map<String, Any?> ?: throw invalid("invalid_message", "$key must be an object")
    }

    private fun requireString(value: Map<String, Any?>, key: String, maximumBytes: Int): String {
        val text = value[key] as? String
            ?: throw invalid("invalid_node", "$key must be a string")
        if (text.encodeToByteArray().size > maximumBytes) {
            throw invalid("invalid_node", "$key exceeds byte limit")
        }
        return text
    }

    private fun requireStringList(value: Map<String, Any?>, key: String, maximum: Int): List<String> {
        val list = value[key] as? List<*>
            ?: throw invalid("invalid_node", "$key must be a list")
        if (list.size > maximum) throw invalid("invalid_node", "$key exceeds size limit")
        return list.map { item ->
            item as? String ?: throw invalid("invalid_node", "$key must contain strings")
        }
    }

    private fun requireLongList(value: Map<String, Any?>, key: String): List<Long> {
        val list = value[key] as? List<*>
            ?: throw invalid("invalid_message", "$key must be a list")
        return list.map { item ->
            item as? Long ?: throw invalid("invalid_message", "$key must contain integers")
        }
    }

    private fun requireLong(
        value: Map<String, Any?>,
        key: String,
        minimum: Long? = null,
        exact: Long? = null,
        code: String = "invalid_message",
    ): Long {
        val number = value[key] as? Long
            ?: throw invalid(code, "$key must be integer")
        if (minimum != null && number < minimum) {
            throw invalid(code, "$key is below minimum")
        }
        if (exact != null && number != exact) {
            throw invalid(code, "$key has unsupported value")
        }
        return number
    }
}
