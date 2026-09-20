package ai.zara.app.peer

import ai.zara.app.runtime.StrictJsonParser
import ai.zara.app.runtime.TextServerMessage
import ai.zara.app.runtime.ZaraTextCodec
import ai.zara.app.runtime.ZaraWireException
import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class PeerNodeWireTest {
    private fun nodeDocument(
        nodeId: String = "desktop-01",
        curveKey: String = "K".repeat(40),
        generation: Long = 3,
        extra: Pair<String, Any?>? = null,
        remove: String? = null,
    ): Map<String, Any?> {
        val document = LinkedHashMap<String, Any?>()
        document["node_id"] = nodeId
        document["display_name"] = "Desktop"
        document["device_class"] = "desktop"
        document["curve_public_key"] = curveKey
        document["endpoints"] = listOf("tcp://192.0.2.10:17865")
        document["capabilities"] = listOf("open_app")
        document["protocol_versions"] = listOf("ZARA/1")
        document["last_seen"] = 1_000L
        document["enrollment_generation"] = generation
        if (extra != null) document[extra.first] = extra.second
        remove?.let { document.remove(it) }
        return document
    }

    private fun helloFrames(
        versions: List<Long> = listOf(1L),
        node: Map<String, Any?>? = null,
        id: String = "req-1",
        timestampNs: Long = 5L,
        bodyExtra: Pair<String, Any?>? = null,
        type: String = "hello",
    ): List<ByteArray> {
        val body = StringBuilder("{")
        body.append("\"versions\":").append(versions.joinToString(prefix = "[", postfix = "]"))
        if (node != null) {
            val fields = node.entries.joinToString(",") { (key, value) ->
                when (value) {
                    is String -> "\"$key\":\"$value\""
                    is List<*> -> "\"$key\":${value.joinToString(prefix = "[", postfix = "]") { "\"$it\"" }}"
                    else -> "\"$key\":$value"
                }
            }
            body.append(",\"node\":{").append(fields).append("}")
        }
        if (bodyExtra != null) {
            body.append(",\"").append(bodyExtra.first).append("\":")
            when (val value = bodyExtra.second) {
                is String -> body.append("\"$value\"")
                else -> body.append(value)
            }
        }
        body.append("}")
        val envelope = StringBuilder("{")
        envelope.append("\"body\":").append(body)
        envelope.append(",\"id\":\"$id\"")
        envelope.append(",\"payload_count\":0")
        envelope.append(",\"timestamp_ns\":$timestampNs")
        envelope.append(",\"type\":\"$type\"")
        envelope.append("}")
        return listOf(
            "ZARA/1".encodeToByteArray(),
            envelope.toString().encodeToByteArray(),
        )
    }

    @Test
    fun decodesHelloWithNodeDocument() {
        val node = nodeDocument()

        val message = PeerNodeWire.decode(helloFrames(node = node)) as PeerClientMessage.Hello

        assertEquals("req-1", message.id)
        assertEquals(5L, message.timestampNs)
        assertEquals(listOf(1L), message.versions)
        val decoded = requireNotNull(message.node)
        assertEquals("desktop-01", decoded.nodeId)
        assertEquals("Desktop", decoded.displayName)
        assertEquals(PeerDeviceClass.DESKTOP, decoded.deviceClass)
        assertEquals("K".repeat(40), decoded.curvePublicKeyZ85)
        assertEquals(listOf("tcp://192.0.2.10:17865"), decoded.endpoints)
        assertEquals(setOf("open_app"), decoded.capabilities)
        assertEquals(setOf("ZARA/1"), decoded.protocolVersions)
        assertEquals(1_000L, decoded.lastSeen)
        assertEquals(3L, decoded.enrollmentGeneration)
    }

    @Test
    fun decodesLegacyHelloWithoutNode() {
        val message = PeerNodeWire.decode(helloFrames()) as PeerClientMessage.Hello

        assertEquals(null, message.node)
    }

    @Test
    fun rejectsUnsupportedProtocolVersion() {
        assertThrows(PeerWireException::class.java) {
            PeerNodeWire.decode(helloFrames(versions = listOf(2L)))
        }.also { assertEquals("unsupported_version", it.code) }
    }

    @Test
    fun rejectsMalformedEnvelopeAsInvalidMessage() {
        listOf(
            "not-json".encodeToByteArray(),
            "{\"type\":\"hello\",\"id\":\"req-1\",\"timestamp_ns\":5}".encodeToByteArray(),
            "{\"type\":\"hello\",\"id\":\"req-1\",\"payload_count\":0,\"timestamp_ns\":-1}".encodeToByteArray(),
            "{\"type\":\"hello\",\"id\":\"req-1\",\"payload_count\":1,\"timestamp_ns\":5}".encodeToByteArray(),
            "{\"type\":\"hello\",\"id\":\"req-1\",\"payload_count\":0,\"timestamp_ns\":5,\"unknown\":1}".encodeToByteArray(),
            "{\"type\":\"hello\",\"id\":\"req-1\",\"payload_count\":0,\"timestamp_ns\":5,\"type\":\"hello\"}".encodeToByteArray(),
            "{\"type\":\"hello!\",\"id\":\"req-1\",\"payload_count\":0,\"timestamp_ns\":5}".encodeToByteArray(),
        ).forEach { envelope ->
            assertThrows(PeerWireException::class.java) {
                PeerNodeWire.decode(listOf("ZARA/1".encodeToByteArray(), envelope))
            }.also { assertEquals("invalid_message", it.code) }
        }
    }

    @Test
    fun rejectsWrongMarkerOrFrameCountAsInvalidMessage() {
        assertThrows(PeerWireException::class.java) {
            PeerNodeWire.decode(listOf("ZARA/2".encodeToByteArray(), "{}".encodeToByteArray()))
        }
        assertThrows(PeerWireException::class.java) {
            PeerNodeWire.decode(helloFrames() + byteArrayOf(1))
        }
    }

    @Test
    fun rejectsOversizedEnvelopeAsInvalidMessage() {
        val padding = "x".repeat(70 * 1024)
        val message = assertThrows(PeerWireException::class.java) {
            PeerNodeWire.decode(helloFrames(node = null, id = padding))
        }
        assertEquals("invalid_message", message.code)
    }

    @Test
    fun rejectsInvalidNodeDocumentAsInvalidNode() {
        listOf(
            nodeDocument(nodeId = "desktop 01"),
            nodeDocument(curveKey = "K".repeat(39)),
            nodeDocument(generation = 0),
            nodeDocument(extra = "principal_id" to "owner"),
            nodeDocument(remove = "node_id"),
            nodeDocument(nodeId = "x".repeat(129)),
        ).forEach { node ->
            assertThrows(PeerWireException::class.java) {
                PeerNodeWire.decode(helloFrames(node = node))
            }.also { assertEquals("invalid_node", it.code) }
        }
    }

    @Test
    fun rejectsNodeAuthorizationCapabilityAsInvalidNode() {
        val node = nodeDocument()
        node as LinkedHashMap
        node["capabilities"] = listOf("daemon.admin")

        assertThrows(PeerWireException::class.java) {
            PeerNodeWire.decode(helloFrames(node = node))
        }.also { assertEquals("invalid_node", it.code) }
    }

    @Test
    fun rejectsNodeHttpEndpointAsInvalidNode() {
        val node = nodeDocument()
        node as LinkedHashMap
        node["endpoints"] = listOf("http://192.0.2.10:17865")

        assertThrows(PeerWireException::class.java) {
            PeerNodeWire.decode(helloFrames(node = node))
        }.also { assertEquals("invalid_node", it.code) }
    }

    @Test
    fun rejectsUnknownHelloBodyFieldAsInvalidMessage() {
        assertThrows(PeerWireException::class.java) {
            PeerNodeWire.decode(helloFrames(bodyExtra = "surprise" to 1L))
        }.also { assertEquals("invalid_message", it.code) }
    }

    @Test
    fun decodesPing() {
        val envelope = "{\"id\":\"ping-1\",\"payload_count\":0,\"timestamp_ns\":9,\"type\":\"ping\"}"

        val message = PeerNodeWire.decode(
            listOf("ZARA/1".encodeToByteArray(), envelope.encodeToByteArray()),
        ) as PeerClientMessage.Ping

        assertEquals("ping-1", message.id)
    }

    @Test
    fun returnsUnknownTypeForGrammaticalTurnSubmit() {
        val message = PeerNodeWire.decode(helloFrames(type = "turn.submit"))

        assertTrue(message is PeerClientMessage.UnsupportedType)
        assertEquals("turn.submit", (message as PeerClientMessage.UnsupportedType).type)
    }

    @Test
    fun encodesHelloOkDecodableByClientCodec() {
        val frames = PeerNodeWire.encodeHelloOk(
            requestId = "req-1",
            sessionId = "session-9",
        )

        val decoded = ZaraTextCodec.decode(frames) as TextServerMessage.HelloOk

        assertEquals(1, decoded.version)
        assertEquals("session-9", decoded.sessionId)
        assertEquals("req-1", decoded.replyTo)
        assertTrue(decoded.maxPayloadFrames > 0)
        assertTrue(decoded.maxPayloadFrameBytes > 0)
        assertTrue(decoded.maxPayloadBytes > 0)
    }

    @Test
    fun encodesProtocolErrorDecodableByClientCodec() {
        val frames = PeerNodeWire.encodeProtocolError(
            requestId = "req-2",
            sessionId = null,
            code = "node_authority_mismatch",
            message = "peer node does not match authenticated identity",
            retryable = false,
        )

        val decoded = ZaraTextCodec.decode(frames) as TextServerMessage.ProtocolError

        assertEquals("node_authority_mismatch", decoded.code)
        assertEquals("peer node does not match authenticated identity", decoded.message)
        assertEquals(false, decoded.retryable)
        assertEquals("req-2", decoded.replyTo)
    }

    @Test
    fun encodesPong() {
        val frames = PeerNodeWire.encodePong(requestId = "ping-1", sessionId = "session-9")

        assertEquals("ZARA/1", frames[0].decodeToString())
        val parsed = StrictJsonParser(frames[1].decodeToString()).parseObject()
        assertEquals("pong", parsed["type"])
        assertEquals("ping-1", parsed["reply_to"])
        assertEquals("session-9", parsed["session_id"])
        assertEquals(0L, parsed["payload_count"])
    }
}
