package ai.zara.app.peer

import ai.zara.app.runtime.StrictJsonParser
import ai.zara.app.runtime.TextServerMessage
import ai.zara.app.runtime.ZaraTextCodec
import java.net.ServerSocket
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test
import org.zeromq.SocketType
import org.zeromq.ZCert
import org.zeromq.ZContext
import org.zeromq.ZMQ

class PeerNodeGatewayTest {
    private val serverCertificate = ZCert()

    private fun identity(key: String = "P".repeat(40)) = PeerNodeIdentity(
        nodeId = "phone-01",
        curvePublicKeyZ85 = key,
        enrollmentGeneration = 5,
    )

    private fun newGateway(
        registry: PeerEnrollmentRegistry,
        endpoint: String = "tcp://127.0.0.1:0",
        maxRoutes: Int = 32,
        rateMaxMessages: Int = 60,
        rateWindowMillis: Long = 1_000,
    ): PeerNodeGateway = PeerNodeGateway(
        lifecycle = PeerNodeListenerLifecycle(identity(serverCertificate.publicKeyAsZ85)),
        registry = registry,
        serverSecretKeyZ85 = serverCertificate.secretKeyAsZ85,
        bindEndpoint = endpoint,
        maxRoutes = maxRoutes,
        rateMaxMessages = rateMaxMessages,
        rateWindowMillis = rateWindowMillis,
    )

    private fun nodeDocument(
        client: ZCert,
        nodeId: String = "desktop-01",
        generation: Long = 3,
    ): Map<String, Any?> = linkedMapOf(
        "node_id" to nodeId,
        "display_name" to "Desktop",
        "device_class" to "desktop",
        "curve_public_key" to client.publicKeyAsZ85,
        "endpoints" to listOf("tcp://192.0.2.10:17865"),
        "capabilities" to listOf("open_app"),
        "protocol_versions" to listOf("ZARA/1"),
        "last_seen" to 1L,
        "enrollment_generation" to generation,
    )

    private fun helloFrames(
        client: ZCert,
        nodeId: String = "desktop-01",
        generation: Long = 3,
        withNode: Boolean = true,
    ): List<ByteArray> {
        val nodeJson = if (withNode) {
            val fields = nodeDocument(client, nodeId, generation).entries.joinToString(",") { (key, value) ->
                when (value) {
                    is String -> "\"$key\":\"$value\""
                    is List<*> -> "\"$key\":${value.joinToString(prefix = "[", postfix = "]") { "\"$it\"" }}"
                    else -> "\"$key\":$value"
                }
            }
            ",\"node\":{$fields}"
        } else {
            ""
        }
        val envelope = "{\"body\":{\"versions\":[1]$nodeJson},\"id\":\"req-1\",\"payload_count\":0," +
            "\"timestamp_ns\":5,\"type\":\"hello\"}"
        return listOf("ZARA/1".encodeToByteArray(), envelope.encodeToByteArray())
    }

    private fun pingFrames(): List<ByteArray> = listOf(
        "ZARA/1".encodeToByteArray(),
        "{\"id\":\"ping-1\",\"payload_count\":0,\"timestamp_ns\":6,\"type\":\"ping\"}".encodeToByteArray(),
    )

    private class TestPeerClient(
        endpoint: String,
        server: ZCert,
        client: ZCert,
    ) {
        private val context = ZContext()
        private val socket = context.createSocket(SocketType.DEALER)

        init {
            socket.setLinger(0)
            socket.setHandshakeIvl(5_000)
            socket.setCurveServerKey(server.publicKey)
            socket.setCurvePublicKey(client.publicKey)
            socket.setCurveSecretKey(client.secretKey)
            assertTrue(socket.connect(endpoint))
        }

        fun send(frames: List<ByteArray>) {
            frames.forEachIndexed { index, frame ->
                val flags = if (index == frames.lastIndex) 0 else ZMQ.SNDMORE
                assertTrue(socket.send(frame, flags))
            }
        }

        fun receiveEnvelope(timeoutMillis: Int = 5_000): Map<String, Any?>? {
            socket.receiveTimeOut = timeoutMillis
            val first = socket.recv(0) ?: return null
            val frames = mutableListOf(first)
            while (socket.hasReceiveMore()) {
                frames += socket.recv(0) ?: return null
            }
            assertTrue(frames[0].contentEquals("ZARA/1".encodeToByteArray()))
            return StrictJsonParser(frames[1].decodeToString()).parseObject()
        }

        fun receiveDecoded(timeoutMillis: Int = 5_000): TextServerMessage? {
            socket.receiveTimeOut = timeoutMillis
            val first = socket.recv(0) ?: return null
            val frames = mutableListOf(first)
            while (socket.hasReceiveMore()) {
                frames += socket.recv(0) ?: return null
            }
            return ZaraTextCodec.decode(frames)
        }

        fun close() {
            socket.close()
            context.close()
        }
    }

    private fun connectClient(
        gateway: PeerNodeGateway,
        client: ZCert,
    ): TestPeerClient = TestPeerClient(gateway.boundEndpoints().first(), serverCertificate, client)

    @Test
    fun authenticatedHelloWithMatchingNodeReceivesHelloOk() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry)
        gateway.start()
        val peer = connectClient(gateway, client)
        try {
            peer.send(helloFrames(client))

            val helloOk = peer.receiveDecoded() as TextServerMessage.HelloOk

            assertTrue(helloOk.sessionId.isNotBlank())
            assertEquals("req-1", helloOk.replyTo)
            assertEquals(1, helloOk.version)
            assertEquals(PeerListenerPhase.ACTIVE, gateway.snapshot().phase)
        } finally {
            peer.close()
            gateway.stop()
        }
    }

    @Test
    fun nodeMismatchReceivesNodeAuthorityMismatchThenCorrectHelloStillSucceeds() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry)
        gateway.start()
        val peer = connectClient(gateway, client)
        try {
            peer.send(helloFrames(client, nodeId = "other-node"))
            val mismatch = peer.receiveDecoded() as TextServerMessage.ProtocolError
            assertEquals("node_authority_mismatch", mismatch.code)
            assertEquals(false, mismatch.retryable)

            peer.send(helloFrames(client))
            val helloOk = peer.receiveDecoded() as TextServerMessage.HelloOk
            assertTrue(helloOk.sessionId.isNotBlank())
        } finally {
            peer.close()
            gateway.stop()
        }
    }

    @Test
    fun malformedNodeReceivesInvalidNode() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry)
        gateway.start()
        val peer = connectClient(gateway, client)
        try {
            val hostile = helloFrames(client)
            val envelope = hostile[1].decodeToString()
                .replace("\"open_app\"", "\"daemon.admin\"")
            peer.send(listOf(hostile[0], envelope.encodeToByteArray()))

            val error = peer.receiveDecoded() as TextServerMessage.ProtocolError

            assertEquals("invalid_node", error.code)
        } finally {
            peer.close()
            gateway.stop()
        }
    }

    @Test
    fun unenrolledKeyGetsNoApplicationReply() {
        val registry = PeerEnrollmentRegistry()
        val gateway = newGateway(registry)
        gateway.start()
        val intruder = connectClient(gateway, ZCert())
        try {
            intruder.send(helloFrames(ZCert()))

            assertNull(intruder.receiveEnvelope(timeoutMillis = 2_000))
        } finally {
            intruder.close()
            gateway.stop()
        }
    }

    @Test
    fun revokedPeerFailsClosedOnNextMessage() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry)
        gateway.start()
        val peer = connectClient(gateway, client)
        try {
            peer.send(helloFrames(client))
            assertTrue(peer.receiveDecoded() is TextServerMessage.HelloOk)

            assertTrue(registry.revoke("desktop-01"))
            peer.send(pingFrames())

            val error = peer.receiveDecoded() as TextServerMessage.ProtocolError
            assertEquals("authentication_required", error.code)
        } finally {
            peer.close()
            gateway.stop()
        }
    }

    @Test
    fun legacyHelloWithoutNodeIsAccepted() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry)
        gateway.start()
        val peer = connectClient(gateway, client)
        try {
            peer.send(helloFrames(client, withNode = false))

            assertTrue(peer.receiveDecoded() is TextServerMessage.HelloOk)
        } finally {
            peer.close()
            gateway.stop()
        }
    }

    @Test
    fun pingOnEstablishedSessionReceivesPong() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry)
        gateway.start()
        val peer = connectClient(gateway, client)
        try {
            peer.send(helloFrames(client))
            val helloOk = peer.receiveDecoded() as TextServerMessage.HelloOk
            peer.send(pingFrames())

            val pong = requireNotNull(peer.receiveEnvelope())

            assertEquals("pong", pong["type"])
            assertEquals("ping-1", pong["reply_to"])
            assertEquals(helloOk.sessionId, pong["session_id"])
        } finally {
            peer.close()
            gateway.stop()
        }
    }

    @Test
    fun pingBeforeHelloReceivesAuthenticationRequired() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry)
        gateway.start()
        val peer = connectClient(gateway, client)
        try {
            peer.send(pingFrames())

            val error = peer.receiveDecoded() as TextServerMessage.ProtocolError

            assertEquals("authentication_required", error.code)
        } finally {
            peer.close()
            gateway.stop()
        }
    }

    @Test
    fun unsupportedTurnSubmitReceivesAuthorizationDenied() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry)
        gateway.start()
        val peer = connectClient(gateway, client)
        try {
            peer.send(helloFrames(client))
            assertTrue(peer.receiveDecoded() is TextServerMessage.HelloOk)
            peer.send(
                listOf(
                    "ZARA/1".encodeToByteArray(),
                    (
                        "{\"body\":{\"text\":\"hi\"},\"id\":\"turn-1\",\"payload_count\":0," +
                            "\"timestamp_ns\":7,\"type\":\"turn.submit\"}"
                        ).encodeToByteArray(),
                ),
            )

            val error = peer.receiveDecoded() as TextServerMessage.ProtocolError

            assertEquals("authorization_denied", error.code)
        } finally {
            peer.close()
            gateway.stop()
        }
    }

    @Test
    fun malformedEnvelopeReceivesInvalidMessage() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry)
        gateway.start()
        val peer = connectClient(gateway, client)
        try {
            peer.send(listOf("ZARA/1".encodeToByteArray(), "this is not json".encodeToByteArray()))

            val error = peer.receiveDecoded() as TextServerMessage.ProtocolError

            assertEquals("invalid_message", error.code)
        } finally {
            peer.close()
            gateway.stop()
        }
    }

    @Test
    fun secondConnectionWithSameCredentialDropsPriorRoute() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry)
        gateway.start()
        val first = connectClient(gateway, client)
        val second = connectClient(gateway, client)
        try {
            first.send(helloFrames(client))
            assertTrue(first.receiveDecoded() is TextServerMessage.HelloOk)
            second.send(helloFrames(client))
            assertTrue(second.receiveDecoded() is TextServerMessage.HelloOk)

            first.send(pingFrames())
            val error = first.receiveDecoded() as TextServerMessage.ProtocolError

            assertEquals("authentication_required", error.code)
            second.send(pingFrames())
            assertEquals("pong", requireNotNull(second.receiveEnvelope())["type"])
        } finally {
            first.close()
            second.close()
            gateway.stop()
        }
    }

    @Test
    fun routeTableIsBoundedAndRejectsWithQuotaExceeded() {
        val registry = PeerEnrollmentRegistry()
        val gateway = newGateway(registry, maxRoutes = 1)
        gateway.start()
        val firstClient = ZCert()
        val secondClient = ZCert()
        registry.enroll("desktop-01", firstClient.publicKeyAsZ85, 3)
        registry.enroll("desktop-02", secondClient.publicKeyAsZ85, 3)
        val first = connectClient(gateway, firstClient)
        val second = connectClient(gateway, secondClient)
        try {
            first.send(helloFrames(firstClient, nodeId = "desktop-01"))
            assertTrue(first.receiveDecoded() is TextServerMessage.HelloOk)

            second.send(helloFrames(secondClient, nodeId = "desktop-02"))
            val error = second.receiveDecoded() as TextServerMessage.ProtocolError

            assertEquals("quota_exceeded", error.code)
        } finally {
            first.close()
            second.close()
            gateway.stop()
        }
    }

    @Test
    fun messageRateIsBoundedAndRejectsWithQuotaExceeded() {
        val registry = PeerEnrollmentRegistry()
        val client = ZCert()
        registry.enroll("desktop-01", client.publicKeyAsZ85, 3)
        val gateway = newGateway(registry, rateMaxMessages = 3, rateWindowMillis = 60_000)
        gateway.start()
        val peer = connectClient(gateway, client)
        try {
            peer.send(helloFrames(client))
            assertTrue(peer.receiveDecoded() is TextServerMessage.HelloOk)
            repeat(2) {
                peer.send(pingFrames())
                assertEquals("pong", requireNotNull(peer.receiveEnvelope())["type"])
            }

            peer.send(pingFrames())
            val error = peer.receiveDecoded() as TextServerMessage.ProtocolError

            assertEquals("quota_exceeded", error.code)
        } finally {
            peer.close()
            gateway.stop()
        }
    }

    @Test
    fun bindFailureFailsLifecycleTruthfullyAndRetryWorks() {
        val registry = PeerEnrollmentRegistry()
        ServerSocket(0).use { blocker ->
            val endpoint = "tcp://127.0.0.1:${blocker.localPort}"
            val gateway = newGateway(registry, endpoint = endpoint)

            assertThrows(IllegalStateException::class.java) { gateway.start() }

            val snapshot = gateway.snapshot()
            assertEquals(PeerListenerPhase.FAILED, snapshot.phase)
            assertNotNull(snapshot.failure)
            gateway.stop()
        }

        val retry = newGateway(registry)
        retry.start()
        try {
            assertEquals(PeerListenerPhase.ACTIVE, retry.snapshot().phase)
        } finally {
            retry.stop()
        }
    }

    @Test
    fun stopIsIdempotentAndNetworkChangeUpdatesAdvertisedEndpoints() {
        val registry = PeerEnrollmentRegistry()
        val gateway = newGateway(registry)
        gateway.start()
        assertTrue(gateway.networkChanged(listOf("tcp://198.51.100.7:17865")))
        assertEquals(listOf("tcp://198.51.100.7:17865"), gateway.snapshot().endpoints)

        gateway.stop()
        gateway.stop()

        assertEquals(PeerListenerPhase.STOPPED, gateway.snapshot().phase)
        assertEquals(emptyList<String>(), gateway.snapshot().endpoints)
        assertEquals(false, gateway.networkChanged(listOf("tcp://198.51.100.8:17865")))
    }

    @Test
    fun startPublishesBoundEndpointAndPreservesIdentity() {
        val registry = PeerEnrollmentRegistry()
        val gateway = newGateway(registry)
        gateway.start()
        try {
            val bound = gateway.boundEndpoints()
            assertEquals(1, bound.size)
            assertTrue(bound.first().startsWith("tcp://"))
            assertEquals(bound, gateway.snapshot().endpoints)
            assertEquals(identity(serverCertificate.publicKeyAsZ85), gateway.snapshot().identity)
        } finally {
            gateway.stop()
        }
    }
}
