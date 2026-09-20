package ai.zara.app.peer

import java.util.concurrent.TimeUnit
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Assume.assumeTrue
import org.junit.Test
import org.zeromq.ZCert

/**
 * Cross-implementation interop: the canonical Python ZARA/1 CURVE peer client
 * (android/integration/zara_peer_client_fixture.py, itself proven against the
 * real Python SecureZaraZmqGateway by t/test_zara_peer_client_fixture.py)
 * connects to the Kotlin peer listener gateway hosted by this test.
 */
class PythonPeerClientInteropTest {
    @Test
    fun pythonClientCompletesAuthenticatedHelloAgainstKotlinListener() {
        val fixturePath = System.getenv("ZARA_PEER_CLIENT_FIXTURE")
        assumeTrue("python peer client fixture is supplied by scripts/test-android.sh", fixturePath != null)
        val serverCertificate = ZCert()
        val clientCertificate = ZCert()
        val registry = PeerEnrollmentRegistry()
        registry.enroll("desktop-01", clientCertificate.publicKeyAsZ85, 3)
        val gateway = PeerNodeGateway(
            lifecycle = PeerNodeListenerLifecycle(
                PeerNodeIdentity("phone-01", serverCertificate.publicKeyAsZ85, 5),
            ),
            registry = registry,
            serverSecretKeyZ85 = serverCertificate.secretKeyAsZ85,
            bindEndpoint = "tcp://127.0.0.1:0",
        )
        gateway.start()
        try {
            val output = runFixture(
                gateway,
                serverCertificate,
                clientCertificate,
                nodeId = "desktop-01",
                generation = 3,
            )

            assertTrue(output, output.contains("RESULT type=hello.ok session_id="))
            assertTrue(output, !output.contains("RESULT timeout"))
            assertTrue(output, !output.contains("protocol.error"))
            assertEquals(PeerListenerPhase.ACTIVE, gateway.snapshot().phase)
        } finally {
            gateway.stop()
        }
    }

    @Test
    fun pythonClientSeesNodeAuthorityMismatchFromKotlinListener() {
        val fixturePath = System.getenv("ZARA_PEER_CLIENT_FIXTURE")
        assumeTrue("python peer client fixture is supplied by scripts/test-android.sh", fixturePath != null)
        val serverCertificate = ZCert()
        val clientCertificate = ZCert()
        val registry = PeerEnrollmentRegistry()
        registry.enroll("desktop-01", clientCertificate.publicKeyAsZ85, 3)
        val gateway = PeerNodeGateway(
            lifecycle = PeerNodeListenerLifecycle(
                PeerNodeIdentity("phone-01", serverCertificate.publicKeyAsZ85, 5),
            ),
            registry = registry,
            serverSecretKeyZ85 = serverCertificate.secretKeyAsZ85,
            bindEndpoint = "tcp://127.0.0.1:0",
        )
        gateway.start()
        try {
            val output = runFixture(
                gateway,
                serverCertificate,
                clientCertificate,
                nodeId = "impostor-node",
                generation = 3,
            )

            assertTrue(
                output,
                output.contains("RESULT type=protocol.error code=node_authority_mismatch retryable=False"),
            )
        } finally {
            gateway.stop()
        }
    }

    @Test
    fun pythonClientWithUnenrolledKeyGetsNoApplicationReply() {
        val fixturePath = System.getenv("ZARA_PEER_CLIENT_FIXTURE")
        assumeTrue("python peer client fixture is supplied by scripts/test-android.sh", fixturePath != null)
        val serverCertificate = ZCert()
        val registry = PeerEnrollmentRegistry()
        val gateway = PeerNodeGateway(
            lifecycle = PeerNodeListenerLifecycle(
                PeerNodeIdentity("phone-01", serverCertificate.publicKeyAsZ85, 5),
            ),
            registry = registry,
            serverSecretKeyZ85 = serverCertificate.secretKeyAsZ85,
            bindEndpoint = "tcp://127.0.0.1:0",
        )
        gateway.start()
        try {
            val output = runFixture(
                gateway,
                serverCertificate,
                ZCert(),
                nodeId = null,
                generation = null,
                timeoutMillis = "1500",
            )

            assertTrue(output, output.contains("RESULT timeout"))
        } finally {
            gateway.stop()
        }
    }

    private fun runFixture(
        gateway: PeerNodeGateway,
        serverCertificate: ZCert,
        clientCertificate: ZCert,
        nodeId: String?,
        generation: Long?,
        timeoutMillis: String = "8000",
    ): String {
        val python = System.getenv("ZARA_PEER_PYTHON") ?: "python3"
        val command = mutableListOf(
            python,
            requireNotNull(System.getenv("ZARA_PEER_CLIENT_FIXTURE")),
            "--endpoint",
            gateway.boundEndpoints().first(),
            "--server-key",
            serverCertificate.publicKeyAsZ85,
            "--client-key",
            clientCertificate.publicKeyAsZ85,
            "--client-secret",
            clientCertificate.secretKeyAsZ85,
            "--timeout-ms",
            timeoutMillis,
        )
        nodeId?.let {
            command += listOf("--node-id", it)
        }
        generation?.let {
            command += listOf("--generation", it.toString())
        }
        val process = ProcessBuilder(command)
            .redirectErrorStream(true)
            .start()
        assertTrue("fixture client must finish", process.waitFor(40, TimeUnit.SECONDS))
        return process.inputStream.readBytes().decodeToString()
    }
}
