package ai.zara.app.auth

import ai.zara.app.AndroidPairingCoordinator
import java.io.BufferedReader
import java.io.File
import java.io.InputStreamReader
import java.net.InetAddress
import java.net.ServerSocket
import java.net.URLEncoder
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.time.Instant
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CountDownLatch
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger
import org.json.JSONObject
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class PairingTest {
    private val serverKey = "a".repeat(40)
    private val clientKey = "b".repeat(40)

    @Test fun `qr payload parses strict zara pairing uri`() {
        val endpoint = "tcp://10.20.30.40:7731"
        val raw = "zara://pair/v1" +
            "?broker_host=10.20.30.40" +
            "&broker_port=45231" +
            "&endpoint=${encode(endpoint)}" +
            "&server_key=${encode(serverKey)}" +
            "&token=${encode("token-123")}" +
            "&expires=2000"

        val payload = PairingPayload.parse(raw, nowEpochSeconds = 1900)

        assertEquals("10.20.30.40", payload.brokerHost)
        assertEquals(45231, payload.brokerPort)
        assertEquals(endpoint, payload.endpoint)
        assertEquals(serverKey, payload.serverKey)
        assertEquals("token-123", payload.token)
        assertEquals(2000L, payload.expiresAtEpochSeconds)
    }

    @Test fun `qr payload rejects expired duplicate unknown and non tcp bootstrap data`() {
        val base = "zara://pair/v1?broker_host=host&broker_port=45231" +
            "&endpoint=${encode("tcp://host:7731")}" +
            "&server_key=${encode(serverKey)}&token=token-123&expires=2000"

        expectFailure { PairingPayload.parse(base, nowEpochSeconds = 2001) }
        expectFailure { PairingPayload.parse("$base&token=other-token", nowEpochSeconds = 1900) }
        expectFailure { PairingPayload.parse("$base&extra=nope", nowEpochSeconds = 1900) }
        expectFailure {
            PairingPayload.parse(
                base.replace(encode("tcp://host:7731"), encode("ipc:///tmp/zara.sock")),
                nowEpochSeconds = 1900,
            )
        }
    }

    @Test fun `verification code and automatic device id match server contract`() {
        assertEquals("android-e26d2da3ab58", PairingProtocol.deriveDeviceId(clientKey))
        val code = PairingProtocol.verificationCode("pairing-token", clientKey)
        assertEquals("944040", code)
        assertEquals(6, code.length)
        assertTrue(code.all(Char::isDigit))
        assertEquals(code, PairingProtocol.verificationCode("pairing-token", clientKey))
        assertTrue(code != PairingProtocol.verificationCode("pairing-token", "c".repeat(40)))
    }

    @Test fun `pairing reuses existing private identity instead of rotating it`() {
        val directory = Files.createTempDirectory("zara-pairing-identity").toFile()
        val publicKey = JeroMqCurveKeyCodec.decode(clientKey)
        val secretKey = ByteArray(32) { (it + 17).toByte() }
        val generator = CountingGenerator(publicKey, secretKey)
        val repository = EnrollmentRepository(
            credentials = WrappedCredentialStore(
                File(directory, "credential.bin"),
                XorCipher(0x5A),
            ),
            serverPins = ServerPinStore(File(directory, "pin.bin")),
            generator = generator,
        )

        assertEquals(clientKey, repository.identityZ85OrCreate())
        assertEquals(clientKey, repository.identityZ85OrCreate())
        assertEquals(1, generator.calls)
    }

    @Test fun `close during pending approval cancels socket future and later side effects`() {
        val directory = Files.createTempDirectory("zara-pairing-close").toFile()
        val publicKey = JeroMqCurveKeyCodec.decode(clientKey)
        val secretKey = ByteArray(32) { (it + 29).toByte() }
        val repository = EnrollmentRepository(
            credentials = WrappedCredentialStore(
                File(directory, "credential.bin"),
                XorCipher(0x33),
            ),
            serverPins = ServerPinStore(File(directory, "pin.bin")),
            generator = CountingGenerator(publicKey, secretKey),
        )
        val token = "pairing-token"
        val endpoint = "tcp://127.0.0.1:7731"
        val server = ServerSocket(0, 1, InetAddress.getByName("127.0.0.1"))
        val brokerExecutor = Executors.newSingleThreadExecutor()
        val brokerReady = CountDownLatch(1)
        val pendingObserved = CountDownLatch(1)
        val socketClosed = CountDownLatch(1)
        val brokerFuture = brokerExecutor.submit {
            brokerReady.countDown()
            server.accept().use { socket ->
                val reader = BufferedReader(
                    InputStreamReader(socket.getInputStream(), StandardCharsets.UTF_8)
                )
                val request = JSONObject(reader.readLine())
                assertEquals(token, request.getString("token"))
                assertEquals(clientKey, request.getString("public_key"))
                assertEquals("android", request.getString("client_kind"))
                val pending = JSONObject()
                    .put("status", "pending")
                    .put("verification_code", PairingProtocol.verificationCode(token, clientKey))
                    .put("device_id", PairingProtocol.deriveDeviceId(clientKey))
                socket.getOutputStream().write(
                    (pending.toString() + "\n").toByteArray(StandardCharsets.UTF_8)
                )
                socket.getOutputStream().flush()
                pendingObserved.countDown()
                socket.soTimeout = 2_000
                if (socket.getInputStream().read() == -1) {
                    socketClosed.countDown()
                }
            }
        }

        val payload = "zara://pair/v1" +
            "?broker_host=127.0.0.1" +
            "&broker_port=${server.localPort}" +
            "&endpoint=${encode(endpoint)}" +
            "&server_key=${encode(serverKey)}" +
            "&token=${encode(token)}" +
            "&expires=${Instant.now().epochSecond + 60}"
        val pinCalls = AtomicInteger(0)
        val connectCalls = AtomicInteger(0)
        val progressObserved = CountDownLatch(1)
        val client = PairingClient(repository, connectTimeoutMs = 1_000)
        val coordinator = AndroidPairingCoordinator(
            pairingClient = client,
            pinServer = { pinCalls.incrementAndGet() },
            connect = {
                connectCalls.incrementAndGet()
                CompletableFuture<ai.zara.app.runtime.ConnectedTextSession>().apply {
                    completeExceptionally(AssertionError("connect must not run after pairing close"))
                }
            },
        )

        try {
            assertTrue(brokerReady.await(2, TimeUnit.SECONDS))
            val result = coordinator.pair(payload) { progress ->
                if (progress is PairingProgress.AwaitingApproval) {
                    progressObserved.countDown()
                }
            }
            // Setup can contend with parallel Gradle workers. Keep the teardown bound below strict.
            awaitPairingSignal("broker pending response", pendingObserved, result)
            awaitPairingSignal("client pending progress", progressObserved, result)

            coordinator.close()
            coordinator.close()

            assertTrue(result.isCancelled)
            assertTrue(socketClosed.await(2, TimeUnit.SECONDS))
            brokerFuture.get(2, TimeUnit.SECONDS)
            assertEquals(0, pinCalls.get())
            assertEquals(0, connectCalls.get())
            assertEquals(null, repository.pinnedServerPublicKeyZ85())
        } finally {
            coordinator.close()
            server.close()
            brokerExecutor.shutdownNow()
        }
    }

    private fun awaitPairingSignal(
        label: String,
        latch: CountDownLatch,
        result: CompletableFuture<*>,
    ) {
        val deadline = System.nanoTime() + TimeUnit.SECONDS.toNanos(10)
        while (System.nanoTime() < deadline) {
            if (latch.await(100, TimeUnit.MILLISECONDS)) return
            if (result.isDone) {
                try {
                    result.join()
                } catch (error: Throwable) {
                    throw AssertionError("pairing terminated before $label", error)
                }
                throw AssertionError("pairing completed before $label")
            }
        }
        throw AssertionError("timed out waiting for $label")
    }

    private fun encode(value: String): String =
        URLEncoder.encode(value, StandardCharsets.UTF_8.name())

    private fun expectFailure(block: () -> Unit) {
        var failed = false
        try {
            block()
        } catch (_: IllegalArgumentException) {
            failed = true
        } catch (_: AuthenticationException) {
            failed = true
        }
        assertTrue(failed)
    }
}

private class CountingGenerator(
    private val publicKey: ByteArray,
    private val secretKey: ByteArray,
) : CurveCredentialGenerator {
    var calls: Int = 0
        private set

    override fun generate(): CurveCredential {
        calls += 1
        return CurveCredential(publicKey, secretKey)
    }
}

private class XorCipher(private val key: Int) : CredentialCipher {
    override fun seal(plaintext: ByteArray): SealedCredential = SealedCredential(
        iv = ByteArray(12) { it.toByte() },
        ciphertext = plaintext.map { (it.toInt() xor key).toByte() }.toByteArray(),
    )

    override fun open(sealed: SealedCredential): ByteArray =
        sealed.ciphertext.map { (it.toInt() xor key).toByte() }.toByteArray()
}
