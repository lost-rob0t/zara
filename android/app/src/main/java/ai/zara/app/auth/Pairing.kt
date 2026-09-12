package ai.zara.app.auth

import java.io.ByteArrayOutputStream
import java.net.InetSocketAddress
import java.net.URI
import java.net.URLDecoder
import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import java.time.Instant
import java.util.Locale
import javax.crypto.Mac
import javax.crypto.spec.SecretKeySpec
import org.json.JSONObject

private const val PAIRING_VERSION = 1
private const val MAX_PAIRING_MESSAGE_BYTES = 4096
private const val MAX_PAIRING_LIFETIME_SECONDS = 600L
private const val CONNECT_TIMEOUT_MS = 5_000
private const val MIN_READ_TIMEOUT_MS = 1_000

class PairingException(message: String, cause: Throwable? = null) : Exception(message, cause)

data class PairingPayload(
    val brokerHost: String,
    val brokerPort: Int,
    val endpoint: String,
    val serverKey: String,
    val token: String,
    val expiresAtEpochSeconds: Long,
) {
    companion object {
        fun parse(
            raw: String,
            nowEpochSeconds: Long = Instant.now().epochSecond,
        ): PairingPayload {
            val uri = try {
                URI(raw.trim())
            } catch (error: Exception) {
                throw IllegalArgumentException("invalid Zara pairing QR", error)
            }
            require(uri.scheme.equals("zara", ignoreCase = true)) { "pairing QR has wrong scheme" }
            require(uri.host.equals("pair", ignoreCase = true)) { "pairing QR has wrong host" }
            require(uri.path == "/v1") { "pairing QR has unsupported version" }
            require(uri.fragment == null && uri.userInfo == null) { "pairing QR has invalid extras" }

            val query = parseStrictQuery(uri.rawQuery ?: "")
            val required = setOf(
                "broker_host",
                "broker_port",
                "endpoint",
                "server_key",
                "token",
                "expires",
            )
            require(query.keys == required) { "pairing QR has invalid fields" }

            val brokerHost = query.getValue("broker_host")
            require(
                brokerHost.isNotBlank() &&
                    brokerHost.length <= 255 &&
                    brokerHost.none { it.isWhitespace() || it.isISOControl() }
            ) { "pairing broker host is invalid" }

            val brokerPort = query.getValue("broker_port").toIntOrNull()
            require(brokerPort != null && brokerPort in 1..65535) { "pairing broker port is invalid" }

            val endpoint = validateTcpEndpoint(query.getValue("endpoint"))
            val serverKey = query.getValue("server_key")
            JeroMqCurveKeyCodec.decode(serverKey)

            val token = query.getValue("token")
            require(token.length in 8..512 && token.none(Char::isISOControl)) {
                "pairing token is invalid"
            }

            val expires = query.getValue("expires").toLongOrNull()
            require(expires != null) { "pairing expiry is invalid" }
            require(expires > nowEpochSeconds) { "pairing QR has expired" }
            require(expires - nowEpochSeconds <= MAX_PAIRING_LIFETIME_SECONDS) {
                "pairing QR lifetime is invalid"
            }

            return PairingPayload(
                brokerHost = brokerHost,
                brokerPort = brokerPort,
                endpoint = endpoint,
                serverKey = serverKey,
                token = token,
                expiresAtEpochSeconds = expires,
            )
        }

        private fun parseStrictQuery(rawQuery: String): Map<String, String> {
            require(rawQuery.isNotBlank()) { "pairing QR is missing query data" }
            val result = linkedMapOf<String, String>()
            rawQuery.split('&').forEach { field ->
                val parts = field.split('=', limit = 2)
                require(parts.size == 2 && parts[0].isNotBlank()) { "pairing QR query is malformed" }
                val name = decode(parts[0])
                val value = decode(parts[1])
                require(result.put(name, value) == null) { "pairing QR contains duplicate fields" }
            }
            return result
        }

        private fun decode(value: String): String =
            URLDecoder.decode(value, StandardCharsets.UTF_8.name())

        private fun validateTcpEndpoint(raw: String): String {
            val uri = try {
                URI(raw)
            } catch (error: Exception) {
                throw IllegalArgumentException("pairing endpoint is invalid", error)
            }
            require(uri.scheme.equals("tcp", ignoreCase = true)) { "pairing endpoint must use tcp" }
            require(!uri.host.isNullOrBlank()) { "pairing endpoint host is invalid" }
            require(uri.port in 1..65535) { "pairing endpoint port is invalid" }
            require(uri.userInfo == null && uri.query == null && uri.fragment == null) {
                "pairing endpoint has invalid extras"
            }
            require(uri.path.isNullOrBlank() || uri.path == "/") { "pairing endpoint path is invalid" }
            return raw.trimEnd('/')
        }
    }
}

object PairingProtocol {
    fun deriveDeviceId(publicKey: String): String {
        JeroMqCurveKeyCodec.decode(publicKey)
        val digest = MessageDigest.getInstance("SHA-256")
            .digest(publicKey.toByteArray(StandardCharsets.US_ASCII))
        val suffix = digest.take(6).joinToString("") { "%02x".format(it.toInt() and 0xff) }
        return "android-$suffix"
    }

    fun verificationCode(token: String, publicKey: String): String {
        JeroMqCurveKeyCodec.decode(publicKey)
        require(token.isNotEmpty()) { "pairing token must not be empty" }
        val mac = Mac.getInstance("HmacSHA256")
        mac.init(SecretKeySpec(token.toByteArray(StandardCharsets.UTF_8), "HmacSHA256"))
        val digest = mac.doFinal(publicKey.toByteArray(StandardCharsets.US_ASCII))
        val value =
            ((digest[0].toLong() and 0xffL) shl 24) or
                ((digest[1].toLong() and 0xffL) shl 16) or
                ((digest[2].toLong() and 0xffL) shl 8) or
                (digest[3].toLong() and 0xffL)
        return String.format(Locale.US, "%06d", value % 1_000_000L)
    }
}

sealed interface PairingProgress {
    data class AwaitingApproval(
        val verificationCode: String,
        val deviceId: String,
    ) : PairingProgress
}

data class PairingOutcome(
    val endpoint: String,
    val deviceId: String,
)

class PairingClient(
    private val enrollment: EnrollmentRepository,
    private val connectTimeoutMs: Int = CONNECT_TIMEOUT_MS,
) {
    fun pair(
        rawPayload: String,
        onProgress: (PairingProgress) -> Unit = {},
    ): PairingOutcome {
        val payload = PairingPayload.parse(rawPayload)
        val publicKey = enrollment.identityZ85OrCreate()
        val expectedDeviceId = PairingProtocol.deriveDeviceId(publicKey)
        val expectedCode = PairingProtocol.verificationCode(payload.token, publicKey)
        val remainingMillis = (payload.expiresAtEpochSeconds - Instant.now().epochSecond)
            .coerceAtLeast(1L)
            .coerceAtMost(MAX_PAIRING_LIFETIME_SECONDS) * 1000L

        java.net.Socket().use { socket ->
            try {
                socket.connect(
                    InetSocketAddress(payload.brokerHost, payload.brokerPort),
                    connectTimeoutMs,
                )
                socket.soTimeout = remainingMillis.coerceAtLeast(MIN_READ_TIMEOUT_MS.toLong())
                    .coerceAtMost(Int.MAX_VALUE.toLong())
                    .toInt()
                val request = JSONObject()
                    .put("version", PAIRING_VERSION)
                    .put("token", payload.token)
                    .put("public_key", publicKey)
                socket.getOutputStream().write(
                    (request.toString() + "\n").toByteArray(StandardCharsets.UTF_8)
                )
                socket.getOutputStream().flush()

                val pending = readObject(socket.getInputStream())
                if (pending.optString("status") != "pending") {
                    throw PairingException(rejectionMessage(pending))
                }
                requireExactFields(
                    pending,
                    setOf("status", "verification_code", "device_id"),
                    "pending",
                )
                val serverCode = pending.getString("verification_code")
                val serverDeviceId = pending.getString("device_id")
                if (serverCode != expectedCode || serverDeviceId != expectedDeviceId) {
                    throw PairingException("pairing verification response did not match this device")
                }
                onProgress(PairingProgress.AwaitingApproval(serverCode, serverDeviceId))

                val final = readObject(socket.getInputStream())
                if (final.optString("status") != "approved") {
                    throw PairingException(rejectionMessage(final))
                }
                requireExactFields(
                    final,
                    setOf("status", "endpoint", "server_key", "device_id"),
                    "approved",
                )
                if (
                    final.getString("endpoint") != payload.endpoint ||
                    final.getString("server_key") != payload.serverKey ||
                    final.getString("device_id") != expectedDeviceId
                ) {
                    throw PairingException("pairing approval did not match the scanned server")
                }

                enrollment.pinServerZ85(payload.serverKey)
                return PairingOutcome(payload.endpoint, expectedDeviceId)
            } catch (error: PairingException) {
                throw error
            } catch (error: Exception) {
                throw PairingException("Zara pairing failed", error)
            }
        }
    }

    private fun readObject(input: java.io.InputStream): JSONObject {
        val bytes = ByteArrayOutputStream()
        while (bytes.size() <= MAX_PAIRING_MESSAGE_BYTES) {
            val value = input.read()
            if (value == -1) throw PairingException("pairing server closed the connection")
            if (value == '\n'.code) {
                val text = bytes.toString(StandardCharsets.UTF_8.name())
                return try {
                    JSONObject(text)
                } catch (error: Exception) {
                    throw PairingException("pairing server returned invalid JSON", error)
                }
            }
            bytes.write(value)
        }
        throw PairingException("pairing server response exceeded byte limit")
    }

    private fun requireExactFields(value: JSONObject, expected: Set<String>, state: String) {
        val actual = mutableSetOf<String>()
        value.keys().forEachRemaining(actual::add)
        if (actual != expected) throw PairingException("pairing $state response has invalid fields")
    }

    private fun rejectionMessage(value: JSONObject): String {
        val code = value.optString("code").takeIf { it.isNotBlank() } ?: "protocol_failure"
        return when (code) {
            "invalid_token" -> "pairing token was rejected"
            "expired" -> "pairing request expired"
            "operator_rejected" -> "pairing was not approved on the server"
            "enrollment_failed" -> "server could not enroll this device"
            "unsupported_version" -> "Zara pairing versions do not match"
            else -> "pairing failed: $code"
        }
    }
}
