package ai.zara.app.peer

import java.net.URI

private const val MAX_DISPLAY_NAME_BYTES = 256
private const val MAX_ENDPOINT_BYTES = 512
private const val MAX_ENDPOINTS = 32
private const val MAX_PROTOCOL_VERSIONS = 16
private val PROTOCOL_VERSION = Regex("ZARA/[1-9][0-9]*")
private val DEVICE_CAPABILITIES = setOf("open_app", "open_uri")
private const val Z85_ALPHABET =
    "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ.-:+=^!/*?&<>()[]{}@%$#"

/**
 * Canonical Android projection of ZaraNode for authenticated ZARA/1 peer sessions.
 *
 * This is metadata only. It never selects a principal, grants authorization, owns
 * trust, or creates a transport. CURVE/ZAP enrollment remains the authority and
 * the server must bind this document to that authenticated identity.
 */
class PeerNodeAdvertisement(
    val identity: PeerNodeIdentity,
    val displayName: String,
    endpoints: Collection<String>,
    capabilities: Collection<String>,
    protocolVersions: Collection<String> = setOf("ZARA/1"),
    val lastSeen: Long,
) {
    private val endpointSnapshot: List<String>
    private val capabilitySnapshot: List<String>
    private val protocolSnapshot: List<String>

    init {
        require(displayName.isNotEmpty() && displayName == displayName.trim()) {
            "display name must be a non-empty trimmed string"
        }
        require(displayName.encodeToByteArray().size <= MAX_DISPLAY_NAME_BYTES) {
            "display name exceeds byte limit"
        }
        require(displayName.none { it.code < 0x20 || it.code == 0x7f }) {
            "display name must not contain control characters"
        }
        require(identity.curvePublicKeyZ85.all { it in Z85_ALPHABET }) {
            "CURVE public key must be valid Z85"
        }
        require(endpoints.size <= MAX_ENDPOINTS) { "endpoints exceed item limit" }
        endpointSnapshot = endpoints.map(::normalizeEndpoint)
        require(endpointSnapshot.toSet().size == endpointSnapshot.size) {
            "endpoints contain a duplicate endpoint"
        }

        capabilitySnapshot = capabilities.toList()
        require(capabilitySnapshot.toSet().size == capabilitySnapshot.size) {
            "capabilities contain a duplicate capability"
        }
        require(capabilitySnapshot.all { it in DEVICE_CAPABILITIES }) {
            "capability is not a known device capability"
        }

        protocolSnapshot = protocolVersions.toList()
        require(protocolSnapshot.isNotEmpty()) { "protocol versions must not be empty" }
        require(protocolSnapshot.size <= MAX_PROTOCOL_VERSIONS) {
            "protocol versions exceed item limit"
        }
        require(protocolSnapshot.toSet().size == protocolSnapshot.size) {
            "protocol versions contain a duplicate protocol"
        }
        require(protocolSnapshot.all(PROTOCOL_VERSION::matches)) {
            "protocol version is invalid"
        }
        require(lastSeen >= 0) { "last seen must be non-negative" }
    }

    fun toWireMapping(): Map<String, Any> = linkedMapOf(
        "node_id" to identity.nodeId,
        "display_name" to displayName,
        "device_class" to "android",
        "curve_public_key" to identity.curvePublicKeyZ85,
        "endpoints" to endpointSnapshot.toList(),
        "capabilities" to capabilitySnapshot.sorted(),
        "protocol_versions" to protocolSnapshot.sorted(),
        "last_seen" to lastSeen,
        "enrollment_generation" to identity.enrollmentGeneration,
    )

    /** Body consumed by the existing ZARA/1 hello codec; no second envelope exists. */
    fun toHelloBody(): Map<String, Any> = linkedMapOf(
        "versions" to listOf(1L),
        "node" to toWireMapping(),
    )

    private fun normalizeEndpoint(value: String): String {
        require(value.isNotEmpty() && value == value.trim()) {
            "endpoint must be a non-empty trimmed string"
        }
        require(value.encodeToByteArray().size <= MAX_ENDPOINT_BYTES) {
            "endpoint exceeds byte limit"
        }
        require(value.none { it.code < 0x20 || it.code == 0x7f }) {
            "endpoint must not contain control characters"
        }

        val parsed = try {
            URI(value)
        } catch (error: Exception) {
            throw IllegalArgumentException("endpoint is invalid", error)
        }
        require(parsed.scheme?.equals("tcp", ignoreCase = true) == true) {
            "endpoint must use tcp"
        }
        require(!parsed.host.isNullOrBlank()) { "endpoint host is required" }
        require(parsed.port in 1..65535) { "endpoint port is invalid" }
        require(parsed.userInfo == null) { "endpoint user info is forbidden" }
        require(parsed.path.isNullOrEmpty() || parsed.path == "/") {
            "endpoint path is forbidden"
        }
        require(parsed.query == null && parsed.fragment == null) {
            "endpoint query and fragment are forbidden"
        }
        return value.removeSuffix("/")
    }
}
