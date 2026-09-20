package ai.zara.app.peer

private const val MAX_DEVICE_ID_CHARS = 128
private val DEVICE_ID = Regex("[A-Za-z0-9][A-Za-z0-9._:-]{0,127}")

data class EnrolledPeer(
    val deviceId: String,
    val publicKeyZ85: String,
    val enrollmentGeneration: Long,
)

/**
 * Bounded in-memory authority for peers allowed to open authenticated ZARA/1
 * sessions with this node. Identity for authorization is resolved only from the
 * ZAP-authenticated CURVE public key through this registry; payload fields never
 * select a peer. Revocation fences the next message, not the next restart.
 */
class PeerEnrollmentRegistry(
    private val maxPeers: Int = MAX_PEERS,
) {
    private val peers = LinkedHashMap<String, EnrolledPeer>()
    private val keys = HashMap<String, String>()

    @Synchronized
    fun enroll(deviceId: String, publicKeyZ85: String, enrollmentGeneration: Long): EnrolledPeer {
        require(deviceId.length <= MAX_DEVICE_ID_CHARS && DEVICE_ID.matches(deviceId)) {
            "device id must use the bounded canonical peer-id grammar"
        }
        require(publicKeyZ85.length == 40 && publicKeyZ85.none(Char::isWhitespace)) {
            "CURVE public key must be one 40-character Z85 value"
        }
        require(enrollmentGeneration > 0) { "enrollment generation must be positive" }
        check(peers.size < maxPeers) { "peer enrollment registry is full" }
        require(deviceId !in peers) { "device id is already enrolled" }
        require(publicKeyZ85 !in keys) { "CURVE public key is already enrolled" }

        val peer = EnrolledPeer(deviceId, publicKeyZ85, enrollmentGeneration)
        peers[deviceId] = peer
        keys[publicKeyZ85] = deviceId
        return peer
    }

    @Synchronized
    fun revoke(deviceId: String): Boolean {
        val peer = peers.remove(deviceId) ?: return false
        keys.remove(peer.publicKeyZ85)
        return true
    }

    @Synchronized
    fun resolve(publicKeyZ85: String): EnrolledPeer? =
        keys[publicKeyZ85]?.let(peers::getValue)

    companion object {
        const val MAX_PEERS: Int = 256
    }
}
