package ai.zara.app.peer

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Test

class PeerNodeAdvertisementTest {
    private val identity = PeerNodeIdentity(
        nodeId = "phone-01",
        curvePublicKeyZ85 = "A".repeat(40),
        enrollmentGeneration = 7,
    )

    @Test
    fun `android peer advertisement matches canonical ZaraNode wire fields`() {
        val advertisement = PeerNodeAdvertisement(
            identity = identity,
            displayName = "Alice phone",
            endpoints = listOf("tcp://192.0.2.10:17865/"),
            capabilities = setOf("open_uri", "open_app"),
            protocolVersions = setOf("ZARA/2", "ZARA/1"),
            lastSeen = 42,
        )

        assertEquals(
            setOf(
                "node_id",
                "display_name",
                "device_class",
                "curve_public_key",
                "endpoints",
                "capabilities",
                "protocol_versions",
                "last_seen",
                "enrollment_generation",
            ),
            advertisement.toWireMapping().keys,
        )
        assertEquals("phone-01", advertisement.toWireMapping()["node_id"])
        assertEquals("Alice phone", advertisement.toWireMapping()["display_name"])
        assertEquals("android", advertisement.toWireMapping()["device_class"])
        assertEquals("A".repeat(40), advertisement.toWireMapping()["curve_public_key"])
        assertEquals(
            listOf("tcp://192.0.2.10:17865"),
            advertisement.toWireMapping()["endpoints"],
        )
        assertEquals(listOf("open_app", "open_uri"), advertisement.toWireMapping()["capabilities"])
        assertEquals(listOf("ZARA/1", "ZARA/2"), advertisement.toWireMapping()["protocol_versions"])
        assertEquals(42L, advertisement.toWireMapping()["last_seen"])
        assertEquals(7L, advertisement.toWireMapping()["enrollment_generation"])
        assertEquals(
            mapOf(
                "versions" to listOf(1L),
                "node" to advertisement.toWireMapping(),
            ),
            advertisement.toHelloBody(),
        )
    }

    @Test
    fun `advertisement rejects authorization capability injection`() {
        assertThrows(IllegalArgumentException::class.java) {
            PeerNodeAdvertisement(
                identity = identity,
                displayName = "Alice phone",
                endpoints = emptyList(),
                capabilities = setOf("daemon.admin"),
                lastSeen = 1,
            )
        }
    }

    @Test
    fun `advertisement rejects non Zara TCP routes and ambiguous endpoints`() {
        val rejected = listOf(
            "https://example.invalid/zara",
            "tcp://user@example.invalid:17865",
            "tcp://example.invalid:17865/path",
            "tcp://example.invalid:17865?route=peer",
            "tcp://example.invalid:17865#peer",
            "tcp://example.invalid:0",
            "tcp://example.invalid:65536",
        )
        rejected.forEach { endpoint ->
            assertThrows(IllegalArgumentException::class.java) {
                PeerNodeAdvertisement(
                    identity = identity,
                    displayName = "Alice phone",
                    endpoints = listOf(endpoint),
                    capabilities = emptySet(),
                    lastSeen = 1,
                )
            }
        }

        assertThrows(IllegalArgumentException::class.java) {
            PeerNodeAdvertisement(
                identity = identity,
                displayName = "Alice phone",
                endpoints = listOf(
                    "tcp://example.invalid:17865",
                    "tcp://example.invalid:17865/",
                ),
                capabilities = emptySet(),
                lastSeen = 1,
            )
        }
    }

    @Test
    fun `advertisement validates display protocol and counter bounds`() {
        assertThrows(IllegalArgumentException::class.java) {
            PeerNodeAdvertisement(
                identity = identity,
                displayName = " bad ",
                endpoints = emptyList(),
                capabilities = emptySet(),
                lastSeen = 1,
            )
        }
        assertThrows(IllegalArgumentException::class.java) {
            PeerNodeAdvertisement(
                identity = identity,
                displayName = "Alice phone",
                endpoints = emptyList(),
                capabilities = emptySet(),
                protocolVersions = emptySet(),
                lastSeen = 1,
            )
        }
        assertThrows(IllegalArgumentException::class.java) {
            PeerNodeAdvertisement(
                identity = identity,
                displayName = "Alice phone",
                endpoints = emptyList(),
                capabilities = emptySet(),
                protocolVersions = setOf("HTTP/1"),
                lastSeen = 1,
            )
        }
        assertThrows(IllegalArgumentException::class.java) {
            PeerNodeAdvertisement(
                identity = identity,
                displayName = "Alice phone",
                endpoints = emptyList(),
                capabilities = emptySet(),
                lastSeen = -1,
            )
        }
    }

    @Test
    fun `advertisement snapshots mutable caller collections`() {
        val endpoints = mutableListOf("tcp://192.0.2.10:17865")
        val capabilities = mutableSetOf("open_uri")
        val protocols = mutableSetOf("ZARA/1")
        val advertisement = PeerNodeAdvertisement(
            identity = identity,
            displayName = "Alice phone",
            endpoints = endpoints,
            capabilities = capabilities,
            protocolVersions = protocols,
            lastSeen = 1,
        )

        endpoints += "tcp://198.51.100.20:17865"
        capabilities += "open_app"
        protocols += "ZARA/2"

        assertEquals(
            listOf("tcp://192.0.2.10:17865"),
            advertisement.toWireMapping()["endpoints"],
        )
        assertEquals(listOf("open_uri"), advertisement.toWireMapping()["capabilities"])
        assertEquals(listOf("ZARA/1"), advertisement.toWireMapping()["protocol_versions"])
    }
}
