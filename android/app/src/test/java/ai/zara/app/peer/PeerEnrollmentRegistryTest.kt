package ai.zara.app.peer

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class PeerEnrollmentRegistryTest {
    @Test
    fun unknownKeyIsDeniedByDefault() {
        val registry = PeerEnrollmentRegistry()

        assertNull(registry.resolve("B".repeat(40)))
    }

    @Test
    fun enrolledPeerResolvesByPublicKey() {
        val registry = PeerEnrollmentRegistry()
        val key = "K".repeat(40)

        val enrolled = registry.enroll(
            deviceId = "desktop-01",
            publicKeyZ85 = key,
            enrollmentGeneration = 3,
        )

        assertEquals("desktop-01", enrolled.deviceId)
        assertEquals(key, enrolled.publicKeyZ85)
        assertEquals(3, enrolled.enrollmentGeneration)
        assertEquals(enrolled, registry.resolve(key))
    }

    @Test
    fun revokedPeerFailsClosedImmediately() {
        val registry = PeerEnrollmentRegistry()
        val key = "K".repeat(40)
        registry.enroll("desktop-01", key, 3)

        assertTrue(registry.revoke("desktop-01"))

        assertNull(registry.resolve(key))
        assertFalse(registry.revoke("desktop-01"))
    }

    @Test
    fun revokingUnknownDeviceIsRejected() {
        val registry = PeerEnrollmentRegistry()

        assertFalse(registry.revoke("desktop-01"))
    }

    @Test
    fun duplicateActiveDeviceOrKeyIsRejected() {
        val registry = PeerEnrollmentRegistry()
        registry.enroll("desktop-01", "K".repeat(40), 3)

        assertThrows(IllegalArgumentException::class.java) {
            registry.enroll("desktop-01", "J".repeat(40), 4)
        }
        assertThrows(IllegalArgumentException::class.java) {
            registry.enroll("desktop-02", "K".repeat(40), 4)
        }
    }

    @Test
    fun reEnrollAfterRevokeStartsAFreshGeneration() {
        val registry = PeerEnrollmentRegistry()
        registry.enroll("desktop-01", "K".repeat(40), 3)
        registry.revoke("desktop-01")

        val reEnrolled = registry.enroll("desktop-01", "J".repeat(40), 9)

        assertEquals(reEnrolled, registry.resolve("J".repeat(40)))
        assertNull(registry.resolve("K".repeat(40)))
    }

    @Test
    fun enrollmentInputIsBoundedAndCanonical() {
        val registry = PeerEnrollmentRegistry()

        assertThrows(IllegalArgumentException::class.java) {
            registry.enroll("desktop 01", "K".repeat(40), 3)
        }
        assertThrows(IllegalArgumentException::class.java) {
            registry.enroll("x".repeat(129), "K".repeat(40), 3)
        }
        assertThrows(IllegalArgumentException::class.java) {
            registry.enroll("desktop-01", "short", 3)
        }
        assertThrows(IllegalArgumentException::class.java) {
            registry.enroll("desktop-01", "K".repeat(39), 3)
        }
        assertThrows(IllegalArgumentException::class.java) {
            registry.enroll("desktop-01", "K".repeat(40), 0)
        }
    }

    @Test
    fun registryIsBounded() {
        val registry = PeerEnrollmentRegistry()
        repeat(PeerEnrollmentRegistry.MAX_PEERS) { index ->
            val digits = index.toString()
            registry.enroll("desktop-$index", "K".repeat(40 - digits.length) + digits, 1)
        }

        assertThrows(IllegalStateException::class.java) {
            registry.enroll("overflow", "Z".repeat(40), 1)
        }
    }
}
