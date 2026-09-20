package ai.zara.app.peer

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertNull
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class PeerNodeIdentityStoreTest {
    private fun tempFile(): File = File.createTempFile("peer-node", ".bin")

    @Test
    fun ensureCreatesCanonicalIdentityOnceAndRestoresIt() {
        val file = tempFile()
        val store = PeerNodeIdentityStore(file)
        val created = store.ensure()

        assertTrue(created.nodeId.matches(Regex("[A-Za-z0-9][A-Za-z0-9._:-]{0,127}")))
        assertTrue(created.nodeId.startsWith("zara-android-"))
        assertEquals(1, created.enrollmentGeneration)

        val reopened = PeerNodeIdentityStore(file).ensure()
        assertEquals(created, reopened)

        file.delete()
    }

    @Test
    fun loadReturnsNullWhenAbsentOrCorrupt() {
        val file = tempFile()
        assertNull(PeerNodeIdentityStore(file).load())

        file.writeBytes("garbage that is not a peer node store".encodeToByteArray())
        assertNull(PeerNodeIdentityStore(file).load())

        file.delete()
    }

    @Test
    fun replaceIsTheOnlyRotationPathAndRequiresCanonicalInput() {
        val file = tempFile()
        val store = PeerNodeIdentityStore(file)
        store.ensure()

        val rotated = store.replace(nodeId = "desktop-paired", enrollmentGeneration = 9)

        assertEquals("desktop-paired", rotated.nodeId)
        assertEquals(9, rotated.enrollmentGeneration)
        assertEquals(rotated, PeerNodeIdentityStore(file).load())

        assertThrows(IllegalArgumentException::class.java) {
            store.replace(nodeId = "bad id", enrollmentGeneration = 2)
        }
        assertThrows(IllegalArgumentException::class.java) {
            store.replace(nodeId = "good-id", enrollmentGeneration = 0)
        }
        assertEquals(rotated, PeerNodeIdentityStore(file).load())

        file.delete()
    }

    @Test
    fun storedFileStaysWithinTheSizeBound() {
        val file = tempFile()
        PeerNodeIdentityStore(file).ensure()

        assertNotNull(file)
        assertTrue(file.length() in 1..(64 * 1024))

        file.delete()
    }
}
