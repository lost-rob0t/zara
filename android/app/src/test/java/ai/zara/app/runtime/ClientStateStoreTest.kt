package ai.zara.app.runtime

import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertNull
import org.junit.Test

class ClientStateStoreTest {

    @Test fun `restore persists only public profile and durable conversation selection`() {
        val root = Files.createTempDirectory("zara-client-state").toFile()
        val file = File(root, "client-state.bin")
        val store = ClientStateStore(file)
        store.save(
            RestorableClientState(
                profile = ServerProfile.create("tcp://zara.example:7731"),
                selectedConversationId = "conversation-7",
            )
        )

        val restored = ClientStateStore(file).load()
        assertEquals("tcp://zara.example:7731", restored?.profile?.endpoint)
        assertEquals("conversation-7", restored?.selectedConversationId)
        assertNull(RuntimeState.fromRestored(restored!!).sessionId)
        assertEquals(ServerConnection.Disconnected, RuntimeState.fromRestored(restored).server)
    }

    @Test fun `corrupt or oversized state fails closed`() {
        val root = Files.createTempDirectory("zara-client-state-corrupt").toFile()
        val file = File(root, "client-state.bin")
        file.writeBytes(ByteArray(70_000) { 0x41 })
        assertNull(ClientStateStore(file).load())

        file.writeText("not-zara-state")
        assertNull(ClientStateStore(file).load())
    }

    @Test fun `saving replaces previous state atomically`() {
        val root = Files.createTempDirectory("zara-client-state-replace").toFile()
        val file = File(root, "client-state.bin")
        val store = ClientStateStore(file)
        store.save(RestorableClientState(ServerProfile.create("tcp://one.example:7731"), null))
        store.save(RestorableClientState(ServerProfile.create("tcp://two.example:7732"), "c2"))

        val restored = store.load()
        assertEquals("tcp://two.example:7732", restored?.profile?.endpoint)
        assertEquals("c2", restored?.selectedConversationId)
        assertEquals(false, File(root, ".client-state.bin.tmp").exists())
    }
}
