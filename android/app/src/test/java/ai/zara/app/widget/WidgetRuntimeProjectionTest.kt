package ai.zara.app.widget

import ai.zara.app.runtime.LocalServerPhase
import ai.zara.app.runtime.RuntimeMode
import ai.zara.app.runtime.ServerConnection
import java.io.File
import java.nio.file.Files
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import org.junit.Test

class WidgetRuntimeProjectionTest {
    @Test
    fun `runtime snapshot uses honest bounded lifecycle labels`() {
        val snapshot = WidgetRuntimeSnapshot.from(
            server = ServerConnection.Reconnecting(7, 3),
            local = LocalServerPhase.RELOADING,
            mode = RuntimeMode.Auto,
            capturedAtEpochMillis = 42,
        )

        assertEquals("RECONNECTING 3", snapshot.remote)
        assertEquals("LOCAL RELOADING", snapshot.local)
        assertEquals("AUTO", snapshot.mode)
        assertEquals(42, snapshot.capturedAtEpochMillis)
    }

    @Test
    fun `snapshot store round trips and rejects corrupt oversized state`() {
        val root = Files.createTempDirectory("zara-widget-runtime").toFile()
        val file = File(root, "runtime.bin")
        val store = WidgetRuntimeSnapshotStore(file)
        val snapshot = WidgetRuntimeSnapshot("CONNECTED", "LOCAL READY", "REMOTE", 91)
        store.save(snapshot)
        assertEquals(snapshot, WidgetRuntimeSnapshotStore(file).load())

        file.writeBytes(ByteArray(9_000) { 1 })
        assertEquals(WidgetRuntimeSnapshot.unknown(), store.load())
    }

    @Test
    fun `all public runtime states have a finite widget projection`() {
        val states = listOf(
            ServerConnection.Disconnected,
            ServerConnection.Connecting(1),
            ServerConnection.Connected(1),
            ServerConnection.Reconnecting(2, 1),
            ServerConnection.OfflineDegraded(5, "private failure text must not leak"),
        )
        states.forEach { state ->
            val snapshot = WidgetRuntimeSnapshot.from(state, LocalServerPhase.READY, RuntimeMode.Local, 1)
            assertTrue(snapshot.remote.length in 1..32)
            assertTrue("failure detail leaked", !snapshot.remote.contains("private"))
        }
    }
}
