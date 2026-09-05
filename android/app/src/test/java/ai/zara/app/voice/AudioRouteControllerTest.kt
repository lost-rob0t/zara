package ai.zara.app.voice

import org.junit.Assert.assertEquals
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class AudioRouteControllerTest {
    @Test
    fun `route change publishes state and interrupts active playback once`() {
        val platform = FakeAudioRoutePlatform(AudioRouteSnapshot(setOf(AudioRouteKind.BuiltIn)))
        val changes = mutableListOf<AudioRouteSnapshot>()
        val interruptions = mutableListOf<Pair<AudioRouteSnapshot, AudioRouteSnapshot>>()
        val controller = AudioRouteController(
            platform = platform,
            onChanged = changes::add,
            onRouteInterrupted = { previous, current -> interruptions += previous to current },
        )

        controller.start()
        platform.emit(AudioRouteSnapshot(setOf(AudioRouteKind.Bluetooth)))
        platform.emit(AudioRouteSnapshot(setOf(AudioRouteKind.Bluetooth)))

        assertEquals(AudioRouteSnapshot(setOf(AudioRouteKind.Bluetooth)), controller.current())
        assertEquals(
            listOf(AudioRouteSnapshot(setOf(AudioRouteKind.BuiltIn)), AudioRouteSnapshot(setOf(AudioRouteKind.Bluetooth))),
            changes,
        )
        assertEquals(1, interruptions.size)
        assertEquals(AudioRouteKind.BuiltIn, interruptions.single().first.outputs.single())
        assertEquals(AudioRouteKind.Bluetooth, interruptions.single().second.outputs.single())
    }

    @Test
    fun `stopped controller ignores late platform callbacks`() {
        val platform = FakeAudioRoutePlatform(AudioRouteSnapshot(setOf(AudioRouteKind.Wired)))
        val changes = mutableListOf<AudioRouteSnapshot>()
        val controller = AudioRouteController(
            platform = platform,
            onChanged = changes::add,
            onRouteInterrupted = { _, _ -> error("stopped controller must not interrupt") },
        )

        controller.start()
        controller.stop()
        platform.emit(AudioRouteSnapshot(setOf(AudioRouteKind.Bluetooth)))

        assertTrue(platform.stopped)
        assertEquals(listOf(AudioRouteSnapshot(setOf(AudioRouteKind.Wired))), changes)
        assertEquals(null, controller.current())
    }

    @Test
    fun `failed platform start rolls back controller lifecycle`() {
        val platform = FakeAudioRoutePlatform(AudioRouteSnapshot(setOf(AudioRouteKind.BuiltIn)))
        platform.failStart = true
        val controller = AudioRouteController(
            platform = platform,
            onChanged = {},
            onRouteInterrupted = { _, _ -> },
        )

        assertThrows(IllegalStateException::class.java) { controller.start() }
        assertEquals(null, controller.current())

        platform.failStart = false
        controller.start()
        assertEquals(AudioRouteSnapshot(setOf(AudioRouteKind.BuiltIn)), controller.current())
    }

    private class FakeAudioRoutePlatform(initial: AudioRouteSnapshot) : AudioRoutePlatform {
        private var snapshot = initial
        private var listener: ((AudioRouteSnapshot) -> Unit)? = null
        var stopped = false
            private set
        var failStart = false

        override fun snapshot(): AudioRouteSnapshot = snapshot

        override fun start(onChanged: (AudioRouteSnapshot) -> Unit) {
            check(!failStart) { "synthetic route monitor failure" }
            check(listener == null)
            listener = onChanged
        }

        override fun stop() {
            stopped = true
            listener = null
        }

        fun emit(next: AudioRouteSnapshot) {
            snapshot = next
            listener?.invoke(next)
        }
    }
}
