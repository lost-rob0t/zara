package ai.zara.app.device

import ai.zara.app.runtime.DeviceCapability
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertThrows
import org.junit.Assert.assertTrue
import org.junit.Test

class DeviceCapabilityRegistryTest {
    @Test
    fun `snapshot contains only available registered adapters in deterministic order`() {
        val registry = DeviceCapabilityRegistry(
            listOf(
                FakeAdapter(DeviceCapability.OpenUri, available = true),
                FakeAdapter(DeviceCapability.OpenApp, available = false),
            )
        )

        assertEquals(setOf(DeviceCapability.OpenUri), registry.availableCapabilities())
        assertTrue(registry.canExecute(DeviceCapability.OpenUri))
        assertFalse(registry.canExecute(DeviceCapability.OpenApp))
    }

    @Test
    fun `permission revoked after advertisement cannot execute stale capability`() {
        val adapter = FakeAdapter(DeviceCapability.OpenUri, available = true)
        val registry = DeviceCapabilityRegistry(listOf(adapter))

        assertEquals(setOf(DeviceCapability.OpenUri), registry.availableCapabilities())
        adapter.available = false

        assertFalse(registry.canExecute(DeviceCapability.OpenUri))
        assertEquals(emptySet<DeviceCapability>(), registry.availableCapabilities())
        assertThrows(DeviceCapabilityUnavailableException::class.java) {
            registry.execute(DeviceCapability.OpenUri, DeviceActionArguments.OpenUri("https://example.com"))
        }
        assertEquals(0, adapter.executionCount)
    }

    @Test
    fun `duplicate capability registration fails closed`() {
        assertThrows(IllegalArgumentException::class.java) {
            DeviceCapabilityRegistry(
                listOf(
                    FakeAdapter(DeviceCapability.OpenUri, available = true),
                    FakeAdapter(DeviceCapability.OpenUri, available = true),
                )
            )
        }
    }

    @Test
    fun `unregistered capability cannot execute`() {
        val registry = DeviceCapabilityRegistry(
            listOf(FakeAdapter(DeviceCapability.OpenUri, available = true))
        )

        assertThrows(DeviceCapabilityUnavailableException::class.java) {
            registry.requireAdapter(DeviceCapability.OpenApp)
        }
    }

    private class FakeAdapter(
        override val capability: DeviceCapability,
        var available: Boolean,
    ) : DeviceCapabilityAdapter {
        var executionCount = 0
            private set

        override fun isAvailable(): Boolean = available

        override fun execute(arguments: DeviceActionArguments): DeviceActionResult {
            executionCount += 1
            return DeviceActionResult.Completed
        }
    }
}
