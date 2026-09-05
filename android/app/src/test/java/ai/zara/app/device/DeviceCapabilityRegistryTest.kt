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
        private val available: Boolean,
    ) : DeviceCapabilityAdapter {
        override fun isAvailable(): Boolean = available
    }
}
