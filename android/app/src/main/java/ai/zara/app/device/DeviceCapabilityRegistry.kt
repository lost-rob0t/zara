package ai.zara.app.device

import ai.zara.app.runtime.DeviceCapability

interface DeviceCapabilityAdapter {
    val capability: DeviceCapability
    fun isAvailable(): Boolean
}

class DeviceCapabilityUnavailableException(message: String) : IllegalStateException(message)

class DeviceCapabilityRegistry(adapters: List<DeviceCapabilityAdapter>) {
    private val adaptersByCapability: Map<DeviceCapability, DeviceCapabilityAdapter>

    init {
        val duplicate = adapters
            .groupingBy(DeviceCapabilityAdapter::capability)
            .eachCount()
            .entries
            .firstOrNull { it.value > 1 }
        require(duplicate == null) { "duplicate device capability adapter" }
        adaptersByCapability = adapters.associateBy(DeviceCapabilityAdapter::capability)
    }

    fun availableCapabilities(): Set<DeviceCapability> =
        adaptersByCapability
            .entries
            .asSequence()
            .filter { (_, adapter) -> adapter.isAvailable() }
            .map(Map.Entry<DeviceCapability, DeviceCapabilityAdapter>::key)
            .sortedBy(DeviceCapability::wireId)
            .toCollection(linkedSetOf())

    fun canExecute(capability: DeviceCapability): Boolean =
        adaptersByCapability[capability]?.isAvailable() == true

    fun requireAdapter(capability: DeviceCapability): DeviceCapabilityAdapter {
        val adapter = adaptersByCapability[capability]
            ?: throw DeviceCapabilityUnavailableException("device capability is not registered")
        if (!adapter.isAvailable()) {
            throw DeviceCapabilityUnavailableException("device capability is unavailable")
        }
        return adapter
    }
}
