package ai.zara.app.runtime

const val ZARA_RUNTIME_PROTOCOL = "ZARA-RUNTIME/1"

private val runtimeIdPattern = Regex("^[a-z0-9][a-z0-9._-]{0,63}$")
private val protocolPattern = Regex("^ZARA-RUNTIME/[1-9][0-9]*$")
private val provenancePattern = Regex("^[a-z0-9][a-z0-9._-]{0,63}:[a-z0-9][a-z0-9._-]{0,127}$")
private val opaqueRefPattern = Regex("^[a-z][a-z0-9._-]{0,31}:[a-z0-9][a-z0-9._-]{0,127}$")

class RuntimeUnavailable(message: String) : IllegalStateException(message)
class IncompatibleRuntimeProtocol(message: String) : IllegalStateException(message)

enum class RuntimeHealth(val wire: String) {
    STARTING("starting"), READY("ready"), BUSY("busy"), DEGRADED("degraded"),
    FAILED("failed"), STOPPED("stopped");

    companion object {
        fun fromWire(value: String): RuntimeHealth =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("unknown runtime health: $value")
    }
}

enum class RuntimeLocality(val wire: String) {
    EMBEDDED("embedded"), LOCAL_PROCESS("local_process"),
    LOCAL_SIDECAR("local_sidecar"), REMOTE("remote");

    companion object {
        fun fromWire(value: String): RuntimeLocality =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("unknown runtime locality: $value")
    }
}

enum class RuntimeTransport(val wire: String) {
    IN_PROCESS("in_process"), STDIO("stdio"), LOOPBACK_HTTP("loopback_http"),
    BINDER("binder"), ZARA_REMOTE("zara_remote");

    companion object {
        fun fromWire(value: String): RuntimeTransport =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("unknown runtime transport: $value")
    }
}

enum class RuntimeControlOwner(val wire: String) {
    RUNTIME("runtime"), ZARA("zara"), MIXED("mixed");

    companion object {
        fun fromWire(value: String): RuntimeControlOwner =
            entries.firstOrNull { it.wire == value }
                ?: throw IllegalArgumentException("unknown runtime control owner: $value")
    }
}

data class RuntimeDescriptor(
    val id: String,
    val displayName: String,
    val protocol: String,
    val runtimeVersion: String,
    val implementationVersion: String,
    val installed: Boolean,
    val available: Boolean,
    val health: RuntimeHealth,
    val locality: RuntimeLocality,
    val transport: RuntimeTransport,
    val capabilities: List<String> = emptyList(),
    val profiles: List<String> = emptyList(),
    val providerControl: RuntimeControlOwner = RuntimeControlOwner.ZARA,
    val modelControl: RuntimeControlOwner = RuntimeControlOwner.ZARA,
    val supportsStreaming: Boolean = false,
    val supportsCancel: Boolean = false,
    val supportsContextHandles: Boolean = false,
    val supportsHostTools: Boolean = false,
    val provenance: String = "",
) {
    init {
        require(runtimeIdPattern.matches(id)) { "invalid runtime id: $id" }
        requireBoundedText(displayName, "displayName", 128)
        requireBoundedText(protocol, "protocol", 32)
        require(protocolPattern.matches(protocol)) { "invalid runtime protocol: $protocol" }
        requireBoundedText(runtimeVersion, "runtimeVersion", 64)
        requireBoundedText(implementationVersion, "implementationVersion", 64)
        requireBoundedText(provenance, "provenance", 512, allowEmpty = true)
        require(provenance.isEmpty() || provenancePattern.matches(provenance)) {
            "invalid provenance: expected opaque 'source:identity' token"
        }
        requireTokens(capabilities, "capabilities", 64)
        requireTokens(profiles, "profiles", 32)
    }

    val protocolCompatible: Boolean get() = protocol == ZARA_RUNTIME_PROTOCOL

    val selectable: Boolean
        get() = protocolCompatible && installed && available &&
            health != RuntimeHealth.FAILED && health != RuntimeHealth.STOPPED
}

data class RuntimeSelection(val runtimeId: String, val generation: Long)

data class RuntimeInvocationBinding(
    val runtimeId: String,
    val generation: Long,
    val contextRef: String,
    val capabilityRefs: List<String> = emptyList(),
) {
    init {
        require(runtimeIdPattern.matches(runtimeId)) { "invalid runtime id: $runtimeId" }
        require(generation > 0) { "generation must be positive" }
        requireOpaqueRef(contextRef, "context_ref", "ctx:")
        requireOpaqueRefs(capabilityRefs, "capability_refs", "cap:", 64)
    }
}

data class RuntimeRegistrySnapshot(
    val generation: Long,
    val descriptors: List<RuntimeDescriptor>,
    val selection: RuntimeSelection?,
)

class RuntimeRegistry {
    private val lock = Any()
    private var generation = 0L
    private var descriptors: Map<String, RuntimeDescriptor> = emptyMap()
    private var selection: RuntimeSelection? = null

    fun refresh(observed: List<RuntimeDescriptor>): RuntimeRegistrySnapshot {
        val replacement = LinkedHashMap<String, RuntimeDescriptor>()
        observed.forEach { descriptor ->
            require(replacement.putIfAbsent(descriptor.id, descriptor) == null) {
                "duplicate runtime id: ${descriptor.id}"
            }
        }
        return synchronized(lock) {
            if (replacement == descriptors) {
                return@synchronized snapshotLocked()
            }

            val nextGeneration = generation + 1
            val currentId = selection?.runtimeId
            val current = currentId?.let(replacement::get)
            val nextSelection = if (currentId != null && current?.selectable == true) {
                RuntimeSelection(currentId, nextGeneration)
            } else null
            descriptors = replacement.toMap()
            generation = nextGeneration
            selection = nextSelection
            snapshotLocked()
        }
    }

    fun discover(): List<RuntimeDescriptor> = synchronized(lock) { sortedDescriptorsLocked() }

    fun selectable(): List<RuntimeDescriptor> = synchronized(lock) {
        sortedDescriptorsLocked().filter { it.selectable }
    }

    fun health(runtimeId: String): RuntimeHealth = capabilities(runtimeId).health

    fun capabilities(runtimeId: String): RuntimeDescriptor = synchronized(lock) {
        descriptors[runtimeId]
            ?: throw RuntimeUnavailable("runtime '$runtimeId' is not discovered")
    }

    fun select(runtimeId: String): RuntimeSelection {
        if (runtimeId == "auto") {
            throw RuntimeUnavailable("'auto' is routing policy, not a runtime identity")
        }
        return synchronized(lock) {
            val descriptor = descriptors[runtimeId]
                ?: throw RuntimeUnavailable("runtime '$runtimeId' is not discovered")
            if (!descriptor.protocolCompatible) {
                throw IncompatibleRuntimeProtocol(
                    "runtime '$runtimeId' uses incompatible protocol '${descriptor.protocol}'",
                )
            }
            if (!descriptor.selectable) {
                throw RuntimeUnavailable("runtime '$runtimeId' is not selectable")
            }
            generation += 1
            RuntimeSelection(runtimeId, generation).also { selection = it }
        }
    }

    fun current(): RuntimeSelection? = synchronized(lock) { selection }

    fun bindInvocation(
        contextRef: String,
        capabilityRefs: List<String> = emptyList(),
    ): RuntimeInvocationBinding = synchronized(lock) {
        val current = selection ?: throw RuntimeUnavailable("no runtime is selected")
        RuntimeInvocationBinding(
            runtimeId = current.runtimeId,
            generation = current.generation,
            contextRef = contextRef,
            capabilityRefs = capabilityRefs.toList(),
        )
    }

    fun acceptsBinding(binding: RuntimeInvocationBinding, contextRef: String): Boolean {
        if (!isOpaqueRef(contextRef, "ctx:")) return false
        return synchronized(lock) {
            val current = selection
            current != null && binding.runtimeId == current.runtimeId &&
                binding.generation == current.generation && binding.generation == generation &&
                binding.contextRef == contextRef
        }
    }

    fun acceptsGeneration(runtimeId: String, candidateGeneration: Long): Boolean = synchronized(lock) {
        val current = selection
        current != null && current.runtimeId == runtimeId &&
            current.generation == candidateGeneration && candidateGeneration == generation
    }

    fun snapshot(): RuntimeRegistrySnapshot = synchronized(lock) { snapshotLocked() }

    private fun snapshotLocked(): RuntimeRegistrySnapshot = RuntimeRegistrySnapshot(
        generation = generation,
        descriptors = sortedDescriptorsLocked(),
        selection = selection,
    )

    private fun sortedDescriptorsLocked(): List<RuntimeDescriptor> =
        descriptors.values.sortedBy { it.id }
}

private fun requireBoundedText(
    value: String,
    field: String,
    limit: Int,
    allowEmpty: Boolean = false,
) {
    val scalarCount = value.codePointCount(0, value.length)
    require(scalarCount <= limit) { "$field exceeds $limit characters" }
    require(allowEmpty || value.isNotEmpty()) { "$field must not be empty" }
    require(value == value.trim()) { "$field must not contain surrounding whitespace" }
    require(value.none { it.code < 0x20 || it.code == 0x7f }) { "$field contains control characters" }
}

private fun requireTokens(values: List<String>, field: String, limit: Int) {
    require(values.size <= limit) { "$field exceeds $limit entries" }
    require(values.distinct().size == values.size) { "$field contains duplicate entries" }
    values.forEach { value ->
        require(runtimeIdPattern.matches(value)) { "invalid $field entry: $value" }
    }
}

private fun isOpaqueRef(value: String, prefix: String): Boolean =
    value.startsWith(prefix) && opaqueRefPattern.matches(value)

private fun requireOpaqueRef(value: String, field: String, prefix: String) {
    require(isOpaqueRef(value, prefix)) {
        "invalid $field: expected opaque '$prefix' host reference"
    }
}

private fun requireOpaqueRefs(values: List<String>, field: String, prefix: String, limit: Int) {
    require(values.size <= limit) { "$field exceeds $limit entries" }
    require(values.distinct().size == values.size) { "$field contains duplicate entries" }
    values.forEach { value -> requireOpaqueRef(value, field, prefix) }
}
