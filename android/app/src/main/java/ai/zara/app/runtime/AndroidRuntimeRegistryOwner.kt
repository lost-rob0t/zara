package ai.zara.app.runtime

import ai.zara.app.localai.LocalAiPhase
import ai.zara.app.localai.LocalAiState

const val EMBEDDED_LOCAL_RUNTIME_ID = "embedded-local"

/**
 * Android lifecycle owner for the one canonical [RuntimeRegistry].
 *
 * The embedded descriptor is derived from the real local-AI lifecycle. Optional
 * runtimes enter only through discovery sources; this owner never manufactures a
 * product-specific optional row. Auto/Local/Remote routing policy is intentionally
 * outside this class.
 */
class AndroidRuntimeRegistryOwner(
    private val runtimeVersion: String,
    private val implementationVersion: String,
    private val registry: RuntimeRegistry = RuntimeRegistry(),
    private val optionalRuntimeSources: List<() -> RuntimeDescriptor?> = emptyList(),
) {
    fun refresh(localAiState: LocalAiState): RuntimeRegistrySnapshot {
        val embedded = embeddedLocalRuntimeDescriptor(localAiState)
        val observed = buildList {
            add(embedded)
            optionalRuntimeSources.forEach { source ->
                discoverOptional(source)
                    ?.takeUnless { descriptor -> descriptor.id == embedded.id }
                    ?.let(::add)
            }
        }
        return registry.refresh(observed)
    }

    fun snapshot(): RuntimeRegistrySnapshot = registry.snapshot()

    fun select(runtimeId: String): RuntimeSelection = registry.select(runtimeId)

    private fun discoverOptional(source: () -> RuntimeDescriptor?): RuntimeDescriptor? = try {
        source()
    } catch (_: Exception) {
        null
    }

    private fun embeddedLocalRuntimeDescriptor(state: LocalAiState): RuntimeDescriptor {
        val hasModel = state.model != null
        val health = when (state.phase) {
            LocalAiPhase.STOPPED -> RuntimeHealth.STOPPED
            LocalAiPhase.LOADING -> RuntimeHealth.STARTING
            LocalAiPhase.READY -> if (hasModel) RuntimeHealth.READY else RuntimeHealth.DEGRADED
            LocalAiPhase.GENERATING -> if (hasModel) RuntimeHealth.BUSY else RuntimeHealth.DEGRADED
            LocalAiPhase.FAILED -> RuntimeHealth.FAILED
        }
        val available = hasModel && health in setOf(RuntimeHealth.READY, RuntimeHealth.BUSY)

        return RuntimeDescriptor(
            id = EMBEDDED_LOCAL_RUNTIME_ID,
            displayName = "Embedded Local",
            protocol = ZARA_RUNTIME_PROTOCOL,
            runtimeVersion = runtimeVersion,
            implementationVersion = implementationVersion,
            installed = true,
            available = available,
            health = health,
            locality = RuntimeLocality.EMBEDDED,
            transport = RuntimeTransport.IN_PROCESS,
            capabilities = if (hasModel) listOf("chat", "local-model") else emptyList(),
            profiles = if (hasModel) listOf("local-model") else emptyList(),
            providerControl = RuntimeControlOwner.ZARA,
            modelControl = RuntimeControlOwner.ZARA,
            supportsStreaming = true,
            supportsCancel = true,
            supportsContextHandles = false,
            supportsHostTools = false,
            provenance = "android-local-ai:embedded-local",
        )
    }
}
