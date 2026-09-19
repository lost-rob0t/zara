package ai.zara.app.runtime

import ai.zara.app.localai.LocalAiPhase
import ai.zara.app.localai.LocalAiState

/**
 * Projects the canonical local-model lifecycle into the backend-neutral runtime UI contract.
 *
 * This is deliberately a projection only: LocalAiState remains the authority for the embedded
 * model lifecycle and AssistantRuntimeRegistry remains the runtime selection authority on this
 * branch. The UI must never synthesize a READY runtime independently of LocalAiState.
 */
fun embeddedLocalRuntimeDescriptor(state: LocalAiState): AssistantRuntimeDescriptor {
    val model = state.model
    val health = when (state.phase) {
        LocalAiPhase.STOPPED -> "stopped"
        LocalAiPhase.LOADING -> "starting"
        LocalAiPhase.READY -> if (model == null) "degraded" else "ready"
        LocalAiPhase.GENERATING -> if (model == null) "degraded" else "busy"
        LocalAiPhase.FAILED -> "failed"
    }
    return AssistantRuntimeDescriptor(
        id = EMBEDDED_LOCAL_RUNTIME_ID,
        displayName = "Embedded Local",
        runtimeVersion = model?.version ?: "unloaded",
        health = health,
        locality = "embedded",
        transport = "in_process",
        profiles = emptyList(),
        supportsStreaming = false,
        supportsCancel = false,
    )
}

/** Replace exactly one embedded-local observation while preserving canonical discovery order. */
fun projectEmbeddedLocalRuntime(
    runtimes: List<AssistantRuntimeDescriptor>,
    state: LocalAiState,
): List<AssistantRuntimeDescriptor> {
    require(runtimes.count { it.id == EMBEDDED_LOCAL_RUNTIME_ID } == 1) {
        "Embedded local runtime discovery identity is missing or duplicated"
    }
    val projected = embeddedLocalRuntimeDescriptor(state)
    return runtimes.map { runtime ->
        if (runtime.id == EMBEDDED_LOCAL_RUNTIME_ID) projected else runtime
    }
}

/**
 * Fail closed when LocalAiState itself cannot be observed. Optional sidecars remain visible, but
 * the embedded runtime must not inherit a stale synthetic READY state from discovery plumbing.
 */
fun failClosedEmbeddedLocalRuntime(
    runtimes: List<AssistantRuntimeDescriptor>,
): List<AssistantRuntimeDescriptor> = runtimes.map { runtime ->
    if (runtime.id == EMBEDDED_LOCAL_RUNTIME_ID) {
        runtime.copy(runtimeVersion = "unavailable", health = "failed")
    } else {
        runtime
    }
}
