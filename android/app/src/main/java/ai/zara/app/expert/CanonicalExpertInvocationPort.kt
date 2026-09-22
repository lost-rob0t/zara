package ai.zara.app.expert

import java.util.concurrent.CompletableFuture

/**
 * Consumer-only seam into Zara's existing canonical ZARA-EXPERT/1 owner.
 *
 * Implementations must delegate to the already-existing activation / registered-predicate
 * authority. This interface intentionally cannot register, activate, grant capabilities, execute
 * raw Prolog goals, or own lifecycle state. A missing active handle is a fail-closed condition.
 */
interface CanonicalExpertInvocationPort {
    fun activeActivation(
        principal: String,
        workspace: String,
        expertId: String,
    ): ActivationHandle?

    fun invoke(request: ExpertRequest): CompletableFuture<ExpertResult>

    fun currentRegistryGeneration(): Long

    fun currentRuntimeGeneration(): Long
}

/**
 * Pure-symbolic request/result adapter around the canonical owner.
 *
 * This owns no registry, executor, provider runtime, permission state, budget ledger, or history.
 * It consumes an already-issued activation, constructs the canonical zero-model request through
 * [PureSymbolicExpertAdmission], invokes through [CanonicalExpertInvocationPort], and validates
 * the returned result against the owner's live generations before conversation projection.
 */
class PureSymbolicExpertInvocationAdapter(
    private val port: CanonicalExpertInvocationPort,
) {
    fun invoke(
        principal: String,
        workspace: String,
        expertId: String,
        requestId: String,
        expertOperation: String,
        input: Map<String, Any?>,
        limits: ExpertLimits,
        idempotencyKey: String,
    ): CompletableFuture<ExpertResult> {
        if (limits.maxModelCalls != 0) {
            return CompletableFuture.failedFuture(
                IllegalArgumentException(
                    "Pure-symbolic expert invocation requires maxModelCalls=0",
                ),
            )
        }

        val activation = port.activeActivation(
            principal = principal,
            workspace = workspace,
            expertId = expertId,
        ) ?: return CompletableFuture.failedFuture(
            IllegalStateException("Canonical expert activation is unavailable"),
        )

        if (
            activation.principal != principal ||
            activation.workspace != workspace ||
            activation.expertId != expertId
        ) {
            return CompletableFuture.failedFuture(
                IllegalArgumentException("Canonical expert activation identity does not match the request scope"),
            )
        }

        try {
            val live = stableLiveGenerations("before invocation")
            require(activation.registryGeneration == live.registryGeneration) {
                "activation registry generation is stale before invocation"
            }
            require(activation.runtimeGeneration == live.runtimeGeneration) {
                "activation runtime generation is stale before invocation"
            }
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }

        val request = try {
            PureSymbolicExpertAdmission.request(
                activation = activation,
                requestId = requestId,
                expertOperation = expertOperation,
                input = input,
                limits = limits,
                idempotencyKey = idempotencyKey,
            )
        } catch (error: Throwable) {
            return CompletableFuture.failedFuture(error)
        }

        return try {
            val ownerFuture = port.invoke(request)
            val admittedFuture = ownerFuture.thenApply { result ->
                val live = stableLiveGenerations("after invocation")
                PureSymbolicExpertAdmission.validateResult(
                    activation = activation,
                    request = request,
                    result = result,
                    currentRegistryGeneration = live.registryGeneration,
                    currentRuntimeGeneration = live.runtimeGeneration,
                )
            }
            admittedFuture.whenComplete { _, _ ->
                if (admittedFuture.isCancelled && !ownerFuture.isDone) {
                    ownerFuture.cancel(true)
                }
            }
            admittedFuture
        } catch (error: Throwable) {
            CompletableFuture.failedFuture(error)
        }
    }

    /**
     * The canonical owner currently exposes registry/runtime generations as separate reads. Sample
     * the pair twice and require stability so a generation change between those reads cannot make a
     * stale activation look current. This is only a consumer-side fence; authority remains with the
     * canonical owner and its expected-generation checks.
     */
    private fun stableLiveGenerations(phase: String): LiveGenerations {
        val first = readLiveGenerations()
        val second = readLiveGenerations()
        require(first == second) {
            "Canonical expert generations changed during $phase"
        }
        return second
    }

    private fun readLiveGenerations(): LiveGenerations {
        val registryGeneration = port.currentRegistryGeneration()
        val runtimeGeneration = port.currentRuntimeGeneration()
        require(registryGeneration >= 0L) {
            "current registry generation must be non-negative"
        }
        require(runtimeGeneration >= 0L) {
            "current runtime generation must be non-negative"
        }
        return LiveGenerations(
            registryGeneration = registryGeneration,
            runtimeGeneration = runtimeGeneration,
        )
    }

    private data class LiveGenerations(
        val registryGeneration: Long,
        val runtimeGeneration: Long,
    )
}
