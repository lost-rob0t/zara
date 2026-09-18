package ai.zara.app.policy

import ai.zara.app.localai.LocalGenerationRequest
import ai.zara.app.localai.LocalGenerationResult
import java.util.concurrent.CompletableFuture

class LocalPolicyModel(
    private val generate: (LocalGenerationRequest) -> CompletableFuture<LocalGenerationResult>,
    private val inspect: (String) -> CompletableFuture<PolicyAdvice>,
    private val observe: (PolicyOutcome) -> Unit = {},
) {
    fun answer(prompt: String): CompletableFuture<String> {
        var first: LocalGenerationResult? = null
        return PolicyReview(
            generate = { text ->
                val source = generate(LocalGenerationRequest(text, maxOutputTokens = 256))
                val result = CompletableFuture<String>()
                result.whenComplete { _, _ -> if (result.isCancelled) source.cancel(true) }
                source.whenComplete { generated, error ->
                    if (!result.isDone) {
                        if (error != null) result.completeExceptionally(error)
                        else try {
                            val original = first
                            if (original == null) first = generated
                            else check(original.modelId == generated.modelId &&
                                original.modelVersion == generated.modelVersion &&
                                original.quantization == generated.quantization &&
                                original.generation == generated.generation) {
                                "Local model changed during policy review"
                            }
                            result.complete(generated.text)
                        } catch (failure: Exception) { result.completeExceptionally(failure) }
                    }
                }
                result
            },
            inspect = inspect,
            observe = observe,
        ).answer(prompt)
    }
}
