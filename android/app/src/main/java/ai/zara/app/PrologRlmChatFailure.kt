package ai.zara.app

import ai.zara.app.runtime.AssistantRuntimeCancelledException
import ai.zara.app.runtime.AssistantRuntimeStaleGenerationException
import ai.zara.app.runtime.AssistantRuntimeTurnFailedException
import ai.zara.app.runtime.AssistantRuntimeUnavailableException
import java.util.concurrent.CompletionException
import java.util.concurrent.ExecutionException

enum class PrologRlmChatFailureKind {
    CANCELLED,
    STALE_GENERATION,
    TURN_FAILED,
    UNAVAILABLE,
}

data class PrologRlmChatFailure(
    val kind: PrologRlmChatFailureKind,
    val message: String,
    val rediscover: Boolean,
)

internal fun classifyPrologRlmChatFailure(error: Throwable): PrologRlmChatFailure {
    val cause = unwrapPrologRlmFailure(error)
    return when (cause) {
        is AssistantRuntimeCancelledException -> PrologRlmChatFailure(
            kind = PrologRlmChatFailureKind.CANCELLED,
            message = "Prolog-RLM generation was cancelled.",
            rediscover = false,
        )
        is AssistantRuntimeStaleGenerationException -> PrologRlmChatFailure(
            kind = PrologRlmChatFailureKind.STALE_GENERATION,
            message = "This Prolog-RLM turn was discarded because the selected runtime changed.",
            rediscover = false,
        )
        is AssistantRuntimeTurnFailedException -> PrologRlmChatFailure(
            kind = PrologRlmChatFailureKind.TURN_FAILED,
            message = "Prolog-RLM could not complete this turn. The runtime is still available; retry or inspect Diagnostics.",
            rediscover = false,
        )
        is AssistantRuntimeUnavailableException -> PrologRlmChatFailure(
            kind = PrologRlmChatFailureKind.UNAVAILABLE,
            message = "The selected Prolog-RLM runtime is unavailable. Zara rechecked installed runtimes; choose another runtime in Settings → Runtime.",
            rediscover = true,
        )
        else -> PrologRlmChatFailure(
            kind = PrologRlmChatFailureKind.UNAVAILABLE,
            message = "The selected Prolog-RLM runtime is unavailable. Zara rechecked installed runtimes; choose another runtime in Settings → Runtime.",
            rediscover = true,
        )
    }
}

private tailrec fun unwrapPrologRlmFailure(error: Throwable): Throwable {
    val cause = error.cause
    return if (
        cause != null &&
        (error is CompletionException || error is ExecutionException)
    ) {
        unwrapPrologRlmFailure(cause)
    } else {
        error
    }
}
