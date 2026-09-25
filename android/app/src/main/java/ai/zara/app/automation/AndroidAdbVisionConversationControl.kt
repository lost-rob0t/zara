package ai.zara.app.automation

import ai.zara.app.AndroidAppSession
import ai.zara.app.localai.LocalAiServiceClient
import ai.zara.app.localai.LocalAiUnavailableException
import ai.zara.app.localai.LocalGenerationRequest
import ai.zara.app.localai.LocalGenerationResult
import ai.zara.app.prolog.AndroidAutomationAction
import ai.zara.app.runtime.RuntimeMode
import android.content.Context
import java.util.concurrent.CompletableFuture

/**
 * Production assembly for #1394.
 *
 * The ADB adapter remains transport-only. Screenshot interpretation binds to the same Android
 * LocalAiService provider runtime used by AndroidAppSession; no provider SDK/client or credential is
 * introduced here. The session's existing local Prolog runtime remains the policy authority.
 *
 * Remote-only mode intentionally returns typed unavailable until ZARA/1 context attachments carry
 * binary image content. Auto mode may use the already-selected local image-capable model, but never
 * silently falls from an unsupported local image turn to a remote provider.
 */
class AndroidAdbVisionConversationControl(
    context: Context,
    private val appSession: AndroidAppSession,
    requestApproval: (AndroidAutomationAction) -> CompletableFuture<Boolean>,
    limits: AndroidVisionLoopLimits = AndroidVisionLoopLimits(),
) : AutoCloseable {
    private val localAi = LocalAiServiceClient(context)
    private val adb = AndroidAdbAutomationAdapter(context)
    private val multimodal = AndroidCanonicalMultimodalVisionPort(
        generate = ::generateWithExistingRuntime,
        cancelGeneration = { localAi.cancelGeneration() },
    )
    private val authority = object : AndroidVisionActionAuthority {
        override fun canExecute(action: AndroidAutomationAction): Boolean = adb.isAvailable()
        override fun approve(action: AndroidAutomationAction): CompletableFuture<Boolean> =
            requestApproval(action)
    }
    private val loop = AndroidAdbVisionControlLoop(
        adb = adb,
        multimodal = multimodal,
        queryProlog = appSession::queryLocalProlog,
        authority = authority,
        limits = limits,
    )

    fun run(goal: String): CompletableFuture<AndroidVisionLoopResult> = loop.run(goal)

    fun cancel() = loop.cancel()

    override fun close() {
        loop.cancel()
        localAi.close()
    }

    private fun generateWithExistingRuntime(
        request: LocalGenerationRequest,
    ): CompletableFuture<LocalGenerationResult> = when (appSession.runtimeMode()) {
        RuntimeMode.Remote -> failed(
            LocalAiUnavailableException(
                "Android remote multimodal transport is unavailable; Zara did not fall back to another provider",
            )
        )
        RuntimeMode.Local,
        RuntimeMode.Auto -> localAi.generate(request)
    }

    private fun <T> failed(error: Throwable): CompletableFuture<T> =
        CompletableFuture<T>().also { it.completeExceptionally(error) }
}
