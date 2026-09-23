package ai.zara.app.automation

import ai.zara.app.localai.LocalGenerationRequest
import ai.zara.app.localai.LocalGenerationResult
import ai.zara.app.prolog.AndroidAutomationAction
import ai.zara.app.prolog.AndroidAutomationPlanParser
import java.util.concurrent.CompletableFuture

/**
 * Thin adapter over Android's existing model/provider runtime.
 *
 * This class never constructs a provider client and never reads credentials. The caller supplies
 * AndroidAppSession's already-owned generation/cancellation functions. Provider/model failures are
 * surfaced as a typed unavailable decision rather than triggering a second or remote fallback.
 */
class AndroidCanonicalMultimodalVisionPort(
    private val generate: (LocalGenerationRequest) -> CompletableFuture<LocalGenerationResult>,
    private val cancelGeneration: () -> Unit,
) : CanonicalMultimodalVisionPort {
    override fun interpret(observation: AndroidVisionObservation): CompletableFuture<AndroidVisionDecision> {
        val prompt = buildString {
            append("You are interpreting one Android screenshot for Zara's typed control loop.\n")
            append("Goal: ").append(observation.goal).append("\n")
            append("Return EXACTLY one line and no prose. Allowed forms:\n")
            append("done\n")
            append("actions([adb_tap(X,Y)])\n")
            append("actions([adb_swipe(X1,Y1,X2,Y2,DURATION_MS)])\n")
            append("actions([adb_text('SAFE TEXT')])\n")
            append("actions([adb_key(back|home|enter|recents|tab|escape|delete|up|down|left|right)])\n")
            append("actions([adb_wait(DURATION_MS)])\n")
            append("Choose at most one action. Never emit shell, Intent, package-manager, URI, or executable text.")
        }
        return generate(
            LocalGenerationRequest(
                prompt = prompt,
                maxOutputTokens = 96,
                imagePng = observation.png,
            )
        ).handle { result, error ->
            if (error != null || result == null) {
                AndroidVisionDecision.Unavailable(
                    boundedMessage(error ?: IllegalStateException("multimodal runtime returned no result")),
                )
            } else {
                parseDecision(result.text)
            }
        }
    }

    override fun verify(verification: AndroidVisionVerification): CompletableFuture<Boolean> {
        val prompt = buildString {
            append("Verify the fresh Android screenshot after Zara executed one typed action.\n")
            append("Goal: ").append(verification.goal).append("\n")
            append("Action: ").append(renderAction(verification.action)).append("\n")
            append("Return EXACTLY verified if the intended state change is visible; otherwise return not_verified.")
        }
        return generate(
            LocalGenerationRequest(
                prompt = prompt,
                maxOutputTokens = 16,
                imagePng = verification.png,
            )
        ).handle { result, error ->
            error == null && result != null && result.text.trim().lowercase() == "verified"
        }
    }

    override fun cancel() {
        cancelGeneration()
    }

    private fun parseDecision(text: String): AndroidVisionDecision {
        val normalized = text.trim().removeSuffix(".").trim()
        if (normalized.equals("done", ignoreCase = true)) {
            return AndroidVisionDecision.Done("goal complete")
        }
        val plan = try {
            AndroidAutomationPlanParser.parse("vision_step", normalized)
        } catch (_: RuntimeException) {
            return AndroidVisionDecision.Unavailable("multimodal interpretation did not return a valid typed action")
        }
        val action = plan.actions.singleOrNull()
            ?: return AndroidVisionDecision.Unavailable("multimodal interpretation must return exactly one typed action")
        if (!isClosedAdbAction(action)) {
            return AndroidVisionDecision.Unavailable("multimodal interpretation returned an action outside the ADB vocabulary")
        }
        return AndroidVisionDecision.Act(action)
    }

    private fun isClosedAdbAction(action: AndroidAutomationAction): Boolean = when (action) {
        is AndroidAutomationAction.AdbTap,
        is AndroidAutomationAction.AdbSwipe,
        is AndroidAutomationAction.AdbText,
        is AndroidAutomationAction.AdbKey,
        is AndroidAutomationAction.AdbWait -> true
        else -> false
    }

    private fun renderAction(action: AndroidAutomationAction): String = when (action) {
        is AndroidAutomationAction.AdbTap -> "tap(${action.x},${action.y})"
        is AndroidAutomationAction.AdbSwipe ->
            "swipe(${action.x1},${action.y1},${action.x2},${action.y2},${action.durationMs})"
        is AndroidAutomationAction.AdbText -> "text(<bounded>)"
        is AndroidAutomationAction.AdbKey -> "key(${action.key.name.lowercase()})"
        is AndroidAutomationAction.AdbWait -> "wait(${action.durationMs})"
        else -> "unsupported"
    }

    private fun boundedMessage(error: Throwable): String {
        var current = error
        while (current.cause != null && current.cause !== current) current = current.cause!!
        return (current.message ?: current::class.java.simpleName).take(256)
    }
}
