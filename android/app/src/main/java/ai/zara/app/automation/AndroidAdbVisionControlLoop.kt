package ai.zara.app.automation

import ai.zara.app.device.DeviceActionErrorCode
import ai.zara.app.device.DeviceActionResult
import ai.zara.app.prolog.AndroidAutomationAction
import ai.zara.app.runtime.LocalQueryResult
import java.util.concurrent.CompletableFuture
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicLong

private const val MAX_SINGLE_SCREENSHOT_BYTES = 16 * 1024 * 1024
private val PNG_MAGIC = byteArrayOf(
    0x89.toByte(), 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
)
private val VISION_TIMEOUT_EXECUTOR: ScheduledExecutorService =
    Executors.newSingleThreadScheduledExecutor { runnable ->
        Thread(runnable, "zara-android-vision-timeout").apply { isDaemon = true }
    }

data class AndroidVisionLoopLimits(
    val maxSteps: Int = 8,
    val maxObservedBytes: Long = 64L * 1024 * 1024,
    val timeoutMillis: Long = 30_000,
) {
    init {
        require(maxSteps in 1..32) { "vision loop step limit must be between 1 and 32" }
        require(maxObservedBytes in 1..(256L * 1024 * 1024)) { "vision loop byte limit is invalid" }
        require(timeoutMillis in 1..120_000) { "vision loop timeout is invalid" }
    }
}

data class AndroidVisionObservation(
    val goal: String,
    val png: ByteArray,
    val sequence: Long,
)

data class AndroidVisionVerification(
    val goal: String,
    val action: AndroidAutomationAction,
    val png: ByteArray,
    val sequence: Long,
)

sealed interface AndroidVisionDecision {
    data class Act(val action: AndroidAutomationAction) : AndroidVisionDecision
    data class Done(val summary: String) : AndroidVisionDecision
    data class Unavailable(val reason: String) : AndroidVisionDecision
}

interface CanonicalMultimodalVisionPort {
    fun interpret(observation: AndroidVisionObservation): CompletableFuture<AndroidVisionDecision>
    fun verify(verification: AndroidVisionVerification): CompletableFuture<Boolean>
    fun cancel()
}

interface AndroidVisionActionAuthority {
    fun canExecute(action: AndroidAutomationAction): Boolean
    fun approve(action: AndroidAutomationAction): CompletableFuture<Boolean>
}

sealed interface AndroidVisionLoopResult {
    data class Completed(
        val summary: String,
        val steps: Int,
        val observedBytes: Long,
    ) : AndroidVisionLoopResult

    data class CapabilityUnavailable(val action: AndroidAutomationAction) : AndroidVisionLoopResult
    data class ApprovalRejected(val action: AndroidAutomationAction) : AndroidVisionLoopResult
    data class PolicyRejected(val action: AndroidAutomationAction) : AndroidVisionLoopResult
    data class VerificationFailed(val action: AndroidAutomationAction) : AndroidVisionLoopResult
    data class ActionFailed(val action: AndroidAutomationAction, val message: String?) : AndroidVisionLoopResult
    data class Unavailable(val reason: String) : AndroidVisionLoopResult
    data class BoundsExceeded(val reason: String) : AndroidVisionLoopResult
    data class Failed(val reason: String) : AndroidVisionLoopResult
    data object Cancelled : AndroidVisionLoopResult
}

/**
 * Bounded observe -> canonical multimodal interpretation -> Prolog policy -> approved typed action
 * -> fresh screenshot verification loop.
 *
 * This coordinator intentionally knows no provider credentials and exposes no raw ADB shell surface.
 * The multimodal port is supplied by Android's existing provider runtime; effects are the closed
 * [AdbAutomationPort] vocabulary from #1393. Every mutation is fenced by generation, capability,
 * Prolog policy, user approval, and fresh postcondition evidence.
 */
class AndroidAdbVisionControlLoop(
    private val adb: AdbAutomationPort,
    private val multimodal: CanonicalMultimodalVisionPort,
    private val queryProlog: (String) -> CompletableFuture<LocalQueryResult>,
    private val authority: AndroidVisionActionAuthority,
    private val limits: AndroidVisionLoopLimits = AndroidVisionLoopLimits(),
    private val nanoTime: () -> Long = System::nanoTime,
) {
    private val generation = AtomicLong(0)
    private val effectFence = Any()

    fun run(goal: String): CompletableFuture<AndroidVisionLoopResult> {
        val normalizedGoal = goal.trim()
        require(normalizedGoal.isNotEmpty()) { "vision goal is required" }
        require(normalizedGoal.length <= 2_048) { "vision goal is too long" }
        val runGeneration = synchronized(effectFence) { generation.incrementAndGet() }
        val deadlineNanos = nanoTime() + TimeUnit.MILLISECONDS.toNanos(limits.timeoutMillis)
        if (!adb.isAvailable()) {
            return completed(
                AndroidVisionLoopResult.Unavailable("authorized Android ADB target is unavailable"),
            )
        }
        return observe(normalizedGoal, sequence = 0, observedBytes = 0)
            .thenCompose { observed ->
                when (observed) {
                    is ObservationResult.Error -> completed(observed.result)
                    is ObservationResult.Ok -> step(
                        goal = normalizedGoal,
                        runGeneration = runGeneration,
                        deadlineNanos = deadlineNanos,
                        observation = observed.observation,
                        steps = 0,
                        observedBytes = observed.observedBytes,
                    )
                }
            }
    }

    fun cancel() {
        multimodal.cancel()
        synchronized(effectFence) {
            generation.incrementAndGet()
        }
    }

    private fun step(
        goal: String,
        runGeneration: Long,
        deadlineNanos: Long,
        observation: AndroidVisionObservation,
        steps: Int,
        observedBytes: Long,
    ): CompletableFuture<AndroidVisionLoopResult> {
        currentFailure(runGeneration, deadlineNanos)?.let { return completed(it) }
        if (steps >= limits.maxSteps) {
            return completed(AndroidVisionLoopResult.BoundsExceeded("vision loop step limit exceeded"))
        }
        return bounded(multimodal.interpret(observation), deadlineNanos).thenCompose { decision ->
            currentFailure(runGeneration, deadlineNanos)?.let { return@thenCompose completed(it) }
            when (decision) {
                is AndroidVisionDecision.Done -> completed(
                    AndroidVisionLoopResult.Completed(
                        decision.summary.ifBlank { "verified" },
                        steps,
                        observedBytes,
                    )
                )
                is AndroidVisionDecision.Unavailable -> completed(
                    AndroidVisionLoopResult.Unavailable(decision.reason),
                )
                is AndroidVisionDecision.Act -> authorizeAndExecute(
                    goal = goal,
                    runGeneration = runGeneration,
                    deadlineNanos = deadlineNanos,
                    action = decision.action,
                    steps = steps,
                    observedBytes = observedBytes,
                )
            }
        }.exceptionally { error ->
            currentFailure(runGeneration, deadlineNanos)
                ?: AndroidVisionLoopResult.Failed(rootMessage(error))
        }
    }

    private fun authorizeAndExecute(
        goal: String,
        runGeneration: Long,
        deadlineNanos: Long,
        action: AndroidAutomationAction,
        steps: Int,
        observedBytes: Long,
    ): CompletableFuture<AndroidVisionLoopResult> {
        if (!isClosedAdbAction(action)) {
            return completed(AndroidVisionLoopResult.PolicyRejected(action))
        }
        val term = actionTerm(action)
        return bounded(
            queryProlog("kb_android_control:android_vision_action_decision($term, Decision)"),
            deadlineNanos,
        ).thenCompose { policy ->
            currentFailure(runGeneration, deadlineNanos)?.let { return@thenCompose completed(it) }
            if (!requiresApproval(policy)) {
                return@thenCompose completed(AndroidVisionLoopResult.PolicyRejected(action))
            }
            if (!authority.canExecute(action)) {
                return@thenCompose completed(AndroidVisionLoopResult.CapabilityUnavailable(action))
            }
            bounded(authority.approve(action), deadlineNanos).thenCompose approval@{ approved ->
                currentFailure(runGeneration, deadlineNanos)?.let { return@approval completed(it) }
                if (!approved) {
                    return@approval completed(AndroidVisionLoopResult.ApprovalRejected(action))
                }
                when (val attempt = executeFenced(action, runGeneration, deadlineNanos)) {
                    is EffectAttempt.Rejected -> completed(attempt.result)
                    is EffectAttempt.Executed -> when (val effect = attempt.result) {
                        is DeviceActionResult.Error -> completed(
                            AndroidVisionLoopResult.ActionFailed(action, effect.message ?: effect.code.wireId),
                        )
                        DeviceActionResult.Completed -> verifyFresh(
                            goal = goal,
                            runGeneration = runGeneration,
                            deadlineNanos = deadlineNanos,
                            action = action,
                            nextSequence = steps.toLong() + 1,
                            nextSteps = steps + 1,
                            observedBytes = observedBytes,
                        )
                    }
                }
            }
        }.exceptionally { error ->
            currentFailure(runGeneration, deadlineNanos)
                ?: AndroidVisionLoopResult.Failed(rootMessage(error))
        }
    }

    private fun verifyFresh(
        goal: String,
        runGeneration: Long,
        deadlineNanos: Long,
        action: AndroidAutomationAction,
        nextSequence: Long,
        nextSteps: Int,
        observedBytes: Long,
    ): CompletableFuture<AndroidVisionLoopResult> {
        currentFailure(runGeneration, deadlineNanos)?.let { return completed(it) }
        return observe(goal, nextSequence, observedBytes).thenCompose { observed ->
            when (observed) {
                is ObservationResult.Error -> completed(observed.result)
                is ObservationResult.Ok -> {
                    val verification = AndroidVisionVerification(
                        goal = goal,
                        action = action,
                        png = observed.observation.png,
                        sequence = observed.observation.sequence,
                    )
                    bounded(multimodal.verify(verification), deadlineNanos).thenCompose { verified ->
                        currentFailure(runGeneration, deadlineNanos)?.let { return@thenCompose completed(it) }
                        if (!verified) {
                            completed(AndroidVisionLoopResult.VerificationFailed(action))
                        } else {
                            step(
                                goal = goal,
                                runGeneration = runGeneration,
                                deadlineNanos = deadlineNanos,
                                observation = observed.observation,
                                steps = nextSteps,
                                observedBytes = observed.observedBytes,
                            )
                        }
                    }
                }
            }
        }
    }

    private fun observe(
        goal: String,
        sequence: Long,
        observedBytes: Long,
    ): CompletableFuture<ObservationResult> {
        val payload = adb.screenshotPng().getOrElse { error ->
            return completed(ObservationResult.Error(AndroidVisionLoopResult.Failed(rootMessage(error))))
        }
        if (payload.size > MAX_SINGLE_SCREENSHOT_BYTES) {
            return completed(
                ObservationResult.Error(
                    AndroidVisionLoopResult.BoundsExceeded("ADB screenshot exceeds 16 MiB"),
                )
            )
        }
        if (!hasPngMagic(payload)) {
            return completed(
                ObservationResult.Error(AndroidVisionLoopResult.Failed("ADB screenshot is not a PNG")),
            )
        }
        val total = observedBytes + payload.size
        if (total > limits.maxObservedBytes) {
            return completed(
                ObservationResult.Error(
                    AndroidVisionLoopResult.BoundsExceeded("vision loop observed-byte limit exceeded"),
                )
            )
        }
        return completed(
            ObservationResult.Ok(
                observation = AndroidVisionObservation(goal, payload.copyOf(), sequence),
                observedBytes = total,
            )
        )
    }

    private fun executeFenced(
        action: AndroidAutomationAction,
        runGeneration: Long,
        deadlineNanos: Long,
    ): EffectAttempt = synchronized(effectFence) {
        currentFailure(runGeneration, deadlineNanos)?.let {
            return@synchronized EffectAttempt.Rejected(it)
        }
        EffectAttempt.Executed(execute(action))
    }

    private fun execute(action: AndroidAutomationAction): DeviceActionResult = when (action) {
        is AndroidAutomationAction.AdbTap -> adb.tap(action.x, action.y)
        is AndroidAutomationAction.AdbSwipe -> adb.swipe(
            action.x1, action.y1, action.x2, action.y2, action.durationMs,
        )
        is AndroidAutomationAction.AdbText -> adb.typeText(action.text)
        is AndroidAutomationAction.AdbKey -> adb.key(action.key)
        is AndroidAutomationAction.AdbWait -> adb.wait(action.durationMs)
        else -> DeviceActionResult.Error(
            DeviceActionErrorCode.InvalidArguments,
            "vision proposed an action outside the closed ADB vocabulary",
        )
    }

    private fun currentFailure(runGeneration: Long, deadlineNanos: Long): AndroidVisionLoopResult? = when {
        generation.get() != runGeneration -> AndroidVisionLoopResult.Cancelled
        nanoTime() >= deadlineNanos -> AndroidVisionLoopResult.BoundsExceeded("vision loop timeout exceeded")
        else -> null
    }

    private fun requiresApproval(result: LocalQueryResult): Boolean {
        val value = result.terms.singleOrNull()?.trim() ?: return false
        return value == "require_approval" ||
            Regex("^Decision\\s*=\\s*require_approval\\.?$").matches(value)
    }

    private fun isClosedAdbAction(action: AndroidAutomationAction): Boolean = when (action) {
        is AndroidAutomationAction.AdbTap,
        is AndroidAutomationAction.AdbSwipe,
        is AndroidAutomationAction.AdbText,
        is AndroidAutomationAction.AdbKey,
        is AndroidAutomationAction.AdbWait -> true
        else -> false
    }

    private fun actionTerm(action: AndroidAutomationAction): String = when (action) {
        is AndroidAutomationAction.AdbTap -> "tap(${action.x},${action.y})"
        is AndroidAutomationAction.AdbSwipe ->
            "swipe(${action.x1},${action.y1},${action.x2},${action.y2},${action.durationMs})"
        is AndroidAutomationAction.AdbText -> "text('${escapeProlog(action.text)}')"
        is AndroidAutomationAction.AdbKey -> "key(${action.key.name.lowercase()})"
        is AndroidAutomationAction.AdbWait -> "wait(${action.durationMs})"
        else -> error("unsupported vision action")
    }

    private fun escapeProlog(value: String): String =
        value.replace("\\", "\\\\").replace("'", "\\'")

    private fun hasPngMagic(payload: ByteArray): Boolean =
        payload.size >= PNG_MAGIC.size && PNG_MAGIC.indices.all { payload[it] == PNG_MAGIC[it] }

    private fun <T> bounded(
        future: CompletableFuture<T>,
        deadlineNanos: Long,
    ): CompletableFuture<T> {
        val remainingNanos = deadlineNanos - nanoTime()
        if (remainingNanos <= 0) {
            return failed(TimeoutException("vision loop timeout exceeded"))
        }
        val bounded = CompletableFuture<T>()
        val timeout = VISION_TIMEOUT_EXECUTOR.schedule(
            {
                if (bounded.completeExceptionally(TimeoutException("vision loop timeout exceeded"))) {
                    multimodal.cancel()
                }
            },
            remainingNanos,
            TimeUnit.NANOSECONDS,
        )
        future.whenComplete { value, error ->
            timeout.cancel(false)
            if (error == null) bounded.complete(value)
            else bounded.completeExceptionally(error)
        }
        return bounded
    }

    private fun rootMessage(error: Throwable): String {
        var current = error
        while (current.cause != null && current.cause !== current) current = current.cause!!
        return (current.message ?: current::class.java.simpleName).take(256)
    }

    private fun <T> completed(value: T): CompletableFuture<T> = CompletableFuture.completedFuture(value)

    private fun <T> failed(error: Throwable): CompletableFuture<T> =
        CompletableFuture<T>().also { it.completeExceptionally(error) }

    private sealed interface ObservationResult {
        data class Ok(
            val observation: AndroidVisionObservation,
            val observedBytes: Long,
        ) : ObservationResult
        data class Error(val result: AndroidVisionLoopResult) : ObservationResult
    }

    private sealed interface EffectAttempt {
        data class Executed(val result: DeviceActionResult) : EffectAttempt
        data class Rejected(val result: AndroidVisionLoopResult) : EffectAttempt
    }
}
