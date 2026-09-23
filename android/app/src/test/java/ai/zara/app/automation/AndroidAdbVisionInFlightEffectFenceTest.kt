package ai.zara.app.automation

import ai.zara.app.device.DeviceActionResult
import ai.zara.app.prolog.AndroidAutomationAction
import ai.zara.app.runtime.LocalQueryResult
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import java.util.concurrent.CompletableFuture
import java.util.concurrent.CountDownLatch
import java.util.concurrent.TimeUnit
import java.util.concurrent.atomic.AtomicInteger

class AndroidAdbVisionInFlightEffectFenceTest {
    @Test
    fun `cancel cannot return while an admitted effect is still in flight`() {
        val action = AndroidAutomationAction.AdbTap(40, 80)
        val effectEntered = CountDownLatch(1)
        val releaseEffect = CountDownLatch(1)
        val verification = CompletableFuture<Boolean>()
        val screenshots = AtomicInteger(0)

        val adb = object : AdbAutomationPort {
            override fun isAvailable(): Boolean = true

            override fun tap(x: Int, y: Int): DeviceActionResult {
                effectEntered.countDown()
                check(releaseEffect.await(2, TimeUnit.SECONDS)) { "test effect release timed out" }
                return DeviceActionResult.Completed
            }

            override fun swipe(x1: Int, y1: Int, x2: Int, y2: Int, durationMs: Int): DeviceActionResult =
                DeviceActionResult.Completed

            override fun typeText(text: String): DeviceActionResult = DeviceActionResult.Completed

            override fun key(key: AdbAutomationKey): DeviceActionResult = DeviceActionResult.Completed

            override fun wait(durationMs: Int): DeviceActionResult = DeviceActionResult.Completed

            override fun screenshotPng(): Result<ByteArray> =
                Result.success(png(screenshots.incrementAndGet()))
        }
        val multimodal = object : CanonicalMultimodalVisionPort {
            override fun interpret(observation: AndroidVisionObservation): CompletableFuture<AndroidVisionDecision> =
                CompletableFuture.completedFuture(AndroidVisionDecision.Act(action))

            override fun verify(verificationRequest: AndroidVisionVerification): CompletableFuture<Boolean> = verification

            override fun cancel() = Unit
        }
        val loop = loop(adb, multimodal)

        val run = CompletableFuture.supplyAsync {
            loop.run("tap target").get(3, TimeUnit.SECONDS)
        }
        assertTrue(effectEntered.await(1, TimeUnit.SECONDS))

        val cancel = CompletableFuture.runAsync { loop.cancel() }
        Thread.sleep(100)
        assertFalse("cancel returned while the old effect could still complete later", cancel.isDone)

        releaseEffect.countDown()
        cancel.get(1, TimeUnit.SECONDS)
        verification.complete(true)

        assertEquals(AndroidVisionLoopResult.Cancelled, run.get(2, TimeUnit.SECONDS))
    }

    @Test
    fun `cancel request fences an effect that has not been admitted yet`() {
        val action = AndroidAutomationAction.AdbTap(12, 24)
        val pendingDecision = CompletableFuture<AndroidVisionDecision>()
        val cancelEntered = CountDownLatch(1)
        val releaseCancelSignal = CountDownLatch(1)
        val effects = AtomicInteger(0)

        val adb = object : AdbAutomationPort {
            override fun isAvailable(): Boolean = true

            override fun tap(x: Int, y: Int): DeviceActionResult {
                effects.incrementAndGet()
                return DeviceActionResult.Completed
            }

            override fun swipe(x1: Int, y1: Int, x2: Int, y2: Int, durationMs: Int): DeviceActionResult =
                DeviceActionResult.Completed

            override fun typeText(text: String): DeviceActionResult = DeviceActionResult.Completed

            override fun key(key: AdbAutomationKey): DeviceActionResult = DeviceActionResult.Completed

            override fun wait(durationMs: Int): DeviceActionResult = DeviceActionResult.Completed

            override fun screenshotPng(): Result<ByteArray> = Result.success(png(1))
        }
        val multimodal = object : CanonicalMultimodalVisionPort {
            override fun interpret(observation: AndroidVisionObservation): CompletableFuture<AndroidVisionDecision> =
                pendingDecision

            override fun verify(verificationRequest: AndroidVisionVerification): CompletableFuture<Boolean> =
                CompletableFuture.completedFuture(true)

            override fun cancel() {
                cancelEntered.countDown()
                check(releaseCancelSignal.await(2, TimeUnit.SECONDS)) { "test cancel signal release timed out" }
            }
        }
        val loop = loop(adb, multimodal)
        val run = loop.run("tap target")

        val cancel = CompletableFuture.runAsync { loop.cancel() }
        assertTrue(cancelEntered.await(1, TimeUnit.SECONDS))

        // Once cancellation has entered the loop, a late interpretation is stale even while the
        // provider cancellation signal itself is still unwinding.
        pendingDecision.complete(AndroidVisionDecision.Act(action))
        Thread.sleep(100)
        assertEquals(0, effects.get())

        releaseCancelSignal.countDown()
        cancel.get(1, TimeUnit.SECONDS)
        assertEquals(AndroidVisionLoopResult.Cancelled, run.get(1, TimeUnit.SECONDS))
        assertEquals(0, effects.get())
    }

    private fun loop(
        adb: AdbAutomationPort,
        multimodal: CanonicalMultimodalVisionPort,
    ): AndroidAdbVisionControlLoop = AndroidAdbVisionControlLoop(
        adb = adb,
        multimodal = multimodal,
        queryProlog = { query ->
            CompletableFuture.completedFuture(LocalQueryResult(query, listOf("require_approval"), 1))
        },
        authority = object : AndroidVisionActionAuthority {
            override fun canExecute(action: AndroidAutomationAction): Boolean = true

            override fun approve(action: AndroidAutomationAction): CompletableFuture<Boolean> =
                CompletableFuture.completedFuture(true)
        },
        limits = AndroidVisionLoopLimits(maxSteps = 2, maxObservedBytes = 1024 * 1024, timeoutMillis = 5_000),
    )

    private fun png(marker: Int): ByteArray = byteArrayOf(
        0x89.toByte(), 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
        marker.toByte(),
    )
}
