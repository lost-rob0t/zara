package ai.zara.app.automation

import ai.zara.app.device.DeviceActionResult
import ai.zara.app.prolog.AndroidAutomationAction
import ai.zara.app.runtime.LocalQueryResult
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertTrue
import java.util.ArrayDeque
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit

/** Emulator-side acceptance invoked by the existing connected-test promotion class. */
object AndroidAdbVisionEmulatorAcceptance {
    fun assertVerifiedLoop() {
        val before = png(1)
        val after = png(2)
        val adb = FakeAdbPort(ArrayDeque(listOf(before, after)))
        val multimodal = FakeVisionPort(
            ArrayDeque(
                listOf(
                    AndroidVisionDecision.Act(AndroidAutomationAction.AdbTap(42, 84)),
                    AndroidVisionDecision.Done("verified on emulator"),
                )
            )
        )
        var approvals = 0
        val loop = AndroidAdbVisionControlLoop(
            adb = adb,
            multimodal = multimodal,
            queryProlog = { query ->
                assertTrue(query.contains("android_vision_action_decision(tap(42,84), Decision)"))
                CompletableFuture.completedFuture(LocalQueryResult(query, listOf("Decision = require_approval"), 1))
            },
            authority = object : AndroidVisionActionAuthority {
                override fun canExecute(action: AndroidAutomationAction): Boolean = true
                override fun approve(action: AndroidAutomationAction): CompletableFuture<Boolean> {
                    approvals += 1
                    return CompletableFuture.completedFuture(true)
                }
            },
            limits = AndroidVisionLoopLimits(maxSteps = 2, maxObservedBytes = 1024 * 1024, timeoutMillis = 5_000),
        )

        val result = loop.run("tap the target and verify it").get(5, TimeUnit.SECONDS)

        assertEquals(
            AndroidVisionLoopResult.Completed("verified on emulator", 1, (before.size + after.size).toLong()),
            result,
        )
        assertEquals(1, approvals)
        assertEquals(listOf("tap:42:84"), adb.actions)
        assertEquals(2, multimodal.observations.size)
        assertEquals(1, multimodal.verifications.size)
        assertArrayEquals(before, multimodal.observations.first().png)
        assertArrayEquals(after, multimodal.verifications.single().png)
        assertArrayEquals(after, multimodal.observations.last().png)
    }

    private class FakeVisionPort(
        private val decisions: ArrayDeque<AndroidVisionDecision>,
    ) : CanonicalMultimodalVisionPort {
        val observations = mutableListOf<AndroidVisionObservation>()
        val verifications = mutableListOf<AndroidVisionVerification>()

        override fun interpret(observation: AndroidVisionObservation): CompletableFuture<AndroidVisionDecision> {
            observations += observation
            return CompletableFuture.completedFuture(decisions.removeFirst())
        }

        override fun verify(verification: AndroidVisionVerification): CompletableFuture<Boolean> {
            verifications += verification
            return CompletableFuture.completedFuture(true)
        }

        override fun cancel() = Unit
    }

    private class FakeAdbPort(
        private val screenshots: ArrayDeque<ByteArray>,
    ) : AdbAutomationPort {
        val actions = mutableListOf<String>()

        override fun isAvailable(): Boolean = true
        override fun tap(x: Int, y: Int): DeviceActionResult {
            actions += "tap:$x:$y"
            return DeviceActionResult.Completed
        }
        override fun swipe(x1: Int, y1: Int, x2: Int, y2: Int, durationMs: Int): DeviceActionResult =
            DeviceActionResult.Completed
        override fun typeText(text: String): DeviceActionResult = DeviceActionResult.Completed
        override fun key(key: AdbAutomationKey): DeviceActionResult = DeviceActionResult.Completed
        override fun wait(durationMs: Int): DeviceActionResult = DeviceActionResult.Completed
        override fun screenshotPng(): Result<ByteArray> = Result.success(screenshots.removeFirst())
    }

    private fun png(marker: Int): ByteArray = byteArrayOf(
        0x89.toByte(), 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
        marker.toByte(),
    )
}
