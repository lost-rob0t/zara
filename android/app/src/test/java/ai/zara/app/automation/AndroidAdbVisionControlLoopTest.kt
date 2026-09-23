package ai.zara.app.automation

import ai.zara.app.device.DeviceActionResult
import ai.zara.app.prolog.AndroidAutomationAction
import ai.zara.app.runtime.LocalQueryResult
import org.junit.Assert.assertArrayEquals
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test
import java.util.ArrayDeque
import java.util.concurrent.CompletableFuture
import java.util.concurrent.TimeUnit

class AndroidAdbVisionControlLoopTest {
    @Test
    fun `screenshot is ingested by canonical multimodal interpreter before any decision`() {
        val screenshot = png(1)
        val adb = FakeAdbPort(ArrayDeque(listOf(screenshot)))
        val interpreter = FakeVisionPort(
            decisions = ArrayDeque(listOf(AndroidVisionDecision.Done("already complete"))),
        )
        val loop = loop(adb, interpreter)

        val result = loop.run("close the notification shade").get(2, TimeUnit.SECONDS)

        assertEquals(AndroidVisionLoopResult.Completed("already complete", 0, screenshot.size.toLong()), result)
        assertEquals(1, interpreter.observations.size)
        assertArrayEquals(screenshot, interpreter.observations.single().png)
        assertTrue(adb.actions.isEmpty())
    }

    @Test
    fun `capability and approval both fence a state changing action`() {
        val action = AndroidAutomationAction.AdbTap(100, 200)
        val adb = FakeAdbPort(ArrayDeque(listOf(png(1), png(2))))
        val interpreter = FakeVisionPort(
            decisions = ArrayDeque(listOf(AndroidVisionDecision.Act(action))),
        )
        val noCapability = FakeAuthority(capable = false, approved = true)

        val blocked = loop(adb, interpreter, noCapability).run("tap Continue").get(2, TimeUnit.SECONDS)

        assertEquals(AndroidVisionLoopResult.CapabilityUnavailable(action), blocked)
        assertFalse(noCapability.approvalRequested)
        assertTrue(adb.actions.isEmpty())

        val adbDenied = FakeAdbPort(ArrayDeque(listOf(png(3), png(4))))
        val interpreterDenied = FakeVisionPort(
            decisions = ArrayDeque(listOf(AndroidVisionDecision.Act(action))),
        )
        val denied = FakeAuthority(capable = true, approved = false)

        val rejected = loop(adbDenied, interpreterDenied, denied).run("tap Continue").get(2, TimeUnit.SECONDS)

        assertEquals(AndroidVisionLoopResult.ApprovalRejected(action), rejected)
        assertTrue(denied.approvalRequested)
        assertTrue(adbDenied.actions.isEmpty())
    }

    @Test
    fun `cancel fences a late interpretation before it can produce an effect`() {
        val adb = FakeAdbPort(ArrayDeque(listOf(png(1), png(2))))
        val pending = CompletableFuture<AndroidVisionDecision>()
        val interpreter = FakeVisionPort(pendingDecision = pending)
        val loop = loop(adb, interpreter)

        val result = loop.run("tap Continue")
        loop.cancel()
        pending.complete(AndroidVisionDecision.Act(AndroidAutomationAction.AdbTap(1, 1)))

        assertEquals(AndroidVisionLoopResult.Cancelled, result.get(2, TimeUnit.SECONDS))
        assertTrue(adb.actions.isEmpty())
        assertTrue(interpreter.cancelled)
    }

    @Test
    fun `new run generation fences a late decision from the prior run`() {
        val adb = FakeAdbPort(ArrayDeque(listOf(png(1), png(2))))
        val firstDecision = CompletableFuture<AndroidVisionDecision>()
        var calls = 0
        val interpreter = object : CanonicalMultimodalVisionPort {
            override fun interpret(observation: AndroidVisionObservation): CompletableFuture<AndroidVisionDecision> {
                calls += 1
                return if (calls == 1) firstDecision
                else CompletableFuture.completedFuture(AndroidVisionDecision.Done("new generation"))
            }

            override fun verify(verification: AndroidVisionVerification): CompletableFuture<Boolean> =
                CompletableFuture.completedFuture(true)

            override fun cancel() = Unit
        }
        val authority = FakeAuthority(capable = true, approved = true)
        val loop = AndroidAdbVisionControlLoop(
            adb = adb,
            multimodal = interpreter,
            queryProlog = { query ->
                CompletableFuture.completedFuture(LocalQueryResult(query, listOf("require_approval"), 1))
            },
            authority = authority,
            limits = AndroidVisionLoopLimits(maxSteps = 4, maxObservedBytes = 1024 * 1024, timeoutMillis = 5_000),
        )

        val oldRun = loop.run("old goal")
        val newRun = loop.run("new goal")
        assertEquals(
            AndroidVisionLoopResult.Completed("new generation", 0, 9),
            newRun.get(2, TimeUnit.SECONDS),
        )
        firstDecision.complete(AndroidVisionDecision.Act(AndroidAutomationAction.AdbTap(9, 9)))

        assertEquals(AndroidVisionLoopResult.Cancelled, oldRun.get(2, TimeUnit.SECONDS))
        assertTrue(adb.actions.isEmpty())
        assertFalse(authority.approvalRequested)
    }

    @Test
    fun `mutation success is not accepted until a fresh screenshot verifies the postcondition`() {
        val before = png(1)
        val after = png(2)
        val action = AndroidAutomationAction.AdbKey(AdbAutomationKey.Home)
        val adb = FakeAdbPort(ArrayDeque(listOf(before, after)))
        val interpreter = FakeVisionPort(
            decisions = ArrayDeque(
                listOf(
                    AndroidVisionDecision.Act(action),
                    AndroidVisionDecision.Done("verified"),
                ),
            ),
            verification = true,
        )
        val loop = loop(adb, interpreter)

        val result = loop.run("go home").get(2, TimeUnit.SECONDS)

        assertEquals(AndroidVisionLoopResult.Completed("verified", 1, (before.size + after.size).toLong()), result)
        assertEquals(listOf("key:home"), adb.actions)
        assertEquals(1, interpreter.verifications.size)
        assertArrayEquals(after, interpreter.verifications.single().png)
        assertFalse(before.contentEquals(interpreter.verifications.single().png))
        assertArrayEquals(after, interpreter.observations.last().png)
    }

    @Test
    fun `fresh verified observation can drive another typed step`() {
        val first = AndroidAutomationAction.AdbTap(10, 20)
        val second = AndroidAutomationAction.AdbKey(AdbAutomationKey.Enter)
        val adb = FakeAdbPort(ArrayDeque(listOf(png(1), png(2), png(3))))
        val interpreter = FakeVisionPort(
            decisions = ArrayDeque(
                listOf(
                    AndroidVisionDecision.Act(first),
                    AndroidVisionDecision.Act(second),
                    AndroidVisionDecision.Done("goal complete"),
                ),
            ),
        )

        val result = loop(adb, interpreter).run("finish two steps").get(2, TimeUnit.SECONDS)

        assertEquals(AndroidVisionLoopResult.Completed("goal complete", 2, 27), result)
        assertEquals(listOf("tap:10:20", "key:enter"), adb.actions)
        assertEquals(2, interpreter.verifications.size)
        assertEquals(3, interpreter.observations.size)
    }

    @Test
    fun `failed fresh verification cannot be reported as success`() {
        val action = AndroidAutomationAction.AdbTap(8, 9)
        val adb = FakeAdbPort(ArrayDeque(listOf(png(1), png(2))))
        val interpreter = FakeVisionPort(
            decisions = ArrayDeque(listOf(AndroidVisionDecision.Act(action))),
            verification = false,
        )

        val result = loop(adb, interpreter).run("tap target").get(2, TimeUnit.SECONDS)

        assertEquals(AndroidVisionLoopResult.VerificationFailed(action), result)
        assertEquals(listOf("tap:8:9"), adb.actions)
    }

    private fun loop(
        adb: FakeAdbPort,
        interpreter: FakeVisionPort,
        authority: FakeAuthority = FakeAuthority(capable = true, approved = true),
    ): AndroidAdbVisionControlLoop = AndroidAdbVisionControlLoop(
        adb = adb,
        multimodal = interpreter,
        queryProlog = { query ->
            CompletableFuture.completedFuture(LocalQueryResult(query, listOf("require_approval"), 1))
        },
        authority = authority,
        limits = AndroidVisionLoopLimits(maxSteps = 4, maxObservedBytes = 1024 * 1024, timeoutMillis = 5_000),
    )

    private class FakeAuthority(
        private val capable: Boolean,
        private val approved: Boolean,
    ) : AndroidVisionActionAuthority {
        var approvalRequested = false

        override fun canExecute(action: AndroidAutomationAction): Boolean = capable

        override fun approve(action: AndroidAutomationAction): CompletableFuture<Boolean> {
            approvalRequested = true
            return CompletableFuture.completedFuture(approved)
        }
    }

    private class FakeVisionPort(
        private val decisions: ArrayDeque<AndroidVisionDecision> = ArrayDeque(),
        private val verification: Boolean = true,
        private val pendingDecision: CompletableFuture<AndroidVisionDecision>? = null,
    ) : CanonicalMultimodalVisionPort {
        val observations = mutableListOf<AndroidVisionObservation>()
        val verifications = mutableListOf<AndroidVisionVerification>()
        var cancelled = false

        override fun interpret(observation: AndroidVisionObservation): CompletableFuture<AndroidVisionDecision> {
            observations += observation
            return pendingDecision ?: CompletableFuture.completedFuture(decisions.removeFirst())
        }

        override fun verify(verification: AndroidVisionVerification): CompletableFuture<Boolean> {
            verifications += verification
            return CompletableFuture.completedFuture(this.verification)
        }

        override fun cancel() {
            cancelled = true
        }
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

        override fun swipe(x1: Int, y1: Int, x2: Int, y2: Int, durationMs: Int): DeviceActionResult {
            actions += "swipe:$x1:$y1:$x2:$y2:$durationMs"
            return DeviceActionResult.Completed
        }

        override fun typeText(text: String): DeviceActionResult {
            actions += "text:$text"
            return DeviceActionResult.Completed
        }

        override fun key(key: AdbAutomationKey): DeviceActionResult {
            actions += "key:${key.name.lowercase()}"
            return DeviceActionResult.Completed
        }

        override fun wait(durationMs: Int): DeviceActionResult {
            actions += "wait:$durationMs"
            return DeviceActionResult.Completed
        }

        override fun screenshotPng(): Result<ByteArray> =
            if (screenshots.isEmpty()) Result.failure(IllegalStateException("no screenshot"))
            else Result.success(screenshots.removeFirst())
    }

    private fun png(marker: Int): ByteArray = byteArrayOf(
        0x89.toByte(), 0x50, 0x4E, 0x47, 0x0D, 0x0A, 0x1A, 0x0A,
        marker.toByte(),
    )
}
