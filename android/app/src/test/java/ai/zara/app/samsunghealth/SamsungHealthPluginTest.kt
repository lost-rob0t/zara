package ai.zara.app.samsunghealth

import ai.zara.app.prolog.PrologWorkspace
import ai.zara.app.runtime.LocalQueryResult
import android.app.Activity
import java.util.concurrent.CompletableFuture
import java.util.concurrent.atomic.AtomicInteger
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Rule
import org.junit.Test
import org.junit.rules.TemporaryFolder

class SamsungHealthPluginTest {
    @get:Rule
    val temporary = TemporaryFolder()

    @Test
    fun codecAcceptsOnlyClosedSamsungHealthEffects() {
        assertEquals(
            SamsungHealthAction.Status,
            SamsungHealthPrologCodec.decode("samsung_health_action(status)"),
        )
        assertEquals(
            SamsungHealthAction.Permissions,
            SamsungHealthPrologCodec.decode("samsung_health_action(permissions)"),
        )
        assertEquals(
            SamsungHealthAction.ReadToday(SamsungHealthMetric.STEPS),
            SamsungHealthPrologCodec.decode("samsung_health_action(read_today(steps))"),
        )
        assertEquals(null, SamsungHealthPrologCodec.decode("other_action(read_today(steps))"))

        expectFailure { SamsungHealthPrologCodec.decode("samsung_health_action(read_today(blood_pressure))") }
        expectFailure { SamsungHealthPrologCodec.decode("samsung_health_action(read_today(steps), shell(x))") }
        expectFailure { SamsungHealthPrologCodec.decode("samsung_health_action(${"x".repeat(600)})") }
    }

    @Test
    fun actorRejectsFanOutAndNeverReadsWithoutPermission() {
        val gateway = RecordingGateway(granted = emptySet())
        val actor = SamsungHealthPluginActor(gateway)
        try {
            expectFutureFailure {
                actor.dispatch(
                    listOf(
                        "samsung_health_action(status)",
                        "samsung_health_action(permissions)",
                    ),
                )
            }

            val reply = actor.dispatch(
                listOf("samsung_health_action(read_today(heart_rate))"),
            ).get()
            assertFalse(requireNotNull(reply).success)
            assertEquals(0, gateway.readCalls.get())
        } finally {
            actor.close()
        }
    }

    @Test
    fun actorSerializesAuthorizedReadsAndBoundsNormalizedPayload() {
        val gateway = RecordingGateway(granted = setOf(SamsungHealthMetric.STEPS))
        val actor = SamsungHealthPluginActor(gateway)
        try {
            val first = actor.dispatch(listOf("samsung_health_action(read_today(steps))"))
            val second = actor.dispatch(listOf("samsung_health_action(read_today(steps))"))
            assertTrue(requireNotNull(first.get()).success)
            assertTrue(requireNotNull(second.get()).success)
            assertEquals(2, gateway.readCalls.get())
            assertEquals(1, gateway.maxConcurrentReads.get())
        } finally {
            actor.close()
        }

        expectFailure {
            SamsungHealthReading(
                metric = SamsungHealthMetric.STEPS,
                values = mapOf("steps" to "9".repeat(9_000)),
            )
        }
    }

    @Test
    fun androidPluginQueriesPrologBeforePhysicalGateway() {
        val order = mutableListOf<String>()
        val gateway = RecordingGateway(
            granted = setOf(SamsungHealthMetric.STEPS),
            onRead = { order += "gateway" },
        )
        val plugin = SamsungHealthAndroidPlugin(
            queryProlog = { query ->
                order += "prolog"
                assertEquals("samsung_health_today(steps, Result)", query)
                CompletableFuture.completedFuture(
                    LocalQueryResult(
                        query = query,
                        terms = listOf("samsung_health_action(read_today(steps))"),
                        generation = 7,
                    ),
                )
            },
            gateway = gateway,
        )
        try {
            val reply = plugin.today(SamsungHealthMetric.STEPS).get()
            assertTrue(reply.success)
            assertEquals(listOf("prolog", "gateway"), order)
        } finally {
            plugin.close()
        }
    }

    @Test
    fun sourceInstallationNeverOverwritesOperatorEdits() {
        val workspace = PrologWorkspace(temporary.newFolder("workspace"))
        SamsungHealthPrologPlugin.install(workspace)
        val seeded = workspace.readSource(SamsungHealthPrologPlugin.SOURCE_NAME).text
        assertTrue(seeded.contains("samsung_health_today"))

        workspace.saveSource(SamsungHealthPrologPlugin.SOURCE_NAME, "% operator override\n")
        SamsungHealthPrologPlugin.install(workspace)
        assertEquals(
            "% operator override\n",
            workspace.readSource(SamsungHealthPrologPlugin.SOURCE_NAME).text,
        )
    }

    private class RecordingGateway(
        private val granted: Set<SamsungHealthMetric>,
        private val onRead: () -> Unit = {},
    ) : SamsungHealthGateway {
        val readCalls = AtomicInteger()
        val maxConcurrentReads = AtomicInteger()
        private val activeReads = AtomicInteger()

        override fun status(): SamsungHealthPlatformStatus =
            SamsungHealthPlatformStatus(SamsungHealthAvailability.READY)

        override fun grantedPermissions(): Set<SamsungHealthMetric> = granted

        override fun requestPermissions(
            activity: Activity,
            metrics: Set<SamsungHealthMetric>,
        ): CompletableFuture<Set<SamsungHealthMetric>> =
            CompletableFuture.completedFuture(granted.intersect(metrics))

        override fun readToday(metric: SamsungHealthMetric): SamsungHealthReading {
            val active = activeReads.incrementAndGet()
            maxConcurrentReads.updateAndGet { current -> maxOf(current, active) }
            return try {
                readCalls.incrementAndGet()
                onRead()
                Thread.sleep(5)
                SamsungHealthReading(metric, mapOf("steps" to "1234"))
            } finally {
                activeReads.decrementAndGet()
            }
        }
    }

    private fun expectFailure(block: () -> Unit) {
        try {
            block()
            throw AssertionError("expected failure")
        } catch (_: IllegalArgumentException) {
        }
    }

    private fun expectFutureFailure(block: () -> CompletableFuture<*>) {
        try {
            block().get()
            throw AssertionError("expected future failure")
        } catch (error: java.util.concurrent.ExecutionException) {
            assertTrue(error.cause is IllegalArgumentException)
        }
    }
}