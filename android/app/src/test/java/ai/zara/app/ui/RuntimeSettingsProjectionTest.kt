package ai.zara.app.ui

import ai.zara.app.runtime.RuntimeControlOwner
import ai.zara.app.runtime.RuntimeDescriptor
import ai.zara.app.runtime.RuntimeHealth
import ai.zara.app.runtime.RuntimeLocality
import ai.zara.app.runtime.RuntimeRegistry
import ai.zara.app.runtime.RuntimeTransport
import ai.zara.app.runtime.ZARA_RUNTIME_PROTOCOL
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class RuntimeSettingsProjectionTest {
    @Test
    fun projectsOnlyCanonicalInstalledDescriptorsAndSelectionByStableId() {
        val registry = RuntimeRegistry()
        registry.refresh(
            listOf(
                descriptor(
                    id = "worker-b",
                    health = RuntimeHealth.DEGRADED,
                    profiles = listOf("profile-b"),
                ),
                descriptor(id = "worker-a", health = RuntimeHealth.READY),
                descriptor(id = "not-installed", installed = false, health = RuntimeHealth.STOPPED),
            )
        )
        registry.select("worker-a")

        val rows = registry.snapshot().runtimeSettingsRows()

        assertEquals(listOf("worker-a", "worker-b"), rows.map { it.runtimeId })
        assertTrue(rows.single { it.runtimeId == "worker-a" }.selected)
        assertFalse(rows.single { it.runtimeId == "worker-b" }.selected)
        assertEquals(RuntimeHealth.DEGRADED, rows.single { it.runtimeId == "worker-b" }.health)
        assertEquals(listOf("profile-b"), rows.single { it.runtimeId == "worker-b" }.profiles)
    }

    @Test
    fun healthControlsSelectionWithoutHidingInstalledDiagnostics() {
        val registry = RuntimeRegistry()
        registry.refresh(
            listOf(
                descriptor(id = "ready-runtime", health = RuntimeHealth.READY),
                descriptor(id = "stopped-runtime", health = RuntimeHealth.STOPPED),
            )
        )

        val rows = registry.snapshot().runtimeSettingsRows()

        assertTrue(rows.single { it.runtimeId == "ready-runtime" }.selectable)
        assertFalse(rows.single { it.runtimeId == "stopped-runtime" }.selectable)
        assertEquals(RuntimeHealth.STOPPED, rows.single { it.runtimeId == "stopped-runtime" }.health)
    }

    @Test
    fun projectionCarriesVersionLocalityAndProfilesWithoutRoutingPolicyState() {
        val registry = RuntimeRegistry()
        registry.refresh(
            listOf(
                descriptor(
                    id = "sidecar-runtime",
                    runtimeVersion = "2.4.1",
                    implementationVersion = "build-17",
                    locality = RuntimeLocality.LOCAL_SIDECAR,
                    profiles = listOf("profile-one", "profile-two"),
                )
            )
        )

        val row = registry.snapshot().runtimeSettingsRows().single()

        assertEquals("2.4.1", row.runtimeVersion)
        assertEquals("build-17", row.implementationVersion)
        assertEquals(RuntimeLocality.LOCAL_SIDECAR, row.locality)
        assertEquals(listOf("profile-one", "profile-two"), row.profiles)
        assertFalse(RuntimeSettingsRuntimeRow::class.java.declaredFields.any { it.name.contains("routing", true) })
    }

    private fun descriptor(
        id: String,
        runtimeVersion: String = "1.0.0",
        implementationVersion: String = "impl-1",
        installed: Boolean = true,
        health: RuntimeHealth = RuntimeHealth.READY,
        locality: RuntimeLocality = RuntimeLocality.EMBEDDED,
        profiles: List<String> = emptyList(),
    ): RuntimeDescriptor = RuntimeDescriptor(
        id = id,
        displayName = id,
        protocol = ZARA_RUNTIME_PROTOCOL,
        runtimeVersion = runtimeVersion,
        implementationVersion = implementationVersion,
        installed = installed,
        available = health != RuntimeHealth.FAILED && health != RuntimeHealth.STOPPED,
        health = health,
        locality = locality,
        transport = when (locality) {
            RuntimeLocality.EMBEDDED -> RuntimeTransport.IN_PROCESS
            RuntimeLocality.LOCAL_PROCESS -> RuntimeTransport.STDIO
            RuntimeLocality.LOCAL_SIDECAR -> RuntimeTransport.LOOPBACK_HTTP
            RuntimeLocality.REMOTE -> RuntimeTransport.ZARA_REMOTE
        },
        profiles = profiles,
        providerControl = RuntimeControlOwner.RUNTIME,
        modelControl = RuntimeControlOwner.RUNTIME,
        supportsStreaming = true,
        supportsCancel = true,
        provenance = "test:$id",
    )
}
