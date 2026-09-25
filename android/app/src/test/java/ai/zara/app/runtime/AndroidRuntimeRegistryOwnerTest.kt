package ai.zara.app.runtime

import ai.zara.app.localai.LocalAiPhase
import ai.zara.app.localai.LocalAiState
import ai.zara.app.localai.LocalModelBackend
import ai.zara.app.localai.LocalModelFormat
import ai.zara.app.localai.LocalModelQuantization
import ai.zara.app.localai.LocalModelSpec
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNotNull
import org.junit.Assert.assertTrue
import org.junit.Test

class AndroidRuntimeRegistryOwnerTest {
    @Test
    fun stoppedEmbeddedRuntimeIsDiscoveredButNotSelectable() {
        val owner = owner()

        val snapshot = owner.refresh(LocalAiState(phase = LocalAiPhase.STOPPED))
        val embedded = snapshot.descriptors.single()

        assertEquals(EMBEDDED_LOCAL_RUNTIME_ID, embedded.id)
        assertEquals(RuntimeHealth.STOPPED, embedded.health)
        assertFalse(embedded.available)
        assertFalse(embedded.selectable)
        assertEquals(null, snapshot.selection)
    }

    @Test
    fun readyAndGeneratingRequireARealLoadedModel() {
        val owner = owner()
        val model = modelSpec()

        val ready = owner.refresh(LocalAiState(phase = LocalAiPhase.READY, model = model))
        val readyDescriptor = ready.descriptors.single()
        assertEquals(RuntimeHealth.READY, readyDescriptor.health)
        assertTrue(readyDescriptor.available)
        assertTrue(readyDescriptor.selectable)
        assertEquals(listOf("local-model"), readyDescriptor.profiles)

        val generating = owner.refresh(
            LocalAiState(
                phase = LocalAiPhase.GENERATING,
                generation = 4,
                model = model,
            )
        )
        val generatingDescriptor = generating.descriptors.single()
        assertEquals(RuntimeHealth.BUSY, generatingDescriptor.health)
        assertTrue(generatingDescriptor.available)
        assertTrue(generatingDescriptor.selectable)

        val impossibleReady = owner.refresh(LocalAiState(phase = LocalAiPhase.READY, model = null))
        val impossibleDescriptor = impossibleReady.descriptors.single()
        assertEquals(RuntimeHealth.DEGRADED, impossibleDescriptor.health)
        assertFalse(impossibleDescriptor.available)
        assertFalse(impossibleDescriptor.selectable)
    }

    @Test
    fun loadingAndFailedNeverInventReadiness() {
        val owner = owner()

        val loading = owner.refresh(LocalAiState(phase = LocalAiPhase.LOADING, model = modelSpec()))
        assertEquals(RuntimeHealth.STARTING, loading.descriptors.single().health)
        assertFalse(loading.descriptors.single().selectable)

        val failed = owner.refresh(
            LocalAiState(
                phase = LocalAiPhase.FAILED,
                model = modelSpec(),
                failure = "backend_failed",
            )
        )
        assertEquals(RuntimeHealth.FAILED, failed.descriptors.single().health)
        assertFalse(failed.descriptors.single().available)
        assertFalse(failed.descriptors.single().selectable)
    }

    @Test
    fun optionalDiscoveryIsGenericAndSelectionUsesStableRuntimeId() {
        val optional = descriptor("sidecar-x", RuntimeHealth.READY)
        val owner = AndroidRuntimeRegistryOwner(
            runtimeVersion = "0.2.2-alpha",
            implementationVersion = "abc123",
            optionalRuntimeSources = listOf({ optional }),
        )

        val snapshot = owner.refresh(LocalAiState(phase = LocalAiPhase.READY, model = modelSpec()))
        assertEquals(listOf(EMBEDDED_LOCAL_RUNTIME_ID, "sidecar-x"), snapshot.descriptors.map { it.id })
        assertTrue(snapshot.descriptors.all { it.installed })

        val selection = owner.select("sidecar-x")
        assertEquals("sidecar-x", selection.runtimeId)
        assertEquals("sidecar-x", owner.snapshot().selection?.runtimeId)
    }

    @Test
    fun unavailableOptionalRuntimeIsOmittedInsteadOfManufactured() {
        val owner = AndroidRuntimeRegistryOwner(
            runtimeVersion = "0.2.2-alpha",
            implementationVersion = "abc123",
            optionalRuntimeSources = listOf(
                { null },
                { throw RuntimeUnavailable("sidecar unavailable") },
            ),
        )

        val snapshot = owner.refresh(LocalAiState(phase = LocalAiPhase.STOPPED))

        assertEquals(listOf(EMBEDDED_LOCAL_RUNTIME_ID), snapshot.descriptors.map { it.id })
        assertNotNull(snapshot.descriptors.single())
    }

    @Test
    fun duplicateOptionalRuntimeCannotReplaceEmbeddedRuntime() {
        val duplicate = descriptor(EMBEDDED_LOCAL_RUNTIME_ID, RuntimeHealth.READY)
        val owner = AndroidRuntimeRegistryOwner(
            runtimeVersion = "0.2.2-alpha",
            implementationVersion = "abc123",
            optionalRuntimeSources = listOf({ duplicate }),
        )

        val snapshot = owner.refresh(LocalAiState(phase = LocalAiPhase.STOPPED))

        assertEquals(1, snapshot.descriptors.size)
        val embedded = snapshot.descriptors.single()
        assertEquals(EMBEDDED_LOCAL_RUNTIME_ID, embedded.id)
        assertEquals(RuntimeLocality.EMBEDDED, embedded.locality)
        assertEquals(RuntimeHealth.STOPPED, embedded.health)
        assertFalse(embedded.available)
        assertFalse(embedded.selectable)
        assertEquals(null, snapshot.selection)
    }

    @Test
    fun duplicateOptionalRuntimeIdsFailClosedAndPreserveUniqueRuntime() {
        val first = descriptor("sidecar-x", RuntimeHealth.READY)
        val unique = descriptor("sidecar-y", RuntimeHealth.READY)
        val second = descriptor("sidecar-x", RuntimeHealth.FAILED)
        val third = descriptor("sidecar-x", RuntimeHealth.READY)
        val owner = AndroidRuntimeRegistryOwner(
            runtimeVersion = "0.2.2-alpha",
            implementationVersion = "abc123",
            optionalRuntimeSources = listOf({ first }, { unique }, { second }, { third }),
        )

        val snapshot = owner.refresh(LocalAiState(phase = LocalAiPhase.STOPPED))

        assertEquals(listOf(EMBEDDED_LOCAL_RUNTIME_ID, "sidecar-y"), snapshot.descriptors.map { it.id })
        assertFalse(snapshot.descriptors.any { it.id == "sidecar-x" })
        val sidecar = snapshot.descriptors.single { it.id == "sidecar-y" }
        assertEquals(RuntimeHealth.READY, sidecar.health)
        assertTrue(sidecar.available)
        assertTrue(sidecar.selectable)
    }

    @Test
    fun localAiStateReadFailureKeepsUniqueOptionalRuntimeButClearsEmbeddedSelection() {
        val sidecar = descriptor("sidecar-y", RuntimeHealth.READY)
        val owner = AndroidRuntimeRegistryOwner(
            runtimeVersion = "0.2.2-alpha",
            implementationVersion = "abc123",
            optionalRuntimeSources = listOf({ sidecar }),
        )
        val model = modelSpec()

        owner.refresh(LocalAiState(phase = LocalAiPhase.READY, model = model))
        owner.select(EMBEDDED_LOCAL_RUNTIME_ID)

        val unavailable = owner.refreshUnavailable()

        assertEquals(
            listOf(EMBEDDED_LOCAL_RUNTIME_ID, "sidecar-y"),
            unavailable.descriptors.map { it.id },
        )
        val embedded = unavailable.descriptors.single { it.id == EMBEDDED_LOCAL_RUNTIME_ID }
        assertEquals(RuntimeHealth.DEGRADED, embedded.health)
        assertFalse(embedded.available)
        assertFalse(embedded.selectable)
        val optional = unavailable.descriptors.single { it.id == "sidecar-y" }
        assertEquals(RuntimeHealth.READY, optional.health)
        assertTrue(optional.available)
        assertTrue(optional.selectable)
        assertEquals(null, unavailable.selection)
    }

    @Test
    fun localAiStateReadFailureClearsStaleEmbeddedReadiness() {
        val owner = owner()
        val model = modelSpec()

        val ready = owner.refresh(LocalAiState(phase = LocalAiPhase.READY, model = model))
        assertTrue(ready.descriptors.single().selectable)
        owner.select(EMBEDDED_LOCAL_RUNTIME_ID)

        val unavailable = owner.refreshUnavailable()
        val embedded = unavailable.descriptors.single()

        assertEquals(RuntimeHealth.DEGRADED, embedded.health)
        assertFalse(embedded.available)
        assertFalse(embedded.selectable)
        assertTrue(embedded.capabilities.isEmpty())
        assertTrue(embedded.profiles.isEmpty())
        assertEquals(null, unavailable.selection)
    }

    private fun owner(): AndroidRuntimeRegistryOwner = AndroidRuntimeRegistryOwner(
        runtimeVersion = "0.2.2-alpha",
        implementationVersion = "abc123",
    )

    private fun modelSpec(): LocalModelSpec = LocalModelSpec(
        id = "qwen3-0.6b",
        version = "1.0",
        quantization = LocalModelQuantization.Q4_K_M,
        sha256 = "0".repeat(64),
        path = "/data/user/0/ai.zara.app/files/qwen3-0.6b.gguf",
        maxContextTokens = 4096,
        backend = LocalModelBackend.CPU,
        format = LocalModelFormat.GGUF,
    )

    private fun descriptor(id: String, health: RuntimeHealth): RuntimeDescriptor = RuntimeDescriptor(
        id = id,
        displayName = id,
        protocol = ZARA_RUNTIME_PROTOCOL,
        runtimeVersion = "1.0.0",
        implementationVersion = "impl-1",
        installed = true,
        available = health == RuntimeHealth.READY || health == RuntimeHealth.BUSY,
        health = health,
        locality = RuntimeLocality.LOCAL_SIDECAR,
        transport = RuntimeTransport.LOOPBACK_HTTP,
        providerControl = RuntimeControlOwner.RUNTIME,
        modelControl = RuntimeControlOwner.RUNTIME,
        supportsStreaming = true,
        supportsCancel = true,
        provenance = "test:$id",
    )
}
