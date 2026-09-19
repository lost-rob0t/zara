package ai.zara.app.runtime

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class RuntimeRegistryV1Test {
    @Test
    fun sharedFixtureExposesOnlyInstalledCompatibleRuntimeAsSelectable() {
        val registry = RuntimeRegistry()
        val snapshot = registry.refresh(loadFixtureDescriptors())

        assertEquals(
            listOf("future-runtime", "prolog-rlm", "zara-python"),
            snapshot.descriptors.map { it.id },
        )
        assertEquals(listOf("zara-python"), registry.selectable().map { it.id })
        assertEquals(ZARA_RUNTIME_PROTOCOL, registry.capabilities("zara-python").protocol)
        assertFalse(registry.capabilities("prolog-rlm").selectable)
    }

    @Test
    fun absentOptionalRuntimeIsNotSelectable() {
        val registry = RuntimeRegistry()
        registry.refresh(loadFixtureDescriptors())

        expectFailure<RuntimeUnavailable> { registry.select("prolog-rlm") }
    }

    @Test
    fun unknownProtocolMajorFailsClosed() {
        val registry = RuntimeRegistry()
        registry.refresh(loadFixtureDescriptors())

        val error = expectFailure<IncompatibleRuntimeProtocol> {
            registry.select("future-runtime")
        }
        assertTrue(error.message.orEmpty().contains("ZARA-RUNTIME/2"))
    }

    @Test
    fun autoIsRoutingPolicyNotRuntimeIdentity() {
        val registry = RuntimeRegistry()
        registry.refresh(loadFixtureDescriptors())

        val error = expectFailure<RuntimeUnavailable> { registry.select("auto") }
        assertTrue(error.message.orEmpty().contains("routing policy"))
    }

    @Test
    fun duplicateDiscoveryFailsAtomically() {
        val registry = RuntimeRegistry()
        val fixture = loadFixtureDescriptors()
        val original = registry.refresh(fixture)

        expectFailure<IllegalArgumentException> {
            registry.refresh(fixture + fixture.first())
        }

        assertEquals(original, registry.snapshot())
    }

    @Test
    fun identicalDiscoveryRefreshIsGenerationNoopAndPreservesBinding() {
        val registry = RuntimeRegistry()
        val fixture = loadFixtureDescriptors()
        registry.refresh(fixture)
        registry.select("zara-python")
        val binding = registry.bindInvocation(
            contextRef = "ctx:turn-1",
            capabilityRefs = listOf("cap:tool-42"),
        )
        val before = registry.snapshot()

        val after = registry.refresh(fixture.reversed())

        assertEquals(before, after)
        assertTrue(registry.acceptsBinding(binding, contextRef = "ctx:turn-1"))
    }

    @Test
    fun refreshInvalidatesSelectionGenerationWhenRuntimeDisappears() {
        val registry = RuntimeRegistry()
        val fixture = loadFixtureDescriptors()
        registry.refresh(fixture)
        val selected = registry.select("zara-python")

        assertTrue(registry.acceptsGeneration("zara-python", selected.generation))

        val refreshed = registry.refresh(fixture.filterNot { it.id == "zara-python" })

        assertNull(registry.current())
        assertFalse(registry.acceptsGeneration("zara-python", selected.generation))
        assertTrue(refreshed.generation > selected.generation)
    }

    @Test
    fun runtimeDeathInvalidatesSelectionAndStaleGeneration() {
        val registry = RuntimeRegistry()
        val fixture = loadFixtureDescriptors()
        registry.refresh(fixture)
        val selected = registry.select("zara-python")

        val dead = fixture.map { descriptor ->
            if (descriptor.id == "zara-python") {
                descriptor.copy(available = false, health = RuntimeHealth.FAILED)
            } else {
                descriptor
            }
        }
        val refreshed = registry.refresh(dead)

        assertNull(registry.current())
        assertEquals(RuntimeHealth.FAILED, registry.health("zara-python"))
        assertFalse(registry.acceptsGeneration("zara-python", selected.generation))
        assertTrue(refreshed.generation > selected.generation)
    }

    @Test
    fun invocationBindingRequiresSelectedRuntimeAndExactHostContext() {
        val registry = RuntimeRegistry()
        val fixture = loadFixtureDescriptors()
        registry.refresh(fixture)

        expectFailure<RuntimeUnavailable> { registry.bindInvocation("ctx:turn-1") }

        val selected = registry.select("zara-python")
        val binding = registry.bindInvocation(
            contextRef = "ctx:turn-1",
            capabilityRefs = listOf("cap:tool-42", "cap:memory-read"),
        )

        assertEquals(selected.runtimeId, binding.runtimeId)
        assertEquals(selected.generation, binding.generation)
        assertTrue(registry.acceptsBinding(binding, contextRef = "ctx:turn-1"))
        assertFalse(registry.acceptsBinding(binding, contextRef = "ctx:turn-2"))

        val changed = fixture.map { descriptor ->
            if (descriptor.id == "zara-python") {
                descriptor.copy(health = RuntimeHealth.DEGRADED)
            } else {
                descriptor
            }
        }
        registry.refresh(changed)

        assertFalse(registry.acceptsBinding(binding, contextRef = "ctx:turn-1"))
    }

    @Test
    fun invocationBindingRejectsWrongRuntimeIdentity() {
        val registry = RuntimeRegistry()
        registry.refresh(loadFixtureDescriptors())
        registry.select("zara-python")
        val binding = registry.bindInvocation("ctx:turn-1")

        val forged = binding.copy(runtimeId = "prolog-rlm")

        assertFalse(registry.acceptsBinding(forged, contextRef = "ctx:turn-1"))
    }

    @Test
    fun invocationBindingRejectsUnboundedOrAuthorityBearingContextRefs() {
        val registry = RuntimeRegistry()
        registry.refresh(loadFixtureDescriptors())
        registry.select("zara-python")
        val unsafeValues = listOf(
            "",
            "turn-1",
            "principal:user-1",
            "ctx:turn?api_key=secret",
            "ctx:../../secrets",
        )

        unsafeValues.forEach { unsafe ->
            val error = expectFailure<IllegalArgumentException> {
                registry.bindInvocation(unsafe)
            }
            assertTrue(error.message.orEmpty().contains("context_ref"))
        }
    }

    @Test
    fun invocationBindingCarriesOnlyOpaqueHostCapabilityRefs() {
        val registry = RuntimeRegistry()
        registry.refresh(loadFixtureDescriptors())
        registry.select("zara-python")
        val unsafeValues = listOf(
            listOf("shell:exec"),
            listOf("filesystem:read"),
            listOf("secret:openai"),
            listOf("principal:admin"),
            listOf("plugin:registry"),
            listOf("cap:tool?token=secret"),
            listOf("cap:tool-42", "cap:tool-42"),
        )

        unsafeValues.forEach { unsafe ->
            val error = expectFailure<IllegalArgumentException> {
                registry.bindInvocation("ctx:turn-1", capabilityRefs = unsafe)
            }
            assertTrue(error.message.orEmpty().contains("capability_refs"))
        }
    }

    @Test
    fun secretBearingProvenanceIsRejected() {
        val descriptor = loadFixtureDescriptors().first { it.id == "zara-python" }
        val unsafeValues = listOf(
            "https://user:secret@host/runtime",
            "provider:token=abc123",
            "env:OPENAI_API_KEY=secret",
            "runtime:source?api_key=secret",
        )

        unsafeValues.forEach { unsafe ->
            val error = expectFailure<IllegalArgumentException> {
                descriptor.copy(provenance = unsafe)
            }
            assertTrue(error.message.orEmpty().contains("provenance"))
        }
    }

    @Test
    fun boundedTextCountsUnicodeScalarsLikeSharedSchema() {
        val descriptor = loadFixtureDescriptors().first { it.id == "zara-python" }
        val scalar = "\uD83E\uDD16"
        val exactLimit = scalar.repeat(64)
        val overLimit = scalar.repeat(65)

        assertEquals(64, exactLimit.codePointCount(0, exactLimit.length))
        assertEquals(exactLimit, descriptor.copy(runtimeVersion = exactLimit).runtimeVersion)

        val error = expectFailure<IllegalArgumentException> {
            descriptor.copy(runtimeVersion = overLimit)
        }
        assertTrue(error.message.orEmpty().contains("runtimeVersion exceeds 64 characters"))
    }

    private fun loadFixtureDescriptors(): List<RuntimeDescriptor> {
        val records = sharedFixtureFile().readLines(Charsets.UTF_8).filter { it.isNotBlank() }
        val header = records.first().split('\t')
        return records.drop(1).map { line ->
            val values = line.split('\t')
            require(values.size == header.size) { "invalid shared runtime fixture row" }
            val row = header.zip(values).toMap()
            RuntimeDescriptor(
                id = row.getValue("id"),
                displayName = row.getValue("display_name"),
                protocol = row.getValue("protocol"),
                runtimeVersion = row.getValue("runtime_version"),
                implementationVersion = row.getValue("implementation_version"),
                installed = row.boolean("installed"),
                available = row.boolean("available"),
                health = RuntimeHealth.fromWire(row.getValue("health")),
                locality = RuntimeLocality.fromWire(row.getValue("locality")),
                transport = RuntimeTransport.fromWire(row.getValue("transport")),
                capabilities = row.items("capabilities"),
                profiles = row.items("profiles"),
                providerControl = RuntimeControlOwner.fromWire(row.getValue("provider_control")),
                modelControl = RuntimeControlOwner.fromWire(row.getValue("model_control")),
                supportsStreaming = row.boolean("supports_streaming"),
                supportsCancel = row.boolean("supports_cancel"),
                supportsContextHandles = row.boolean("supports_context_handles"),
                supportsHostTools = row.boolean("supports_host_tools"),
                provenance = row.getValue("provenance"),
            )
        }
    }

    private fun sharedFixtureFile(): File {
        var current = File(System.getProperty("user.dir")).canonicalFile
        repeat(6) {
            val candidate = File(current, "contracts/zara-runtime-v1/descriptors.tsv")
            if (candidate.isFile) return candidate
            current = current.parentFile ?: return@repeat
        }
        error("shared ZARA-RUNTIME/1 descriptors.tsv fixture not found from ${System.getProperty("user.dir")}")
    }

    private fun Map<String, String>.boolean(key: String): Boolean = when (val value = getValue(key)) {
        "true" -> true
        "false" -> false
        else -> error("invalid fixture boolean $key=$value")
    }

    private fun Map<String, String>.items(key: String): List<String> {
        val value = getValue(key)
        return if (value == "-" || value.isEmpty()) emptyList() else value.split(',')
    }

    private inline fun <reified T : Throwable> expectFailure(block: () -> Unit): T {
        try {
            block()
        } catch (error: Throwable) {
            if (error is T) return error
            throw error
        }
        fail("expected ${T::class.java.simpleName}")
        throw AssertionError("unreachable")
    }
}
