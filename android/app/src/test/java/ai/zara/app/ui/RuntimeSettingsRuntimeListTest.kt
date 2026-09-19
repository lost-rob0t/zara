package ai.zara.app.ui

import ai.zara.app.runtime.RuntimeHealth
import ai.zara.app.runtime.RuntimeLocality
import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Test

class RuntimeSettingsRuntimeListTest {
    @Test
    fun evidenceTextCarriesCanonicalRuntimeIdentityAndDiagnostics() {
        val row = row(
            runtimeId = "runtime-one",
            displayName = "Runtime One",
            runtimeVersion = "2.4.1",
            implementationVersion = "impl-17",
            health = RuntimeHealth.DEGRADED,
            locality = RuntimeLocality.LOCAL_SIDECAR,
            profiles = listOf("planner", "agent"),
            selectable = false,
            selected = false,
        )

        assertEquals(
            "Runtime One · id runtime-one · runtime 2.4.1 · implementation impl-17 · " +
                "health degraded · locality local_sidecar · profiles planner,agent · " +
                "selectable false · selected false",
            runtimeSettingsEvidenceText(row),
        )
    }

    @Test
    fun selectionTargetIsStableRuntimeIdOnly() {
        val row = row(runtimeId = "runtime-one", displayName = "Renamed Runtime")

        assertEquals("runtime-one", runtimeSettingsSelectionTarget(row))
    }

    @Test
    fun composeListDoesNotNameOptionalRuntimeOrOwnRoutingPolicy() {
        val source = File("src/main/java/ai/zara/app/ui/RuntimeSettingsRuntimeList.kt").readText()

        assertFalse(source.contains("prolog-rlm", ignoreCase = true))
        assertFalse(source.contains("agentprolog", ignoreCase = true))
        assertFalse(source.contains("RuntimeMode"))
        assertFalse(source.contains("Auto prefers"))
        assertFalse(source.contains("Remote fails"))
        assertTrue(source.contains("enabled = row.selectable"))
        assertTrue(source.contains("onSelectRuntime(runtimeSettingsSelectionTarget(row))"))
    }

    private fun row(
        runtimeId: String,
        displayName: String,
        runtimeVersion: String = "1.0.0",
        implementationVersion: String = "impl-1",
        health: RuntimeHealth = RuntimeHealth.READY,
        locality: RuntimeLocality = RuntimeLocality.EMBEDDED,
        profiles: List<String> = emptyList(),
        selectable: Boolean = true,
        selected: Boolean = true,
    ) = RuntimeSettingsRuntimeRow(
        runtimeId = runtimeId,
        displayName = displayName,
        runtimeVersion = runtimeVersion,
        implementationVersion = implementationVersion,
        health = health,
        locality = locality,
        profiles = profiles,
        selectable = selectable,
        selected = selected,
    )
}
