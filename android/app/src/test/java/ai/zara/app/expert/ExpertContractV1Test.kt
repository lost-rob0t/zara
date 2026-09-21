package ai.zara.app.expert

import java.io.File
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Assert.fail
import org.junit.Test

class ExpertContractV1Test {
    @Test
    fun sharedFixtureRowsAllParse() {
        val rows = loadParsedFixtureRows()

        assertEquals(
            listOf(
                "zara:expert/android-troubleshooting",
                "zara:expert/prolog-rlm",
                "zara:expert/todo",
            ),
            rows.map { it.expertId }.sorted(),
        )
    }

    @Test
    fun selectableCatalogExcludesAbsentAndIncompatibleExperts() {
        val rows = loadFixtureRows()
        val selectable = rows.filter { it.selectable }.map { it.expertId }.sorted()

        assertEquals(listOf("zara:expert/android-troubleshooting", "zara:expert/todo"), selectable)
        assertFalse(rows.first { it.expertId == "zara:expert/prolog-rlm" }.selectable)
    }

    @Test
    fun incompatibleProtocolMajorFailsClosedWithFieldContext() {
        val futureRow = rawFixtureRows().first { it.getValue("expert_id") == "zara:expert/future" }
        val error = expectFailure<IllegalArgumentException> {
            ExpertDescriptorRow.fromTsvRow(futureRow)
        }
        assertTrue(error.message!!.contains("zara:expert/future"))
        assertTrue(error.message!!.contains("protocol"))
    }

    @Test
    fun unknownEnumValuesFailClosed() {
        assertEquals("symbolic", ReasoningKind.fromWire("symbolic").wire)
        expectFailure<IllegalArgumentException> { ReasoningKind.fromWire("neural") }
        expectFailure<IllegalArgumentException> { ExpertAvailability.fromWire("maybe") }
        expectFailure<IllegalArgumentException> { ExpertVerdict.fromWire("perhaps") }
        expectFailure<IllegalArgumentException> { LifecycleState.fromWire("dormant") }
        expectFailure<IllegalArgumentException> { ExpertErrorCode.fromWire("oops") }
    }

    @Test
    fun activationTransitionTableMirrorsPythonContract() {
        val valid = setOf(
            LifecycleState.INACTIVE to LifecycleState.ACTIVATING,
            LifecycleState.ACTIVATING to LifecycleState.ACTIVE,
            LifecycleState.ACTIVATING to LifecycleState.FAILED,
            LifecycleState.ACTIVE to LifecycleState.DRAINING,
            LifecycleState.ACTIVE to LifecycleState.UNAVAILABLE,
            LifecycleState.ACTIVE to LifecycleState.FAILED,
            LifecycleState.DRAINING to LifecycleState.INACTIVE,
            LifecycleState.UNAVAILABLE to LifecycleState.ACTIVE,
            LifecycleState.FAILED to LifecycleState.INACTIVE,
        )
        assertEquals(valid, ExpertContract.activationTransitions.flatMap { (from, tos) ->
            tos.map { from to it }
        }.toSet())
        assertEquals(valid, LifecycleState.entries.flatMap { from ->
            LifecycleState.entries.filter { to -> ExpertContract.transitionValid(from, to) }
                .map { from to it }
        }.toSet())
    }

    @Test
    fun limitsAdmissionMirrorsHostCeilings() {
        val ceilings = ExpertLimits(
            timeoutMs = ExpertContract.HOST_CEILING_TIMEOUT_MS,
            maxResults = ExpertContract.HOST_CEILING_MAX_RESULTS,
            maxOutputBytes = ExpertContract.HOST_CEILING_MAX_OUTPUT_BYTES,
            maxModelCalls = ExpertContract.HOST_CEILING_MAX_MODEL_CALLS,
        )
        assertTrue(ceilings.validAgainstCeilings())
        assertTrue(
            ExpertLimits(timeoutMs = 1, maxResults = 1, maxOutputBytes = 1, maxModelCalls = 0)
                .validAgainstCeilings(),
        )
        expectFailure<IllegalArgumentException> {
            ExpertLimits(timeoutMs = 0, maxResults = 1, maxOutputBytes = 1)
        }
        expectFailure<IllegalArgumentException> {
            ExpertLimits(
                timeoutMs = ExpertContract.HOST_CEILING_TIMEOUT_MS + 1,
                maxResults = 1,
                maxOutputBytes = 1,
            )
        }
        expectFailure<IllegalArgumentException> {
            ExpertLimits(
                timeoutMs = 1,
                maxResults = 1,
                maxOutputBytes = 1,
                maxModelCalls = ExpertContract.HOST_CEILING_MAX_MODEL_CALLS + 1,
            )
        }
    }

    @Test
    fun catalogProjectionWhitelistOmitsRegistrationInternals() {
        val row = loadParsedFixtureRows().first { it.expertId == "zara:expert/todo" }
        val projection = row.catalogProjection()

        assertEquals("zara:expert/todo", projection.expertId)
        assertEquals("symbolic", projection.reasoningKind)
        assertEquals(listOf("route.diagnose", "route.explain"), projection.operations)
        assertEquals(listOf("todo", "tasks", "reminders"), projection.applicabilityKeywords)
        assertTrue(projection.availability == "ready")
    }

    @Test
    fun verdictAndErrorCodeSetsMirrorPythonDeclarationOrder() {
        assertEquals(
            listOf(
                "succeeded", "failed", "unknown", "blocked",
                "unsupported", "cancelled", "error",
            ),
            ExpertVerdict.entries.map { it.wire },
        )
        assertEquals(
            listOf(
                "invalid_input", "ambiguity", "unsupported_operation", "unsupported_backend",
                "incompatible_protocol", "denied", "approval_required", "stale_generation",
                "unavailable", "deadline_exceeded", "budget_exceeded", "cancelled",
                "interrupted", "unknown_external_outcome",
            ),
            ExpertErrorCode.entries.map { it.wire },
        )
    }

    private fun loadFixtureRows(): List<ExpertDescriptorRow> = parseFixtureRows().second

    private fun loadParsedFixtureRows(): List<ExpertDescriptorRow> {
        val (incompatible, rows) = parseFixtureRows()
        assertEquals(listOf("zara:expert/future"), incompatible)
        return rows
    }

    private fun parseFixtureRows(): Pair<List<String>, List<ExpertDescriptorRow>> {
        val incompatible = mutableListOf<String>()
        val rows = mutableListOf<ExpertDescriptorRow>()
        for (row in rawFixtureRows()) {
            try {
                rows += ExpertDescriptorRow.fromTsvRow(row)
            } catch (error: IllegalArgumentException) {
                if (error.message?.contains("protocol") == true) {
                    incompatible += row.getValue("expert_id")
                } else {
                    throw error
                }
            }
        }
        return incompatible to rows
    }

    private fun rawFixtureRows(): List<Map<String, String>> {
        val file = sharedFixtureFile()
        val lines = file.readLines().filter { it.isNotBlank() }
        val header = lines.first().split('\t')
        return lines.drop(1).map { line ->
            val values = line.split('\t')
            require(values.size == header.size) { "malformed fixture row: $line" }
            header.indices.associate { header[it] to values[it] }
        }
    }

    private fun sharedFixtureFile(): File {
        var current = File(System.getProperty("user.dir")).canonicalFile
        repeat(6) {
            val candidate = File(current, "contracts/zara-expert-v1/descriptors.tsv")
            if (candidate.isFile) return candidate
            current = current.parentFile ?: return@repeat
        }
        error("shared ZARA-EXPERT/1 descriptors.tsv fixture not found from ${System.getProperty("user.dir")}")
    }

    private inline fun <reified T : Throwable> expectFailure(block: () -> Unit): T {
        try {
            block()
        } catch (error: Throwable) {
            if (error is T) return error
            throw error
        }
        fail("expected ${T::class.simpleName}")
        error("unreachable")
    }
}
