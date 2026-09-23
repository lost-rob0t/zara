package ai.zara.wear.surface

import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertNull
import org.junit.Assert.assertTrue
import org.junit.Test

class OrgScheduleSnapshotTest {
    @Test
    fun codecRoundTripsCanonicalAllocationMetadata() {
        val todoId = "5021ae0b-6b2f-4dc2-92c9-cc79b8ed1ed6"
        val snapshot = OrgScheduleSnapshot(
            generatedAtEpochMillis = 1_789_614_000_000L,
            allocations = listOf(
                OrgScheduleAllocation(
                    id = todoId,
                    title = "Build Org face | verify",
                    status = "STRT",
                    startMinute = 360,
                    endMinute = 450,
                    priority = "A",
                    tags = listOf("todo", "wear os"),
                    source = "agenda/zara.org",
                ),
            ),
            currentOrNextId = todoId,
            currentOrNextTitle = "Build Org face",
        )

        assertEquals(snapshot, OrgScheduleSnapshotCodec.decode(OrgScheduleSnapshotCodec.encode(snapshot)))
    }

    @Test
    fun malformedFutureOrIncompleteSnapshotsFailClosed() {
        assertNull(OrgScheduleSnapshotCodec.decode(""))
        assertNull(OrgScheduleSnapshotCodec.decode("schema=2\ngenerated=1\n"))
        assertNull(OrgScheduleSnapshotCodec.decode("schema=1\nnext=Task\n"))
        assertNull(OrgScheduleSnapshotCodec.decode("schema=1\ngenerated=1\ngenerated=2\n"))
        assertNull(OrgScheduleSnapshotCodec.decode("schema=1\ngenerated=1\nnext=Task\n"))
        assertNull(OrgScheduleSnapshotCodec.decode("schema=1\ngenerated=1\nnext_id=id\n"))
        assertNull(
            OrgScheduleSnapshotCodec.decode(
                "schema=1\ngenerated=1\nallocation=id|task|TODO|500|400||||\n",
            ),
        )
    }

    @Test
    fun duplicateStableIdsFailClosed() {
        val duplicate = """
            schema=1
            generated=1
            allocation=same|one|TODO|10|20|||
            allocation=same|two|TODO|30|40|||
        """.trimIndent()

        assertNull(OrgScheduleSnapshotCodec.decode(duplicate))
    }

    @Test
    fun currentOrNextTodoRequiresStableAllocationIdentity() {
        val allocation = OrgScheduleAllocation(
            id = "task-1",
            title = "Task",
            status = "TODO",
            startMinute = 10,
            endMinute = 20,
            priority = null,
            tags = emptyList(),
            source = null,
        )

        assertTrue(
            runCatching {
                OrgScheduleSnapshot(1L, listOf(allocation), null, "Task")
            }.isFailure,
        )
        assertTrue(
            runCatching {
                OrgScheduleSnapshot(1L, listOf(allocation), "task-1", null)
            }.isFailure,
        )
        assertTrue(
            runCatching {
                OrgScheduleSnapshot(1L, listOf(allocation), "missing", "Task")
            }.isFailure,
        )
        assertEquals(
            "task-1",
            OrgScheduleSnapshot(1L, listOf(allocation), "task-1", "Task").currentOrNextId,
        )
    }

    @Test
    fun staleOrConflictingSnapshotCannotRollBackAcceptedCacheState() {
        val current = OrgScheduleSnapshot(
            generatedAtEpochMillis = 200L,
            allocations = emptyList(),
            currentOrNextId = null,
            currentOrNextTitle = null,
        )
        val newer = current.copy(generatedAtEpochMillis = 201L)
        val identical = current.copy()
        val conflictingSameGeneration = current.copy(allocations = listOf(
            OrgScheduleAllocation(
                id = "conflict",
                title = "Conflict",
                status = "TODO",
                startMinute = 1,
                endMinute = 2,
                priority = null,
                tags = emptyList(),
                source = null,
            ),
        ))
        val stale = current.copy(generatedAtEpochMillis = 199L)

        assertTrue(shouldAcceptOrgScheduleSnapshot(current, newer))
        assertTrue(shouldAcceptOrgScheduleSnapshot(current, identical))
        assertFalse(shouldAcceptOrgScheduleSnapshot(current, conflictingSameGeneration))
        assertFalse(shouldAcceptOrgScheduleSnapshot(current, stale))
    }

    @Test
    fun allocationContractIsHalfDayAndSixLaneBounded() {
        val valid = OrgScheduleAllocation(
            id = "a",
            title = "Task",
            status = "TODO",
            startMinute = 0,
            endMinute = 720,
            priority = null,
            tags = emptyList(),
            source = null,
        )
        assertEquals(720, valid.endMinute)

        val tooMany = List(7) { index ->
            valid.copy(id = "a$index", startMinute = index, endMinute = index + 1)
        }
        assertTrue(
            runCatching { OrgScheduleSnapshot(1L, tooMany, null, null) }.isFailure,
        )
    }
}
