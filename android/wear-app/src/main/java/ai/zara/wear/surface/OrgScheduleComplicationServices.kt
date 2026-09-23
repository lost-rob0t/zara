package ai.zara.wear.surface

import androidx.wear.watchface.complications.data.ComplicationData
import androidx.wear.watchface.complications.data.ComplicationType
import androidx.wear.watchface.complications.data.NoDataComplicationData
import androidx.wear.watchface.complications.data.PlainComplicationText
import androidx.wear.watchface.complications.data.RangedValueComplicationData
import androidx.wear.watchface.complications.data.ShortTextComplicationData
import androidx.wear.watchface.complications.datasource.ComplicationRequest
import androidx.wear.watchface.complications.datasource.SuspendingComplicationDataSourceService

abstract class OrgScheduleLaneComplicationService(
    private val laneIndex: Int,
) : SuspendingComplicationDataSourceService() {
    final override suspend fun onComplicationRequest(request: ComplicationRequest): ComplicationData? {
        if (request.complicationType != ComplicationType.RANGED_VALUE) return null
        val allocation = OrgScheduleSnapshotStore.read(this).allocations.getOrNull(laneIndex)
            ?: return NoDataComplicationData()
        return allocationData(allocation)
    }

    final override fun getPreviewData(type: ComplicationType): ComplicationData? {
        if (type != ComplicationType.RANGED_VALUE) return null
        return allocationData(previewSnapshot().allocations[laneIndex])
    }

    private fun allocationData(allocation: OrgScheduleAllocation): ComplicationData {
        val description = PlainComplicationText.Builder(
            "${allocation.title}, ${allocation.startMinute} to ${allocation.endMinute} minutes",
        ).build()
        return RangedValueComplicationData.Builder(
            allocation.startMinute.toFloat(),
            allocation.startMinute.toFloat(),
            allocation.endMinute.toFloat(),
            description,
        )
            .setText(PlainComplicationText.Builder(allocation.title.take(7)).build())
            .setTapAction(ZaraWearLaunchTargets.orgTodoPendingIntent(this, allocation.id))
            .build()
    }
}

open class OrgNextTodoComplicationService : SuspendingComplicationDataSourceService() {
    override suspend fun onComplicationRequest(request: ComplicationRequest): ComplicationData? {
        if (request.complicationType != ComplicationType.SHORT_TEXT) return null
        val snapshot = OrgScheduleSnapshotStore.read(this)
        val title = snapshot.currentOrNextTitle ?: return NoDataComplicationData()
        val todoId = snapshot.currentOrNextId ?: return NoDataComplicationData()
        return shortTextData(title, todoId)
    }

    override fun getPreviewData(type: ComplicationType): ComplicationData? {
        if (type != ComplicationType.SHORT_TEXT) return null
        val snapshot = previewSnapshot()
        return shortTextData(
            requireNotNull(snapshot.currentOrNextTitle),
            requireNotNull(snapshot.currentOrNextId),
        )
    }

    private fun shortTextData(title: String, todoId: String): ComplicationData {
        val text = PlainComplicationText.Builder(title.take(7)).build()
        val description = PlainComplicationText.Builder(title).build()
        return ShortTextComplicationData.Builder(text, description)
            .setTapAction(ZaraWearLaunchTargets.orgTodoPendingIntent(this, todoId))
            .build()
    }
}

private fun previewSnapshot(): OrgScheduleSnapshot {
    val starts = listOf(30, 115, 205, 310, 430, 575)
    val durations = listOf(55, 45, 70, 35, 60, 50)
    val titles = listOf("Focus", "Review", "Build", "Docs", "Walk", "Plan")
    val allocations = starts.indices.map { index ->
        OrgScheduleAllocation(
            id = "preview-$index",
            title = titles[index],
            status = if (index == 0) "STRT" else "TODO",
            startMinute = starts[index],
            endMinute = starts[index] + durations[index],
            priority = if (index == 0) "A" else null,
            tags = listOf("preview"),
            source = "agenda/preview.org",
        )
    }
    return OrgScheduleSnapshot(
        generatedAtEpochMillis = 0L,
        allocations = allocations,
        currentOrNextId = "preview-0",
        currentOrNextTitle = "Focus",
    )
}
