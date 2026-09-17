package ai.zara.wear.todo

import ai.zara.wear.WearMainActivity
import android.app.PendingIntent
import android.content.Context
import android.content.Intent
import androidx.wear.watchface.complications.data.ComplicationData
import androidx.wear.watchface.complications.data.ComplicationType
import androidx.wear.watchface.complications.data.PlainComplicationText
import androidx.wear.watchface.complications.data.ShortTextComplicationData
import androidx.wear.watchface.complications.datasource.ComplicationRequest
import androidx.wear.watchface.complications.datasource.SuspendingComplicationDataSourceService

private const val ACTION_OPEN_TODOS = "ai.zara.action.OPEN_TODOS"
private const val EXTRA_ROUTE = "route"
private const val ROUTE_SCHEDULED = "scheduled"

abstract class ZaraTodoComplicationService : SuspendingComplicationDataSourceService() {
    protected abstract fun render(projection: TodoProjection): TodoComplicationText

    override suspend fun onComplicationRequest(request: ComplicationRequest): ComplicationData? =
        dataFor(request.complicationType, TodoProjectionStore.read(this))

    override fun getPreviewData(type: ComplicationType): ComplicationData? =
        dataFor(
            type,
            TodoProjection(
                nextTitle = "Review Zara PR",
                openCount = 5,
                topic = "zara/android",
                syncState = TodoSyncState.SYNCED,
                updatedAtEpochMillis = System.currentTimeMillis(),
            ),
        )

    private fun dataFor(type: ComplicationType, projection: TodoProjection): ComplicationData? {
        if (type != ComplicationType.SHORT_TEXT) return null
        val value = render(projection)
        return ShortTextComplicationData.Builder(
            text = PlainComplicationText.Builder(value.text).build(),
            contentDescription = PlainComplicationText.Builder("Zara Todos ${value.title} ${value.text}").build(),
        )
            .setTitle(PlainComplicationText.Builder(value.title).build())
            .setTapAction(openTodosPendingIntent(this))
            .build()
    }
}

class ZaraNextTodoComplicationService : ZaraTodoComplicationService() {
    override fun render(projection: TodoProjection): TodoComplicationText =
        TodoProjectionFormatter.next(projection)
}

class ZaraTodoSummaryComplicationService : ZaraTodoComplicationService() {
    override fun render(projection: TodoProjection): TodoComplicationText =
        TodoProjectionFormatter.summary(projection)
}

class ZaraTodoSyncComplicationService : ZaraTodoComplicationService() {
    override fun render(projection: TodoProjection): TodoComplicationText =
        TodoProjectionFormatter.sync(projection)
}

private fun openTodosPendingIntent(context: Context): PendingIntent =
    PendingIntent.getActivity(
        context,
        927,
        Intent(context, WearMainActivity::class.java)
            .setAction(ACTION_OPEN_TODOS)
            .putExtra(EXTRA_ROUTE, ROUTE_SCHEDULED),
        PendingIntent.FLAG_UPDATE_CURRENT or PendingIntent.FLAG_IMMUTABLE,
    )
