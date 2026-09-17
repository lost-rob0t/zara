package ai.zara.wear.todo

import android.content.Context

enum class TodoSyncState {
    SYNCED,
    SYNCING,
    LOCAL_ONLY,
    OFFLINE,
    ERROR,
}

data class TodoProjection(
    val nextTitle: String?,
    val openCount: Int,
    val topic: String?,
    val syncState: TodoSyncState,
    val updatedAtEpochMillis: Long,
)

data class TodoComplicationText(
    val title: String,
    val text: String,
)

object TodoProjectionFormatter {
    const val MAX_TEXT_LENGTH = 20
    const val STALE_AFTER_MILLIS = 30L * 60L * 1_000L

    fun next(projection: TodoProjection): TodoComplicationText =
        TodoComplicationText(
            title = "NEXT",
            text = bounded(projection.nextTitle?.trim().orEmpty().ifBlank { "No next task" }),
        )

    fun summary(
        projection: TodoProjection,
        nowEpochMillis: Long = System.currentTimeMillis(),
    ): TodoComplicationText {
        val count = projection.openCount.coerceAtLeast(0)
        val topic = projection.topic?.trim().orEmpty()
        val text = if (topic.isBlank()) "$count open" else bounded("$count · $topic")
        return TodoComplicationText(
            title = syncLabel(projection, nowEpochMillis),
            text = text,
        )
    }

    fun sync(
        projection: TodoProjection,
        nowEpochMillis: Long = System.currentTimeMillis(),
    ): TodoComplicationText =
        TodoComplicationText(
            title = "SYNC",
            text = syncLabel(projection, nowEpochMillis),
        )

    private fun syncLabel(projection: TodoProjection, nowEpochMillis: Long): String {
        if (projection.updatedAtEpochMillis > 0L &&
            nowEpochMillis - projection.updatedAtEpochMillis > STALE_AFTER_MILLIS
        ) {
            return "STALE"
        }
        return when (projection.syncState) {
            TodoSyncState.SYNCED -> "SYNCED"
            TodoSyncState.SYNCING -> "SYNCING"
            TodoSyncState.LOCAL_ONLY -> "LOCAL"
            TodoSyncState.OFFLINE -> "OFFLINE"
            TodoSyncState.ERROR -> "ERROR"
        }
    }

    private fun bounded(value: String): String =
        if (value.length <= MAX_TEXT_LENGTH) value else value.take(MAX_TEXT_LENGTH - 1).trimEnd() + "…"
}

object TodoProjectionStore {
    private const val PREFS = "zara_todo_projection_v1"
    private const val KEY_NEXT = "next_title"
    private const val KEY_OPEN = "open_count"
    private const val KEY_TOPIC = "topic"
    private const val KEY_SYNC = "sync_state"
    private const val KEY_UPDATED = "updated_at"

    fun read(context: Context): TodoProjection {
        val prefs = context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
        return TodoProjection(
            nextTitle = prefs.getString(KEY_NEXT, null),
            openCount = prefs.getInt(KEY_OPEN, 0),
            topic = prefs.getString(KEY_TOPIC, null),
            syncState = runCatching {
                TodoSyncState.valueOf(prefs.getString(KEY_SYNC, TodoSyncState.LOCAL_ONLY.name).orEmpty())
            }.getOrDefault(TodoSyncState.LOCAL_ONLY),
            updatedAtEpochMillis = prefs.getLong(KEY_UPDATED, 0L),
        )
    }

    fun write(context: Context, projection: TodoProjection) {
        context.getSharedPreferences(PREFS, Context.MODE_PRIVATE)
            .edit()
            .putString(KEY_NEXT, projection.nextTitle?.trim())
            .putInt(KEY_OPEN, projection.openCount.coerceAtLeast(0))
            .putString(KEY_TOPIC, projection.topic?.trim())
            .putString(KEY_SYNC, projection.syncState.name)
            .putLong(KEY_UPDATED, projection.updatedAtEpochMillis)
            .apply()
    }
}
