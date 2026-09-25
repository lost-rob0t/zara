package ai.zara.org.core

import java.time.LocalDateTime
import java.time.LocalTime

enum class OrgReminderKind {
    SCHEDULED,
    DEADLINE,
}

data class OrgReminderSpec(
    val taskPath: String,
    val taskLine: Int,
    val title: String,
    val taskState: String,
    val kind: OrgReminderKind,
    val whenLocal: LocalDateTime,
    val explicitTime: Boolean,
) {
    val stableKey: String =
        "$taskPath:$taskLine:${kind.name}:${whenLocal.toLocalDate()}:${whenLocal.toLocalTime()}"
}

object OrgReminders {
    val defaultReminderTime: LocalTime = LocalTime.of(9, 0)

    /**
     * Derive reminders only from the projection's canonical open-task set.
     *
     * Done/open semantics come from each ordinary Org file's own `#+TODO` /
     * `#+SEQ_TODO` declarations as resolved by [OrgWorkspaceProjector]. This
     * keeps reminder filtering from silently inheriting DoomOrgProfile states.
     */
    fun fromProjection(
        projection: OrgWorkspaceProjection,
        defaultTime: LocalTime = defaultReminderTime,
    ): List<OrgReminderSpec> = buildList {
        projection.openTasks.forEach { task ->
            task.scheduled?.let { date ->
                val explicit = task.scheduledTime != null
                add(
                    OrgReminderSpec(
                        taskPath = task.path,
                        taskLine = task.line,
                        title = task.title,
                        taskState = task.state,
                        kind = OrgReminderKind.SCHEDULED,
                        whenLocal = date.atTime(task.scheduledTime ?: defaultTime),
                        explicitTime = explicit,
                    ),
                )
            }
            task.deadline?.let { date ->
                val explicit = task.deadlineTime != null
                add(
                    OrgReminderSpec(
                        taskPath = task.path,
                        taskLine = task.line,
                        title = task.title,
                        taskState = task.state,
                        kind = OrgReminderKind.DEADLINE,
                        whenLocal = date.atTime(task.deadlineTime ?: defaultTime),
                        explicitTime = explicit,
                    ),
                )
            }
        }
    }.sortedWith(
        compareBy<OrgReminderSpec> { it.whenLocal }
            .thenBy { it.taskPath }
            .thenBy { it.taskLine }
            .thenBy { it.kind.ordinal },
    )
}
