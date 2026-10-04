package ai.zara.org.core

import java.time.Duration

data class OrgTimerTemplate(
    val taskPath: String,
    val taskLine: Int,
    val name: String,
    val duration: Duration,
) {
    val stableKey: String = "$taskPath:$taskLine:$name:${duration.seconds}"
}

object OrgTimers {
    /**
     * Derive timers only from canonical open tasks.
     *
     * The projection resolves each file's own `#+TODO` / `#+SEQ_TODO` workflow,
     * so timer eligibility never falls back to DoomOrgProfile done-state guesses.
     */
    fun fromProjection(projection: OrgWorkspaceProjection): List<OrgTimerTemplate> =
        projection.openTasks.asSequence()
            .filter { task -> "timer" in task.tags }
            .mapNotNull { task ->
                val duration = task.effort?.let(::parseEffort) ?: return@mapNotNull null
                OrgTimerTemplate(
                    taskPath = task.path,
                    taskLine = task.line,
                    name = task.title,
                    duration = duration,
                )
            }
            .sortedBy { it.name.lowercase() }
            .toList()

    fun parseEffort(value: String): Duration? {
        val match = Regex("^([0-9]+):([0-5][0-9])$").matchEntire(value.trim()) ?: return null
        val hours = match.groupValues[1].toLongOrNull() ?: return null
        val minutes = match.groupValues[2].toLongOrNull() ?: return null
        val duration = Duration.ofHours(hours).plusMinutes(minutes)
        return duration.takeIf { !it.isZero && !it.isNegative }
    }
}
