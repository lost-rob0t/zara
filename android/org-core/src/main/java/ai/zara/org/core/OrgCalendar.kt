package ai.zara.org.core

import java.time.LocalDate

/**
 * Source-preserving calendar projection over canonical Org task timestamps.
 *
 * This is derived state only. `SCHEDULED` and `DEADLINE` stay distinct so UI,
 * native-calendar bridges, alarms, and Prolog policy cannot accidentally turn a
 * queue placement into a final cutoff (or vice versa).
 */
enum class OrgOccurrenceKind {
    SCHEDULED,
    DEADLINE,
}

data class OrgOccurrence(
    val path: String,
    val line: Int,
    val title: String,
    val taskState: String,
    val kind: OrgOccurrenceKind,
    val date: LocalDate,
    val tags: Set<String> = emptySet(),
    val effort: String? = null,
    val category: String? = null,
)

object OrgCalendar {
    fun occurrences(tasks: Iterable<OrgTask>): List<OrgOccurrence> = buildList {
        tasks.forEach { task ->
            task.scheduled?.let { date ->
                add(task.toOccurrence(OrgOccurrenceKind.SCHEDULED, date))
            }
            task.deadline?.let { date ->
                add(task.toOccurrence(OrgOccurrenceKind.DEADLINE, date))
            }
        }
    }.sortedWith(
        compareBy<OrgOccurrence> { it.date }
            .thenBy { it.path }
            .thenBy { it.line }
            .thenBy { it.kind.ordinal },
    )

    fun between(
        tasks: Iterable<OrgTask>,
        startInclusive: LocalDate,
        endInclusive: LocalDate,
    ): List<OrgOccurrence> {
        require(!endInclusive.isBefore(startInclusive)) { "Calendar range is inverted" }
        return occurrences(tasks).filter { occurrence ->
            !occurrence.date.isBefore(startInclusive) && !occurrence.date.isAfter(endInclusive)
        }
    }

    fun groupedByDate(tasks: Iterable<OrgTask>): Map<LocalDate, List<OrgOccurrence>> =
        occurrences(tasks).groupBy { it.date }

    private fun OrgTask.toOccurrence(kind: OrgOccurrenceKind, date: LocalDate): OrgOccurrence =
        OrgOccurrence(
            path = path,
            line = line,
            title = title,
            taskState = state,
            kind = kind,
            date = date,
            tags = tags,
            effort = effort,
            category = category,
        )
}
