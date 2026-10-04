package ai.zara.org.core

import java.time.Clock
import java.time.LocalDate
import java.time.ZoneId
import java.time.format.DateTimeFormatter

/**
 * Derived workspace projection over canonical Org text.
 *
 * This layer owns no files, database, index authority, sync state, or UI-local
 * product state. Rebuild it from the configured Org corpus whenever needed.
 */
data class OrgDailySpec(
    val relativePathTemplate: String,
    val datePattern: String,
    val zoneId: ZoneId,
) {
    private val token = "{date}"
    private val tokenIndex = relativePathTemplate.indexOf(token)
    private val formatter = DateTimeFormatter.ofPattern(datePattern)

    init {
        require(tokenIndex >= 0) { "Daily path template must contain {date}" }
        require(relativePathTemplate.indexOf(token, tokenIndex + token.length) < 0) {
            "Daily path template must contain exactly one {date} token"
        }
        require(!relativePathTemplate.startsWith('/')) { "Daily path template must be workspace-relative" }
        require(relativePathTemplate.split('/').none { it == ".." }) {
            "Daily path template cannot traverse outside the workspace"
        }
    }

    private val prefix: String = relativePathTemplate.substring(0, tokenIndex)
    private val suffix: String = relativePathTemplate.substring(tokenIndex + token.length)

    fun pathFor(date: LocalDate): String = prefix + formatter.format(date) + suffix

    fun logicalDate(relativePath: String): LocalDate? {
        if (!relativePath.startsWith(prefix) || !relativePath.endsWith(suffix)) return null
        val dateTextEnd = relativePath.length - suffix.length
        if (dateTextEnd < prefix.length) return null
        val dateText = relativePath.substring(prefix.length, dateTextEnd)
        return runCatching { LocalDate.parse(dateText, formatter) }.getOrNull()
    }
}

data class OrgDailyEntry(
    val date: LocalDate,
    val path: String,
    val source: String,
)

data class OrgWorkspaceProjection(
    val tasks: List<OrgTask>,
    val roam: OrgRoamGraph,
    val dailies: List<OrgDailyEntry>,
    val today: LocalDate?,
    val todayPath: String?,
    private val doneStatesByPath: Map<String, Set<String>> = emptyMap(),
) {
    val openTasks: List<OrgTask>
        get() = tasks.filter { task -> task.state !in doneStatesByPath[task.path].orEmpty() }
}

object OrgWorkspaceProjector {
    fun project(
        documents: Map<String, String>,
        dailySpec: OrgDailySpec? = null,
        clock: Clock? = null,
    ): OrgWorkspaceProjection {
        val orderedDocuments = documents.toSortedMap()
        val tasks = orderedDocuments.flatMap { (path, source) ->
            OrgParser.parse(source, path).tasks
        }
        val doneStatesByPath = orderedDocuments.mapValues { (_, source) ->
            OrgParser.todoWorkflow(source).doneStates
        }
        val roam = OrgRoam.build(orderedDocuments)

        if (dailySpec == null) {
            return OrgWorkspaceProjection(
                tasks = tasks,
                roam = roam,
                dailies = emptyList(),
                today = null,
                todayPath = null,
                doneStatesByPath = doneStatesByPath,
            )
        }

        val effectiveClock = clock ?: Clock.system(dailySpec.zoneId)
        val today = LocalDate.now(effectiveClock)
        val dailies = orderedDocuments.mapNotNull { (path, source) ->
            dailySpec.logicalDate(path)
                ?.takeIf { date -> !date.isAfter(today) }
                ?.let { date -> OrgDailyEntry(date, path, source) }
        }.sortedWith(compareByDescending<OrgDailyEntry> { it.date }.thenBy { it.path })

        return OrgWorkspaceProjection(
            tasks = tasks,
            roam = roam,
            dailies = dailies,
            today = today,
            todayPath = dailySpec.pathFor(today),
            doneStatesByPath = doneStatesByPath,
        )
    }
}
