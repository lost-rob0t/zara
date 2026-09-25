package ai.zara.org.core

import java.time.LocalDate
import java.time.LocalTime

/** Optional Doom-compatible workflow profile. Ordinary Org text/config remains authoritative. */
object DoomOrgProfile {
    const val orgRoot = "Configured Org workspace"
    const val agendaDirectory = "agenda"
    const val ideasFile = "ideas.org"

    val todoStates = listOf("TODO", "STRT", "LOOP", "WAIT", "IDEA", "PROJ", "DONE", "NO")
    val openStates = todoStates.takeWhile { it != "DONE" }
    val doneStates = setOf("DONE", "NO")
    val effortChoices = listOf("0:30", "1:00", "1:30", "2:00")
    val categoryChoices = listOf("Misc", "Work", "Education", "Bug Bounty", "Personal Task")
    val babelLanguages = setOf("python", "prolog")

    fun nextTodoState(current: String?): String {
        val index = todoStates.indexOf(current)
        return if (index < 0) todoStates.first() else todoStates[(index + 1) % todoStates.size]
    }

    fun captureTodo(
        title: String,
        effort: String = effortChoices.first(),
        category: String = categoryChoices.first(),
        scheduled: String? = null,
        deadline: String? = null,
    ): String = buildString {
        append("* TODO ").append(title.trim()).append('\n')
        append(":PROPERTIES:\n")
        append(":Effort: ").append(effort).append('\n')
        append(":CATEGORY: ").append(category).append('\n')
        append(":END:\n")
        scheduled?.takeIf { it.isNotBlank() }?.let { append("SCHEDULED: ").append(toTimestamp(it)).append('\n') }
        deadline?.takeIf { it.isNotBlank() }?.let { append("DEADLINE: ").append(toTimestamp(it)).append('\n') }
    }

    private fun toTimestamp(value: String): String {
        val text = value.trim()
        return if (text.startsWith("<") || text.startsWith("[")) text else "<$text>"
    }
}

data class OrgTask(
    val path: String,
    val line: Int,
    val level: Int,
    val state: String,
    val title: String,
    val priority: Char? = null,
    val tags: Set<String> = emptySet(),
    val scheduled: LocalDate? = null,
    val scheduledTime: LocalTime? = null,
    val deadline: LocalDate? = null,
    val deadlineTime: LocalTime? = null,
    val effort: String? = null,
    val category: String? = null,
)

data class OrgSourceBlock(
    val language: String,
    val headers: Map<String, String>,
    val body: String,
    val startLine: Int,
)

data class OrgDocument(
    val title: String?,
    val tasks: List<OrgTask>,
    val sourceBlocks: List<OrgSourceBlock>,
)

enum class AgendaGroup(val label: String) {
    OVERDUE("⚠ Overdue"),
    TODAY("📆 Today"),
    HIGH_PRIORITY("🔥 High priority"),
    STARINTEL("🧠 StarIntel / Temple"),
    ORG_PARSER("📚 Org parser"),
    HACKMODE("🕵 Hackmode / bug bounty"),
    EMACS("⌨ Emacs / editor / LLM tooling"),
    LISP("λ Lisp / command-server"),
    WORK("💼 Work shifts"),
    HOME("🏠 Home / maintenance"),
    LOOPS("🔁 Loops / recurring"),
    APPOINTMENTS("🏛 Appointments"),
    WAITING("⏳ Waiting"),
    IN_PROGRESS("▶ In progress"),
    IDEAS("💡 Ideas / design"),
    NOT_DOING("🚫 Not doing"),
    INBOX("📥 Inbox / unscheduled"),
    DONE("✅ Done"),
    OTHER("Other"),
}

object DoomAgenda {
    fun group(task: OrgTask, today: LocalDate = LocalDate.now()): AgendaGroup = when {
        task.deadline?.isBefore(today) == true || task.scheduled?.isBefore(today) == true -> AgendaGroup.OVERDUE
        task.deadline == today || task.scheduled == today -> AgendaGroup.TODAY
        task.priority == 'A' -> AgendaGroup.HIGH_PRIORITY
        task.tags.any { it == "StarIntel" || it == "Temple" } -> AgendaGroup.STARINTEL
        "org_parser" in task.tags -> AgendaGroup.ORG_PARSER
        task.tags.any { it == "hackmode" || it == "hackmode_expert" } -> AgendaGroup.HACKMODE
        "emacs" in task.tags -> AgendaGroup.EMACS
        task.tags.any { it == "lisp" || it == "command-server" } -> AgendaGroup.LISP
        "work" in task.tags -> AgendaGroup.WORK
        task.tags.any { it == "cleaning" || it == "home" || it == "maintenance" } -> AgendaGroup.HOME
        task.state == "LOOP" -> AgendaGroup.LOOPS
        "apt" in task.tags -> AgendaGroup.APPOINTMENTS
        task.state == "WAIT" -> AgendaGroup.WAITING
        task.state == "STRT" -> AgendaGroup.IN_PROGRESS
        task.state == "IDEA" -> AgendaGroup.IDEAS
        task.state == "NO" -> AgendaGroup.NOT_DOING
        task.state == "TODO" && (task.scheduled == null || !task.scheduled.isAfter(today)) -> AgendaGroup.INBOX
        task.state == "DONE" -> AgendaGroup.DONE
        else -> AgendaGroup.OTHER
    }

    fun grouped(tasks: List<OrgTask>, today: LocalDate = LocalDate.now()): Map<AgendaGroup, List<OrgTask>> =
        tasks.groupBy { group(it, today) }
}

object OrgParser {
    val defaultTodoStates: List<String> = listOf("TODO", "DONE")

    private val heading = Regex("^(\\*+)\\s+(.*)$")
    private val priority = Regex("^\\[#([A-Z])](?:\\s+|$)")
    private val tags = Regex("\\s+:([A-Za-z0-9_@#%:.-]+):\\s*$")
    private val property = Regex("^:([^:]+):\\s*(.*)$")
    private val timestamp = Regex(
        "(?:<|\\[)([0-9]{4}-[0-9]{2}-[0-9]{2})(?:\\s+[A-Za-z]{3})?(?:\\s+([0-9]{2}:[0-9]{2}))?",
    )
    private val sourceBegin = Regex("(?i)^#\\+begin_src\\s+(\\S+)(.*)$")
    private val sourceEnd = Regex("(?i)^#\\+end_src\\s*$")
    private val titleLine = Regex("(?i)^#\\+title:\\s*(.*)$")

    fun todoStates(
        source: String,
        fallbackTodoStates: List<String> = defaultTodoStates,
    ): List<String> =
        parseOrgTodoSequences(source, fallbackTodoStates)
            .flatMap { it.states }
            .distinct()

    fun nextTodoState(
        source: String,
        current: String?,
        fallbackTodoStates: List<String> = defaultTodoStates,
    ): String {
        val sequences = parseOrgTodoSequences(source, fallbackTodoStates)
        val active = sequences.firstOrNull { sequence -> current in sequence.states }
            ?: return sequences.first().states.first()
        val index = active.states.indexOf(current)
        return active.states[(index + 1) % active.states.size]
    }

    fun parse(
        source: String,
        path: String = "",
        fallbackTodoStates: List<String> = defaultTodoStates,
    ): OrgDocument {
        val lines = source.lines()
        val resolvedTodoStates = todoStates(source, fallbackTodoStates)
        val tasks = mutableListOf<OrgTask>()
        val blocks = mutableListOf<OrgSourceBlock>()
        var documentTitle: String? = null
        var index = 0

        while (index < lines.size) {
            val line = lines[index]
            titleLine.matchEntire(line)?.let { documentTitle = it.groupValues[1].trim() }

            val sourceMatch = sourceBegin.matchEntire(line)
            if (sourceMatch != null) {
                val language = sourceMatch.groupValues[1].lowercase()
                val headers = parseHeaders(sourceMatch.groupValues[2])
                val body = StringBuilder()
                val startLine = index + 1
                index += 1
                while (index < lines.size && !sourceEnd.matches(lines[index])) {
                    if (body.isNotEmpty()) body.append('\n')
                    body.append(lines[index])
                    index += 1
                }
                blocks += OrgSourceBlock(language, headers, body.toString(), startLine)
                index += 1
                continue
            }

            val headingMatch = heading.matchEntire(line)
            if (headingMatch != null) {
                val level = headingMatch.groupValues[1].length
                var text = headingMatch.groupValues[2].trim()
                val state = resolvedTodoStates.firstOrNull { candidate ->
                    text == candidate || text.startsWith("$candidate ")
                }
                if (state != null) {
                    text = text.removePrefix(state).trimStart()
                    var taskPriority: Char? = null
                    priority.find(text)?.let { match ->
                        taskPriority = match.groupValues[1].single()
                        text = text.removeRange(match.range).trimStart()
                    }
                    var taskTags = emptySet<String>()
                    tags.find(text)?.let { match ->
                        taskTags = match.groupValues[1].split(':').filter { it.isNotBlank() }.toSet()
                        text = text.removeRange(match.range).trimEnd()
                    }

                    var scheduled: LocalDate? = null
                    var scheduledTime: LocalTime? = null
                    var deadline: LocalDate? = null
                    var deadlineTime: LocalTime? = null
                    var effort: String? = null
                    var category: String? = null
                    var scan = index + 1
                    while (scan < lines.size && heading.matchEntire(lines[scan]) == null) {
                        val bodyLine = lines[scan].trim()
                        when {
                            bodyLine.startsWith("SCHEDULED:", ignoreCase = true) -> {
                                scheduled = parseDate(bodyLine)
                                scheduledTime = parseTime(bodyLine)
                            }
                            bodyLine.startsWith("DEADLINE:", ignoreCase = true) -> {
                                deadline = parseDate(bodyLine)
                                deadlineTime = parseTime(bodyLine)
                            }
                            bodyLine.startsWith(":Effort:", ignoreCase = true) -> effort = property.matchEntire(bodyLine)?.groupValues?.get(2)?.trim()
                            bodyLine.startsWith(":CATEGORY:", ignoreCase = true) -> category = property.matchEntire(bodyLine)?.groupValues?.get(2)?.trim()
                        }
                        scan += 1
                    }
                    tasks += OrgTask(
                        path = path,
                        line = index + 1,
                        level = level,
                        state = state,
                        title = text,
                        priority = taskPriority,
                        tags = taskTags,
                        scheduled = scheduled,
                        scheduledTime = scheduledTime,
                        deadline = deadline,
                        deadlineTime = deadlineTime,
                        effort = effort,
                        category = category,
                    )
                }
            }
            index += 1
        }
        return OrgDocument(documentTitle, tasks, blocks)
    }

    fun parseHeaders(raw: String): Map<String, String> {
        val result = linkedMapOf<String, String>()
        val matcher = Regex(":([A-Za-z0-9_-]+)(?:\\s+(\"[^\"]*\"|'[^']*'|[^:][^\\s]*))?")
        matcher.findAll(raw).forEach { match ->
            val value = match.groupValues.getOrElse(2) { "" }.trim().trim('"', '\'')
            result[match.groupValues[1].lowercase()] = if (value.isBlank()) "yes" else value
        }
        return result
    }

    private fun parseDate(line: String): LocalDate? =
        timestamp.find(line)?.groupValues?.getOrNull(1)?.let {
            runCatching { LocalDate.parse(it) }.getOrNull()
        }

    private fun parseTime(line: String): LocalTime? =
        timestamp.find(line)?.groupValues?.getOrNull(2)?.takeIf { it.isNotBlank() }?.let {
            runCatching { LocalTime.parse(it) }.getOrNull()
        }
}

data class TangleOutput(val path: String, val language: String, val content: String)
data class TangleResult(val outputs: List<TangleOutput>, val skippedBlocks: Int)

object OrgTangler {
    private val propertyLine = Regex("(?i)^#\\+property:\\s+header-args(?::([A-Za-z0-9_+.-]+))?\\s+(.*)$")

    fun tangle(source: String, sourceName: String = "config.org"): TangleResult {
        val global = linkedMapOf<String, String>()
        val perLanguage = linkedMapOf<String, MutableMap<String, String>>()
        source.lineSequence().forEach { line ->
            val match = propertyLine.matchEntire(line.trim()) ?: return@forEach
            val language = match.groupValues[1].lowercase().ifBlank { null }
            val headers = OrgParser.parseHeaders(match.groupValues[2])
            if (language == null) global.putAll(headers)
            else perLanguage.getOrPut(language) { linkedMapOf() }.putAll(headers)
        }

        val parsed = OrgParser.parse(source, sourceName)
        val buckets = linkedMapOf<Pair<String, String>, MutableList<String>>()
        var skipped = 0
        parsed.sourceBlocks.forEach { block ->
            if (block.language !in DoomOrgProfile.babelLanguages) {
                skipped += 1
                return@forEach
            }
            val headers = linkedMapOf<String, String>().apply {
                putAll(global)
                putAll(perLanguage[block.language].orEmpty())
                putAll(block.headers)
            }
            val targetHeader = headers["tangle"]
            if (targetHeader == null || targetHeader.equals("no", ignoreCase = true)) {
                skipped += 1
                return@forEach
            }
            val target = if (targetHeader.equals("yes", ignoreCase = true)) {
                val stem = sourceName.substringAfterLast('/').substringBeforeLast('.', sourceName)
                "$stem.${extension(block.language)}"
            } else sanitizeTarget(targetHeader)
            buckets.getOrPut(target to block.language) { mutableListOf() } += block.body
        }

        val outputs = buckets.map { (key, bodies) ->
            TangleOutput(
                path = key.first,
                language = key.second,
                content = bodies.joinToString("\n\n", postfix = "\n"),
            )
        }
        return TangleResult(outputs, skipped)
    }

    private fun extension(language: String): String = when (language) {
        "python" -> "py"
        "prolog" -> "pl"
        else -> error("Unsupported tangle language: $language")
    }

    private fun sanitizeTarget(raw: String): String {
        val target = raw.trim().replace('\\', '/').removePrefix("./")
        require(target.isNotBlank()) { "Empty :tangle target" }
        require(!target.startsWith('/')) { "Absolute :tangle targets are not allowed on Android" }
        require(target.split('/').none { it == ".." }) { "Parent traversal is not allowed in :tangle target" }
        return target
    }
}
