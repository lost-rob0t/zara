package ai.zara.org.core

/**
 * Canonical TODO workflow semantics derived from ordinary Org text.
 *
 * Ordinary file-local `#+TODO` / `#+SEQ_TODO` declarations remain the source
 * of truth. Multiple declarations represent parallel Org sequences; this
 * helper preserves those sequence boundaries for parsing, cycling, and
 * derived open/done projections without introducing another workflow store.
 */
data class OrgTodoWorkflow(
    val states: List<String>,
    val doneStates: Set<String>,
)

internal data class OrgTodoSequence(
    val states: List<String>,
    val doneStates: Set<String>,
)

private val workflowDirective = Regex("(?i)^#\\+(?:TODO|SEQ_TODO):\\s*(.*)$")
private val workflowKeyword = Regex("^([^\\s(|]+)")

private fun parseWorkflowKeywords(raw: String): List<String> =
    raw.split(Regex("\\s+"))
        .asSequence()
        .filter(String::isNotBlank)
        .mapNotNull { token -> workflowKeyword.find(token)?.groupValues?.getOrNull(1) }
        .filter(String::isNotBlank)
        .distinct()
        .toList()

internal fun parseOrgTodoSequences(
    source: String,
    fallbackTodoStates: List<String>,
): List<OrgTodoSequence> {
    val declared = source.lineSequence()
        .mapNotNull { line -> workflowDirective.matchEntire(line.trim())?.groupValues?.getOrNull(1) }
        .mapNotNull { raw ->
            val parts = raw.split('|', limit = 2)
            val openStates = parseWorkflowKeywords(parts[0])
            val explicitDoneStates = if (parts.size == 2) {
                parseWorkflowKeywords(parts[1])
            } else {
                emptyList()
            }
            val states = (openStates + explicitDoneStates).distinct()
            if (states.isEmpty()) {
                null
            } else {
                OrgTodoSequence(
                    states = states,
                    doneStates = if (parts.size == 2) {
                        explicitDoneStates.toSet()
                    } else {
                        setOf(states.last())
                    },
                )
            }
        }
        .toList()

    if (declared.isNotEmpty()) return declared

    val fallback = fallbackTodoStates
        .filter(String::isNotBlank)
        .distinct()
        .ifEmpty { OrgParser.defaultTodoStates }

    return listOf(
        OrgTodoSequence(
            states = fallback,
            doneStates = setOf(fallback.last()),
        ),
    )
}

fun OrgParser.todoWorkflow(
    source: String,
    fallbackTodoStates: List<String> = defaultTodoStates,
): OrgTodoWorkflow {
    val sequences = parseOrgTodoSequences(source, fallbackTodoStates)
    val states = sequences.flatMap { it.states }.distinct()
    require(states.isNotEmpty()) { "Org TODO workflow cannot be empty" }

    return OrgTodoWorkflow(
        states = states,
        doneStates = sequences.flatMap { it.doneStates }.toSet(),
    )
}
