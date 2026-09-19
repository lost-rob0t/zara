package ai.zara.org.core

/**
 * Canonical TODO workflow semantics derived from ordinary Org text.
 *
 * Keyword parsing remains owned by [OrgParser]. This helper only preserves the
 * Org `|` boundary so derived projections can distinguish open and done states
 * without falling back to an operator-specific profile.
 */
data class OrgTodoWorkflow(
    val states: List<String>,
    val doneStates: Set<String>,
)

private val workflowDirective = Regex("(?i)^#\\+(?:TODO|SEQ_TODO):\\s*(.*)$")

fun OrgParser.todoWorkflow(
    source: String,
    fallbackTodoStates: List<String> = defaultTodoStates,
): OrgTodoWorkflow {
    val states = todoStates(source, fallbackTodoStates)
    require(states.isNotEmpty()) { "Org TODO workflow cannot be empty" }

    val raw = source.lineSequence()
        .mapNotNull { line -> workflowDirective.matchEntire(line.trim())?.groupValues?.getOrNull(1) }
        .firstOrNull()

    val doneStates = raw?.let { directive ->
        val tokens = directive.split(Regex("\\s+")).filter(String::isNotBlank)
        val separator = tokens.indexOf("|")
        if (separator >= 0) {
            val openCount = tokens.take(separator).size.coerceAtMost(states.size)
            states.drop(openCount).toSet()
        } else {
            setOf(states.last())
        }
    } ?: setOf(states.last())

    return OrgTodoWorkflow(states = states, doneStates = doneStates)
}
