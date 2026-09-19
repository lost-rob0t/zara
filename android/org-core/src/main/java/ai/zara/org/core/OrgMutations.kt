package ai.zara.org.core

data class OrgTodoMutation(
    val source: String,
    val state: String,
)

fun OrgParser.cycleTodoState(
    source: String,
    task: OrgTask,
    fallbackTodoStates: List<String> = defaultTodoStates,
): OrgTodoMutation {
    require(task.line > 0) { "Task line must be positive" }

    val (lineStart, lineEnd) = lineBounds(source, task.line)
    val line = source.substring(lineStart, lineEnd)
    val prefix = "*".repeat(task.level) + " "
    require(line.startsWith(prefix)) { "Task heading changed; refresh agenda" }

    val afterStars = line.removePrefix(prefix)
    require(afterStars == task.state || afterStars.startsWith("${task.state} ")) {
        "Task state changed; refresh agenda"
    }

    val next = nextTodoState(source, task.state, fallbackTodoStates)
    val replacement = prefix + next + afterStars.removePrefix(task.state)
    return OrgTodoMutation(
        source = source.substring(0, lineStart) + replacement + source.substring(lineEnd),
        state = next,
    )
}

private fun lineBounds(source: String, lineNumber: Int): Pair<Int, Int> {
    var currentLine = 1
    var index = 0
    var lineStart = 0

    while (currentLine < lineNumber) {
        while (index < source.length && source[index] != '\n' && source[index] != '\r') {
            index += 1
        }
        require(index < source.length) { "Task line is out of range" }

        index += if (
            source[index] == '\r' &&
            index + 1 < source.length &&
            source[index + 1] == '\n'
        ) {
            2
        } else {
            1
        }
        lineStart = index
        currentLine += 1
    }

    while (index < source.length && source[index] != '\n' && source[index] != '\r') {
        index += 1
    }
    return lineStart to index
}
