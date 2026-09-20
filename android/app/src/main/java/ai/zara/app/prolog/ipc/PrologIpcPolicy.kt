package ai.zara.app.prolog.ipc

data class ProjectedTerms(
    val terms: List<String>,
    val truncated: Boolean,
)

object PrologIpcPolicy {
    const val MAX_GOAL_CHARS = 8_192
    const val MAX_TIMEOUT_MS = 15_000L
    const val MAX_TERM_CHARS = 4_096
    const val MAX_TERMS = 64
    const val MAX_RESULT_CHARS = 60_000

    private val requestIdPattern = Regex("[A-Za-z0-9._:-]{1,96}")

    fun requireRequestId(raw: String): String {
        require(requestIdPattern.matches(raw)) { "Invalid Prolog request id" }
        return raw
    }

    fun requireGoal(raw: String): String {
        val goal = raw.trim()
        require(goal.isNotEmpty()) { "Prolog goal is required" }
        require(goal.length <= MAX_GOAL_CHARS) { "Prolog goal is too large" }
        return goal
    }

    fun requireDeadline(deadlineEpochMs: Long, nowEpochMs: Long = System.currentTimeMillis()): Long {
        require(deadlineEpochMs > nowEpochMs) { "Prolog request deadline already expired" }
        require(deadlineEpochMs - nowEpochMs <= MAX_TIMEOUT_MS) { "Prolog request deadline is too far away" }
        return deadlineEpochMs
    }

    fun projectTerms(source: List<String>): ProjectedTerms {
        val result = ArrayList<String>(minOf(source.size, MAX_TERMS))
        var chars = 0
        var truncated = source.size > MAX_TERMS

        for (term in source.take(MAX_TERMS)) {
            val bounded = if (term.length > MAX_TERM_CHARS) {
                truncated = true
                term.take(MAX_TERM_CHARS)
            } else {
                term
            }
            if (chars + bounded.length > MAX_RESULT_CHARS) {
                truncated = true
                break
            }
            chars += bounded.length
            result += bounded
        }
        return ProjectedTerms(result, truncated)
    }
}
