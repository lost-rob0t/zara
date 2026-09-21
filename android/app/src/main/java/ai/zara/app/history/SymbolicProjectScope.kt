package ai.zara.app.history

private const val MAX_SYMBOLIC_PROJECT_ID_CHARS = 512

/**
 * Deterministic project identity carried by the canonical symbolic projection.
 *
 * This is not a project store. It only derives the next project id/generation pair from the
 * existing [SymbolicConversationProjection] so Android can reuse the same stale-project fence as
 * Desktop and plugin surfaces. A changed project identity always advances the generation; an
 * unchanged identity preserves it.
 */
internal data class SymbolicProjectScope(
    val projectId: String?,
    val projectGeneration: Long,
)

internal object SymbolicProjectScopeContract {
    fun next(
        current: SymbolicConversationProjection?,
        requestedProjectId: String?,
    ): SymbolicProjectScope {
        val projectId = normalizeProjectId(requestedProjectId)
        val currentGeneration = current?.projectGeneration ?: 0L
        val projectGeneration = when {
            current == null && projectId == null -> 0L
            current == null -> 1L
            current.projectId == projectId -> currentGeneration
            else -> Math.addExact(currentGeneration, 1L)
        }
        return SymbolicProjectScope(
            projectId = projectId,
            projectGeneration = projectGeneration,
        )
    }

    private fun normalizeProjectId(rawProjectId: String?): String? {
        val projectId = rawProjectId?.trim()?.takeIf(String::isNotEmpty) ?: return null
        require(projectId.length <= MAX_SYMBOLIC_PROJECT_ID_CHARS) {
            "Project id exceeds $MAX_SYMBOLIC_PROJECT_ID_CHARS characters"
        }
        require(projectId.none(Char::isISOControl)) {
            "Project id contains control characters"
        }
        return projectId
    }
}
