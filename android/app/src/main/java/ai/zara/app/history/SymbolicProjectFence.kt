package ai.zara.app.history

/**
 * Advance a pending symbolic projection onto the requested project scope before a project switch
 * becomes visible to Android chat.
 *
 * This reuses the canonical [PortableConversationStore] projection row and its generation CAS. It
 * does not own project metadata or create another conversation/project store. A late completion
 * from the previous project therefore loses the existing projection-generation fence.
 */
fun PortableConversationStore.fencePendingSymbolicProject(
    conversationId: String,
    requestedProjectId: String?,
): SymbolicConversationProjection? = synchronized(this) {
    val current = loadSymbolicProjection(conversationId) ?: return@synchronized null
    if (current.outcome != "pending") return@synchronized current

    val scope = SymbolicProjectScopeContract.next(
        current = current,
        requestedProjectId = requestedProjectId,
    )
    if (
        scope.projectId == current.projectId &&
        scope.projectGeneration == current.projectGeneration
    ) {
        return@synchronized current
    }

    saveSymbolicProjection(
        projection = current.copy(
            projectionGeneration = Math.addExact(current.projectionGeneration, 1L),
            projectId = scope.projectId,
            projectGeneration = scope.projectGeneration,
        ),
        expectedGeneration = current.projectionGeneration,
    )
}
