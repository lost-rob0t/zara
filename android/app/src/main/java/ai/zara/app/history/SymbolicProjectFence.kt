package ai.zara.app.history

/**
 * Atomically cancel a pending symbolic turn onto the requested project scope before that project
 * switch becomes visible to Android chat.
 *
 * This reuses the canonical [PortableConversationStore] history/projection transaction and its
 * generation CAS. It does not own project metadata or create another conversation/project store.
 * A late completion from the previous project therefore loses the existing projection-generation
 * fence, while the canonical assistant row is terminal immediately instead of remaining wedged in
 * Running state until process recreation.
 *
 * Project-scoped dialogue knowledge is cleared on an actual scope change. Verified outcome receipt
 * history is deliberately retained because it is the canonical bounded anti-replay ledger, not
 * conversational project context. The terminal dialogue act is the canonical `cancelled` act so
 * the same persisted truth remains consumable by the shared phone/Wear edge projection contract.
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

    val turnId = requireNotNull(current.turnId) {
        "pending symbolic project switch requires canonical turn identity"
    }
    completeSymbolicTurnAtomically(
        projection = current.copy(
            projectionGeneration = Math.addExact(current.projectionGeneration, 1L),
            outcome = "cancelled",
            projectId = scope.projectId,
            projectGeneration = scope.projectGeneration,
            dialogueAct = "cancelled",
            dialogueStateJson = "{}",
            discourseEntitiesJson = "[]",
            unresolvedQuestionsJson = "[]",
            expertEvidenceJson = "[]",
            verifiedFactsJson = "[]",
        ),
        expectedGeneration = current.projectionGeneration,
        turnId = turnId,
        assistantContent = "",
        assistantStatus = HistoryMessageStatus.Cancelled,
    )
}
