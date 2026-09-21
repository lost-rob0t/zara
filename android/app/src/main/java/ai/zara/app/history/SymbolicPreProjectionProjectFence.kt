package ai.zara.app.history

import android.content.ContentValues

/**
 * Atomically fence a canonical running assistant when a project switch wins before the natural
 * symbolic runtime has installed its pending projection.
 *
 * The terminal projection is a durable generation-1 tombstone in the requested project scope. It
 * prevents the losing preflight from later inserting a generation-1 project-A pending projection,
 * while keeping the assistant row and symbolic projection in the same `zara.db` transaction.
 */
internal fun PortableConversationStore.fenceRunningTurnBeforeSymbolicProjection(
    conversationId: String,
    requestedProjectId: String?,
): SymbolicConversationProjection? = synchronized(this) {
    check(loadSymbolicProjection(conversationId) == null) {
        "pre-projection project fence requires no existing symbolic projection"
    }
    val assistant = loadMessages(conversationId).lastOrNull { message ->
        message.role == HistoryMessageRole.Assistant &&
            (message.status == HistoryMessageStatus.Pending ||
                message.status == HistoryMessageStatus.Streaming)
    } ?: return@synchronized null
    val turnId = requireNotNull(assistant.turnId) {
        "pre-projection project fence requires canonical turn identity"
    }
    val scope = SymbolicProjectScopeContract.next(
        current = null,
        requestedProjectId = requestedProjectId,
    )
    val now = PortableConversationStore.nowIso()
    val tombstone = SymbolicConversationProjection(
        conversationId = conversationId,
        projectionGeneration = 1L,
        runtimeGeneration = 1L,
        turnId = turnId,
        outcome = "cancelled",
        projectId = scope.projectId,
        projectGeneration = scope.projectGeneration,
        dialogueAct = "cancelled",
        dialogueStateJson = "{}",
        discourseEntitiesJson = "[]",
        unresolvedQuestionsJson = "[]",
        expertEvidenceJson = "[]",
        verifiedFactsJson = "[]",
        rendererProvenance = "",
        providersEnabled = false,
        maxModelCalls = 0L,
        providerCalls = 0L,
        modelCalls = 0L,
        updatedAt = now,
    ).also(SymbolicConversationProjection::assertPureSymbolic)
    SymbolicProjectionContract.validateWrite(
        current = null,
        proposed = tombstone,
        expectedGeneration = 0L,
    )

    val projectionValues = ContentValues().apply {
        put("conversation_id", tombstone.conversationId)
        put("principal_id", ConversationHistoryContract.localPrincipalId)
        put("turn_id", turnId)
        put("outcome", tombstone.outcome)
        put("projection_generation", tombstone.projectionGeneration)
        put("runtime_generation", tombstone.runtimeGeneration)
        if (tombstone.projectId == null) putNull("project_id") else put("project_id", tombstone.projectId)
        put("project_generation", tombstone.projectGeneration)
        put("dialogue_act", tombstone.dialogueAct)
        put("dialogue_state_json", tombstone.dialogueStateJson)
        put("discourse_entities_json", tombstone.discourseEntitiesJson)
        put("unresolved_questions_json", tombstone.unresolvedQuestionsJson)
        put("expert_evidence_json", tombstone.expertEvidenceJson)
        put("verified_facts_json", tombstone.verifiedFactsJson)
        put("verified_outcome_refs", "")
        put("renderer_provenance", tombstone.rendererProvenance)
        put("providers_enabled", 0)
        put("max_model_calls", 0L)
        put("provider_calls", 0L)
        put("model_calls", 0L)
        put("updated_at", now)
    }

    val db = writableDatabase
    db.beginTransaction()
    try {
        val messageChanged = db.update(
            "desktop_messages",
            ContentValues().apply {
                put("content", "")
                put("status", HistoryMessageStatus.Cancelled.wireName)
                put("error", "")
                put("updated_at", now)
            },
            "conversation_id = ? AND principal_id = ? AND turn_id = ? AND role = ? " +
                "AND status IN (?, ?)",
            arrayOf(
                conversationId,
                ConversationHistoryContract.localPrincipalId,
                turnId,
                HistoryMessageRole.Assistant.wireName,
                HistoryMessageStatus.Pending.wireName,
                HistoryMessageStatus.Streaming.wireName,
            ),
        )
        check(messageChanged == 1) {
            "stale pre-projection project-switch assistant fence rejected"
        }
        db.insertOrThrow("desktop_symbolic_projections", null, projectionValues)
        db.update(
            "desktop_conversations",
            ContentValues().apply { put("updated_at", now) },
            "id = ? AND principal_id = ?",
            arrayOf(conversationId, ConversationHistoryContract.localPrincipalId),
        )
        db.setTransactionSuccessful()
    } finally {
        db.endTransaction()
    }
    tombstone
}
