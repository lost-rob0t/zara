package ai.zara.app.history

import android.content.ContentValues

/**
 * Atomically terminalize one running assistant message and its symbolic projection.
 *
 * Pure-symbolic Android turns already use [PortableConversationStore] as the canonical history and
 * projection owner. This operation closes the process-death gap between publishing Context1 and
 * terminalizing the visible assistant turn: either both terminal writes commit in `zara.db`, or
 * neither does. A stale restart/cancellation generation loses the projection CAS and therefore the
 * whole transaction.
 *
 * The caller must have installed a `pending` projection for [turnId] before local evaluation. The
 * supplied [projection] is the next terminal generation for that same turn. No provider, expert,
 * permission, or alternate conversation authority is introduced here.
 */
fun PortableConversationStore.completeSymbolicTurnAtomically(
    projection: SymbolicConversationProjection,
    expectedGeneration: Long,
    turnId: String,
    assistantContent: String,
    assistantStatus: HistoryMessageStatus,
    assistantError: String = "",
): SymbolicConversationProjection = synchronized(this) {
    require(turnId.isNotBlank()) { "turnId must not be blank" }
    require(projection.turnId == turnId) {
        "symbolic terminal projection must preserve the canonical assistant turn identity"
    }
    val expectedOutcome = when (assistantStatus) {
        HistoryMessageStatus.Complete -> "success"
        HistoryMessageStatus.Error -> "error"
        HistoryMessageStatus.Cancelled -> "cancelled"
        HistoryMessageStatus.Pending,
        HistoryMessageStatus.Streaming,
        -> error("symbolic terminal commit requires a terminal assistant status")
    }
    require(projection.outcome == expectedOutcome) {
        "assistant status ${assistantStatus.wireName} requires symbolic outcome $expectedOutcome"
    }

    val current = requireNotNull(loadSymbolicProjection(projection.conversationId)) {
        "symbolic terminal commit requires an existing pending projection"
    }
    check(current.turnId == turnId) {
        "symbolic terminal commit turn does not own the current projection"
    }
    check(current.outcome == "pending") {
        "symbolic terminal commit requires a pending projection"
    }
    SymbolicProjectionContract.validateWrite(
        current = current,
        proposed = projection,
        expectedGeneration = expectedGeneration,
    )

    val now = PortableConversationStore.nowIso()
    val stored = projection.copy(updatedAt = now)
    val db = writableDatabase
    db.beginTransaction()
    try {
        val messageChanged = db.update(
            "desktop_messages",
            ContentValues().apply {
                put("content", assistantContent)
                put("status", assistantStatus.wireName)
                put("error", assistantError)
                put("updated_at", now)
            },
            "conversation_id = ? AND principal_id = ? AND turn_id = ? AND role = ? " +
                "AND status IN (?, ?)",
            arrayOf(
                stored.conversationId,
                ConversationHistoryContract.localPrincipalId,
                turnId,
                HistoryMessageRole.Assistant.wireName,
                HistoryMessageStatus.Pending.wireName,
                HistoryMessageStatus.Streaming.wireName,
            ),
        )
        check(messageChanged == 1) {
            "stale symbolic assistant terminal update rejected"
        }

        val projectionChanged = db.update(
            "desktop_symbolic_projections",
            ContentValues().apply {
                put("conversation_id", stored.conversationId)
                put("principal_id", ConversationHistoryContract.localPrincipalId)
                put("turn_id", turnId)
                put("outcome", stored.outcome)
                put("projection_generation", stored.projectionGeneration)
                put("runtime_generation", stored.runtimeGeneration)
                if (stored.projectId == null) putNull("project_id") else put("project_id", stored.projectId)
                put("project_generation", stored.projectGeneration)
                put("dialogue_act", stored.dialogueAct)
                put("dialogue_state_json", stored.dialogueStateJson)
                put("discourse_entities_json", stored.discourseEntitiesJson)
                put("unresolved_questions_json", stored.unresolvedQuestionsJson)
                put("expert_evidence_json", stored.expertEvidenceJson)
                put("verified_facts_json", stored.verifiedFactsJson)
                put("verified_outcome_refs", stored.verifiedOutcomeRefs.joinToString("\n"))
                put("renderer_provenance", stored.rendererProvenance)
                put("providers_enabled", if (stored.providersEnabled) 1 else 0)
                put("max_model_calls", stored.maxModelCalls)
                put("provider_calls", stored.providerCalls)
                put("model_calls", stored.modelCalls)
                put("updated_at", now)
            },
            "conversation_id = ? AND principal_id = ? AND turn_id = ? " +
                "AND outcome = ? AND projection_generation = ?",
            arrayOf(
                stored.conversationId,
                ConversationHistoryContract.localPrincipalId,
                turnId,
                "pending",
                expectedGeneration.toString(),
            ),
        )
        check(projectionChanged == 1) {
            "stale symbolic projection terminal update rejected"
        }

        db.update(
            "desktop_conversations",
            ContentValues().apply { put("updated_at", now) },
            "id = ? AND principal_id = ?",
            arrayOf(stored.conversationId, ConversationHistoryContract.localPrincipalId),
        )
        db.setTransactionSuccessful()
    } finally {
        db.endTransaction()
    }
    stored
}

/**
 * Fail a canonical pure-symbolic assistant turn before a pending projection was installed.
 *
 * MainActivity creates the user and pending-assistant rows before the runtime factory enters its
 * context/project preflight. If that preflight fails, the same [PortableConversationStore] must
 * terminalize the already-owned assistant row so the conversation cannot remain wedged in running
 * state and the controller cannot invent a replacement turn identity. This path is deliberately
 * unavailable once a pending symbolic projection exists; after that boundary callers must use
 * [completeSymbolicTurnAtomically] and its projection-generation CAS.
 */
fun PortableConversationStore.failSymbolicTurnBeforeProjection(
    conversationId: String,
    turnId: String,
    assistantContent: String,
    assistantError: String = assistantContent,
) = synchronized(this) {
    require(conversationId.isNotBlank()) { "conversationId must not be blank" }
    require(turnId.isNotBlank()) { "turnId must not be blank" }

    val current = loadSymbolicProjection(conversationId)
    check(current == null || current.turnId != turnId || current.outcome != "pending") {
        "symbolic preflight failure cannot bypass pending projection CAS"
    }

    val now = PortableConversationStore.nowIso()
    val db = writableDatabase
    db.beginTransaction()
    try {
        val messageChanged = db.update(
            "desktop_messages",
            ContentValues().apply {
                put("content", assistantContent)
                put("status", HistoryMessageStatus.Error.wireName)
                put("error", assistantError)
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
            "stale symbolic preflight assistant update rejected"
        }

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
}
