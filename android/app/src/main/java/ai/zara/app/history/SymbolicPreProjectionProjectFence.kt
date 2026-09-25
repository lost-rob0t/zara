package ai.zara.app.history

import android.content.ContentValues

/**
 * Atomically fence a canonical running assistant when a project switch wins before the natural
 * symbolic runtime has installed its pending projection.
 *
 * The terminal projection is a durable tombstone in the requested project scope. On the first
 * symbolic turn it inserts generation 1. On later turns it advances the already-terminal prior
 * projection to the next generation and binds that tombstone to the new canonical turn id. Either
 * form prevents the losing preflight from installing its stale pending projection while keeping
 * the assistant row and symbolic projection in the same `zara.db` transaction.
 *
 * [currentProjection] is the already-loaded prior terminal projection when this is a later turn.
 * It is deliberately optional so the first-turn path still uses the same canonical fence instead
 * of creating a second project-generation authority.
 */
internal fun PortableConversationStore.fenceRunningTurnBeforeSymbolicProjection(
    conversationId: String,
    requestedProjectId: String?,
    currentProjection: SymbolicConversationProjection? = null,
): SymbolicConversationProjection? = synchronized(this) {
    val loadedProjection = loadSymbolicProjection(conversationId)
    val current = if (currentProjection == null) {
        check(loadedProjection == null) {
            "pre-projection project fence requires the expected symbolic projection state"
        }
        null
    } else {
        val loaded = checkNotNull(loadedProjection) {
            "pre-projection project fence lost the prior symbolic projection"
        }
        check(loaded.projectionGeneration == currentProjection.projectionGeneration) {
            "pre-projection project fence observed a stale projection generation"
        }
        check(loaded.turnId == currentProjection.turnId && loaded.outcome == currentProjection.outcome) {
            "pre-projection project fence observed a changed prior projection"
        }
        currentProjection.assertPureSymbolic()
        check(currentProjection.outcome != "pending") {
            "pre-projection project fence cannot replace an installed pending projection"
        }
        currentProjection
    }

    val assistant = loadMessages(conversationId).lastOrNull { message ->
        message.role == HistoryMessageRole.Assistant &&
            (message.status == HistoryMessageStatus.Pending ||
                message.status == HistoryMessageStatus.Streaming)
    } ?: return@synchronized current
    val turnId = requireNotNull(assistant.turnId) {
        "pre-projection project fence requires canonical turn identity"
    }
    if (current != null) {
        check(current.turnId != turnId) {
            "pre-projection project fence requires a new canonical turn identity"
        }
    }

    val scope = SymbolicProjectScopeContract.next(
        current = current,
        requestedProjectId = requestedProjectId,
    )
    if (
        current != null &&
        scope.projectId == current.projectId &&
        scope.projectGeneration == current.projectGeneration
    ) {
        return@synchronized current
    }

    val now = PortableConversationStore.nowIso()
    val projectionGeneration = Math.addExact(current?.projectionGeneration ?: 0L, 1L)
    val runtimeGeneration = Math.addExact(current?.runtimeGeneration ?: 0L, 1L)
    val tombstone = if (current == null) {
        SymbolicConversationProjection(
            conversationId = conversationId,
            projectionGeneration = projectionGeneration,
            runtimeGeneration = runtimeGeneration,
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
            rendererProvenance = "symbolic-dcg/v1",
            providersEnabled = false,
            maxModelCalls = 0L,
            providerCalls = 0L,
            modelCalls = 0L,
            updatedAt = now,
        )
    } else {
        current.copy(
            projectionGeneration = projectionGeneration,
            runtimeGeneration = runtimeGeneration,
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
            rendererProvenance = "symbolic-dcg/v1",
            providersEnabled = false,
            maxModelCalls = 0L,
            providerCalls = 0L,
            modelCalls = 0L,
            updatedAt = now,
        )
    }.also(SymbolicConversationProjection::assertPureSymbolic)

    val expectedGeneration = current?.projectionGeneration ?: 0L
    SymbolicProjectionContract.validateWrite(
        current = current,
        proposed = tombstone,
        expectedGeneration = expectedGeneration,
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
        put("verified_outcome_refs", tombstone.verifiedOutcomeRefs.joinToString("\n"))
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

        if (current == null) {
            db.insertOrThrow("desktop_symbolic_projections", null, projectionValues)
        } else {
            val projectionChanged = db.update(
                "desktop_symbolic_projections",
                projectionValues,
                "conversation_id = ? AND principal_id = ? AND projection_generation = ?",
                arrayOf(
                    conversationId,
                    ConversationHistoryContract.localPrincipalId,
                    expectedGeneration.toString(),
                ),
            )
            check(projectionChanged == 1) {
                "stale pre-projection project-switch projection fence rejected"
            }
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
    tombstone
}
