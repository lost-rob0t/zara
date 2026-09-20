package ai.zara.app.history

import android.content.ContentValues

/**
 * Portable symbolic context projected over the canonical conversation history.
 *
 * This is not a second Android chat store. Rows live in the same zara.db file,
 * use the same conversation/principal identity, and are cascade-owned by the
 * canonical conversation row.
 */
data class SymbolicConversationProjection(
    val conversationId: String,
    val projectionGeneration: Long,
    val runtimeGeneration: Long,
    val projectId: String? = null,
    val projectGeneration: Long = 0,
    val dialogueStateJson: String = "{}",
    val discourseEntitiesJson: String = "[]",
    val unresolvedQuestionsJson: String = "[]",
    val expertEvidenceJson: String = "[]",
    val verifiedFactsJson: String = "[]",
    val rendererProvenance: String = "",
    val providerCalls: Long = 0,
    val modelCalls: Long = 0,
    val updatedAt: String = "",
) {
    fun assertPureSymbolic() {
        check(providerCalls == 0L && modelCalls == 0L) {
            "pure-symbolic conversation recorded providerCalls=$providerCalls, modelCalls=$modelCalls"
        }
    }
}

internal object SymbolicProjectionContract {
    fun validatePayload(projection: SymbolicConversationProjection) {
        require(projection.conversationId.isNotEmpty()) { "conversationId must not be empty" }
        require(projection.projectionGeneration >= 1L) { "projectionGeneration must be >= 1" }
        require(projection.runtimeGeneration >= 0L) { "runtimeGeneration must be >= 0" }
        require(projection.projectGeneration >= 0L) { "projectGeneration must be >= 0" }
        require(projection.providerCalls >= 0L) { "providerCalls must be >= 0" }
        require(projection.modelCalls >= 0L) { "modelCalls must be >= 0" }
        require((projection.projectId?.length ?: 0) <= 512) { "projectId exceeds 512 characters" }
        require(projection.rendererProvenance.length <= 512) {
            "rendererProvenance exceeds 512 characters"
        }
        requireJsonShape(projection.dialogueStateJson, '{', '}', "dialogueStateJson")
        requireJsonShape(projection.discourseEntitiesJson, '[', ']', "discourseEntitiesJson")
        requireJsonShape(projection.unresolvedQuestionsJson, '[', ']', "unresolvedQuestionsJson")
        requireJsonShape(projection.expertEvidenceJson, '[', ']', "expertEvidenceJson")
        requireJsonShape(projection.verifiedFactsJson, '[', ']', "verifiedFactsJson")
    }

    fun validateWrite(
        current: SymbolicConversationProjection?,
        proposed: SymbolicConversationProjection,
        expectedGeneration: Long,
    ) {
        validatePayload(proposed)
        require(expectedGeneration >= 0L) { "expectedGeneration must be >= 0" }
        require(proposed.projectionGeneration == expectedGeneration + 1L) {
            "projectionGeneration must equal expectedGeneration + 1"
        }
        if (current == null) {
            check(expectedGeneration == 0L) {
                "stale symbolic projection write: projection does not exist"
            }
            return
        }
        check(current.projectionGeneration == expectedGeneration) {
            "stale symbolic projection write: expected generation $expectedGeneration, " +
                "current ${current.projectionGeneration}"
        }
        check(proposed.runtimeGeneration >= current.runtimeGeneration) {
            "runtimeGeneration regression rejected"
        }
        check(proposed.providerCalls >= current.providerCalls) {
            "provider-call ledger rewind rejected"
        }
        check(proposed.modelCalls >= current.modelCalls) {
            "model-call ledger rewind rejected"
        }
        if (proposed.projectId == current.projectId) {
            check(proposed.projectGeneration >= current.projectGeneration) {
                "projectGeneration regression rejected"
            }
        } else {
            check(proposed.projectGeneration > current.projectGeneration) {
                "project switch must advance projectGeneration"
            }
        }
    }

    private fun requireJsonShape(value: String, open: Char, close: Char, name: String) {
        val trimmed = value.trim()
        require(trimmed.length >= 2 && trimmed.first() == open && trimmed.last() == close) {
            "$name has invalid JSON container shape"
        }
    }
}

fun PortableConversationStore.loadSymbolicProjection(
    conversationId: String,
): SymbolicConversationProjection? = synchronized(this) {
    if (getConversation(conversationId) == null) return@synchronized null
    readableDatabase.query(
        "desktop_symbolic_projections",
        null,
        "conversation_id = ? AND principal_id = ?",
        arrayOf(conversationId, ConversationHistoryContract.localPrincipalId),
        null,
        null,
        null,
        "1",
    ).use { cursor ->
        if (!cursor.moveToFirst()) return@use null
        SymbolicConversationProjection(
            conversationId = cursor.getString(cursor.getColumnIndexOrThrow("conversation_id")),
            projectionGeneration = cursor.getLong(cursor.getColumnIndexOrThrow("projection_generation")),
            runtimeGeneration = cursor.getLong(cursor.getColumnIndexOrThrow("runtime_generation")),
            projectId = cursor.nullableString("project_id"),
            projectGeneration = cursor.getLong(cursor.getColumnIndexOrThrow("project_generation")),
            dialogueStateJson = cursor.getString(cursor.getColumnIndexOrThrow("dialogue_state_json")),
            discourseEntitiesJson = cursor.getString(cursor.getColumnIndexOrThrow("discourse_entities_json")),
            unresolvedQuestionsJson = cursor.getString(cursor.getColumnIndexOrThrow("unresolved_questions_json")),
            expertEvidenceJson = cursor.getString(cursor.getColumnIndexOrThrow("expert_evidence_json")),
            verifiedFactsJson = cursor.getString(cursor.getColumnIndexOrThrow("verified_facts_json")),
            rendererProvenance = cursor.getString(cursor.getColumnIndexOrThrow("renderer_provenance")),
            providerCalls = cursor.getLong(cursor.getColumnIndexOrThrow("provider_calls")),
            modelCalls = cursor.getLong(cursor.getColumnIndexOrThrow("model_calls")),
            updatedAt = cursor.getString(cursor.getColumnIndexOrThrow("updated_at")),
        )
    }
}

fun PortableConversationStore.saveSymbolicProjection(
    projection: SymbolicConversationProjection,
    expectedGeneration: Long,
): SymbolicConversationProjection = synchronized(this) {
    requireNotNull(getConversation(projection.conversationId)) {
        "Unknown conversation ${projection.conversationId}"
    }
    val current = loadSymbolicProjection(projection.conversationId)
    SymbolicProjectionContract.validateWrite(current, projection, expectedGeneration)

    val stored = projection.copy(updatedAt = PortableConversationStore.nowIso())
    val values = ContentValues().apply {
        put("conversation_id", stored.conversationId)
        put("principal_id", ConversationHistoryContract.localPrincipalId)
        put("projection_generation", stored.projectionGeneration)
        put("runtime_generation", stored.runtimeGeneration)
        if (stored.projectId == null) putNull("project_id") else put("project_id", stored.projectId)
        put("project_generation", stored.projectGeneration)
        put("dialogue_state_json", stored.dialogueStateJson)
        put("discourse_entities_json", stored.discourseEntitiesJson)
        put("unresolved_questions_json", stored.unresolvedQuestionsJson)
        put("expert_evidence_json", stored.expertEvidenceJson)
        put("verified_facts_json", stored.verifiedFactsJson)
        put("renderer_provenance", stored.rendererProvenance)
        put("provider_calls", stored.providerCalls)
        put("model_calls", stored.modelCalls)
        put("updated_at", stored.updatedAt)
    }

    val db = writableDatabase
    db.beginTransaction()
    try {
        if (current == null) {
            db.insertOrThrow("desktop_symbolic_projections", null, values)
        } else {
            val changed = db.update(
                "desktop_symbolic_projections",
                values,
                "conversation_id = ? AND principal_id = ? AND projection_generation = ?",
                arrayOf(
                    stored.conversationId,
                    ConversationHistoryContract.localPrincipalId,
                    expectedGeneration.toString(),
                ),
            )
            check(changed == 1) { "stale symbolic projection write rejected" }
        }
        db.setTransactionSuccessful()
    } finally {
        db.endTransaction()
    }
    stored
}

private fun android.database.Cursor.nullableString(column: String): String? {
    val index = getColumnIndexOrThrow(column)
    return if (isNull(index)) null else getString(index)
}
