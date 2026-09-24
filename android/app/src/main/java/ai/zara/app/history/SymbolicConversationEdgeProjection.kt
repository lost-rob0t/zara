package ai.zara.app.history

import ai.zara.ui.continuity.SymbolicConversationEdgeSnapshot
import org.json.JSONArray
import org.json.JSONObject

/**
 * Project the canonical Android conversation row into the read-only shared edge contract.
 *
 * This does not persist anything and owns no conversation state. It always reads through
 * [PortableConversationStore], so phone/Wear consumers observe the same scoped truth that
 * survives Android process recreation.
 */
fun PortableConversationStore.loadSymbolicEdgeSnapshot(
    conversationId: String,
): SymbolicConversationEdgeSnapshot? = synchronized(this) {
    val projection = loadSymbolicProjection(conversationId) ?: return@synchronized null
    projection.toSymbolicEdgeSnapshot(ConversationHistoryContract.localPrincipalId)
}

internal fun SymbolicConversationProjection.toSymbolicEdgeSnapshot(
    principalId: String,
): SymbolicConversationEdgeSnapshot {
    require(principalId.isNotBlank()) { "principalId must not be blank" }
    assertPureSymbolic()

    val snapshot = SymbolicConversationEdgeSnapshot(
        principalId = principalId,
        conversationId = conversationId,
        projectionGeneration = projectionGeneration,
        runtimeGeneration = runtimeGeneration,
        projectId = projectId,
        projectGeneration = projectGeneration,
        dialogueAct = dialogueAct,
        discourseEntityRefs = extractStableRefs(
            discourseEntitiesJson,
            label = "discourse entity reference",
            keys = arrayOf("ref", "entity_ref", "entity_id"),
        ),
        unresolvedQuestionRefs = extractStableRefs(
            unresolvedQuestionsJson,
            label = "unresolved question reference",
            keys = arrayOf("ref", "question_ref", "question_id", "slot"),
        ),
        expertEvidenceRefs = extractStableRefs(
            expertEvidenceJson,
            label = "expert evidence reference",
            keys = arrayOf("ref", "evidence_ref", "evidence_id"),
        ),
        verifiedOutcomeRefs = verifiedOutcomeRefs,
        rendererProvenance = rendererProvenance,
        providersEnabled = providersEnabled,
        maxModelCalls = maxModelCalls,
        modelCalls = modelCalls,
        providerCalls = providerCalls,
    )
    snapshot.assertPureSymbolic()
    return snapshot
}

private fun extractStableRefs(
    encoded: String,
    label: String,
    keys: Array<String>,
): List<String> {
    val array = JSONArray(encoded)
    return List(array.length()) { index ->
        val item = array.getJSONObject(index)
        requireNotNull(item.firstNonBlankString(keys)) {
            "$label[$index] is missing a stable reference key: ${keys.joinToString()}"
        }
    }
}

private fun JSONObject.firstNonBlankString(keys: Array<String>): String? {
    for (key in keys) {
        if (!has(key) || isNull(key)) continue
        val value = optString(key, "").trim()
        if (value.isNotEmpty()) return value
    }
    return null
}
