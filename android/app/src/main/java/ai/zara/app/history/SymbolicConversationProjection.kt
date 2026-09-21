package ai.zara.app.history

import android.content.ContentValues
import android.database.Cursor

private const val SYMBOLIC_RENDERER_ID = "symbolic-dcg/v1"
private const val VERIFIED_OUTCOME_WINDOW = 64

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
    val turnId: String? = null,
    val outcome: String = "unknown",
    val projectId: String? = null,
    val projectGeneration: Long = 0,
    val dialogueAct: String = "unknown",
    val dialogueStateJson: String = "{}",
    val discourseEntitiesJson: String = "[]",
    val unresolvedQuestionsJson: String = "[]",
    val expertEvidenceJson: String = "[]",
    val verifiedFactsJson: String = "[]",
    val verifiedOutcomeRefs: List<String> = emptyList(),
    val rendererProvenance: String = "",
    val providersEnabled: Boolean = true,
    val maxModelCalls: Long = 1,
    val providerCalls: Long = 0,
    val modelCalls: Long = 0,
    val updatedAt: String = "",
) {
    fun assertPureSymbolic() {
        SymbolicProjectionContract.validatePayload(this)
        check(!providersEnabled) {
            "pure-symbolic conversation has providers enabled"
        }
        check(maxModelCalls == 0L) {
            "pure-symbolic conversation recorded maxModelCalls=$maxModelCalls"
        }
        check(providerCalls == 0L && modelCalls == 0L) {
            "pure-symbolic conversation recorded providerCalls=$providerCalls, modelCalls=$modelCalls"
        }
        check(rendererProvenance.isEmpty() || rendererProvenance == SYMBOLIC_RENDERER_ID) {
            "pure-symbolic conversation recorded non-symbolic renderer $rendererProvenance"
        }
        check(outcome != "success" || rendererProvenance == SYMBOLIC_RENDERER_ID) {
            "successful projection requires canonical symbolic renderer"
        }
        check(dialogueAct != "verified" || verifiedOutcomeRefs.isNotEmpty()) {
            "verified projection requires verified outcome evidence"
        }
    }
}

private enum class JsonContainerKind {
    OBJECT,
    ARRAY,
    SCALAR,
}

/** Pure Kotlin JSON grammar validator used by local JVM tests and Android. */
private object PortableJsonValidator {
    fun requireObject(value: String, name: String) {
        require(Parser(value).parseDocument() == JsonContainerKind.OBJECT) {
            "$name must be a JSON object"
        }
    }

    fun requireObjectArray(value: String, name: String) {
        try {
            Parser(value).parseObjectArrayDocument()
        } catch (error: IllegalArgumentException) {
            throw IllegalArgumentException("$name must be a JSON array of objects: ${error.message}", error)
        }
    }

    private class Parser(private val text: String) {
        private var index = 0

        fun parseDocument(): JsonContainerKind {
            skipWhitespace()
            val kind = parseValue()
            skipWhitespace()
            require(index == text.length) { "trailing JSON content" }
            return kind
        }

        fun parseObjectArrayDocument() {
            skipWhitespace()
            expect('[')
            skipWhitespace()
            if (consume(']')) {
                skipWhitespace()
                require(index == text.length) { "trailing JSON content" }
                return
            }
            while (true) {
                require(parseValue() == JsonContainerKind.OBJECT) {
                    "top-level array item must be an object"
                }
                skipWhitespace()
                if (consume(']')) break
                expect(',')
                skipWhitespace()
            }
            skipWhitespace()
            require(index == text.length) { "trailing JSON content" }
        }

        private fun parseValue(): JsonContainerKind {
            require(index < text.length) { "unexpected end of JSON" }
            return when (text[index]) {
                '{' -> parseObject()
                '[' -> parseArray()
                '"' -> {
                    parseString()
                    JsonContainerKind.SCALAR
                }
                't' -> {
                    parseLiteral("true")
                    JsonContainerKind.SCALAR
                }
                'f' -> {
                    parseLiteral("false")
                    JsonContainerKind.SCALAR
                }
                'n' -> {
                    parseLiteral("null")
                    JsonContainerKind.SCALAR
                }
                '-', in '0'..'9' -> {
                    parseNumber()
                    JsonContainerKind.SCALAR
                }
                else -> throw IllegalArgumentException("invalid JSON value")
            }
        }

        private fun parseObject(): JsonContainerKind {
            expect('{')
            skipWhitespace()
            if (consume('}')) return JsonContainerKind.OBJECT
            while (true) {
                require(index < text.length && text[index] == '"') {
                    "JSON object key must be a string"
                }
                parseString()
                skipWhitespace()
                expect(':')
                skipWhitespace()
                parseValue()
                skipWhitespace()
                if (consume('}')) return JsonContainerKind.OBJECT
                expect(',')
                skipWhitespace()
            }
        }

        private fun parseArray(): JsonContainerKind {
            expect('[')
            skipWhitespace()
            if (consume(']')) return JsonContainerKind.ARRAY
            while (true) {
                parseValue()
                skipWhitespace()
                if (consume(']')) return JsonContainerKind.ARRAY
                expect(',')
                skipWhitespace()
            }
        }

        private fun parseString() {
            expect('"')
            while (index < text.length) {
                val char = text[index++]
                when {
                    char == '"' -> return
                    char == '\\' -> parseEscape()
                    char.code < 0x20 -> throw IllegalArgumentException("control character in JSON string")
                }
            }
            throw IllegalArgumentException("unterminated JSON string")
        }

        private fun parseEscape() {
            require(index < text.length) { "unterminated JSON escape" }
            when (text[index++]) {
                '"', '\\', '/', 'b', 'f', 'n', 'r', 't' -> Unit
                'u' -> repeat(4) {
                    require(index < text.length && text[index].isHexDigit()) {
                        "invalid JSON unicode escape"
                    }
                    index += 1
                }
                else -> throw IllegalArgumentException("invalid JSON escape")
            }
        }

        private fun parseLiteral(literal: String) {
            require(text.regionMatches(index, literal, 0, literal.length)) {
                "invalid JSON literal"
            }
            index += literal.length
        }

        private fun parseNumber() {
            consume('-')
            require(index < text.length) { "incomplete JSON number" }
            if (consume('0')) {
                require(index >= text.length || text[index] !in '0'..'9') {
                    "leading zero in JSON number"
                }
            } else {
                require(text[index] in '1'..'9') { "invalid JSON number" }
                while (index < text.length && text[index] in '0'..'9') index += 1
            }
            if (consume('.')) {
                require(index < text.length && text[index] in '0'..'9') {
                    "JSON fraction requires digits"
                }
                while (index < text.length && text[index] in '0'..'9') index += 1
            }
            if (index < text.length && (text[index] == 'e' || text[index] == 'E')) {
                index += 1
                if (index < text.length && (text[index] == '+' || text[index] == '-')) index += 1
                require(index < text.length && text[index] in '0'..'9') {
                    "JSON exponent requires digits"
                }
                while (index < text.length && text[index] in '0'..'9') index += 1
            }
        }

        private fun skipWhitespace() {
            while (index < text.length && text[index] in charArrayOf(' ', '\t', '\n', '\r')) {
                index += 1
            }
        }

        private fun expect(expected: Char) {
            require(consume(expected)) { "expected '$expected' in JSON" }
        }

        private fun consume(expected: Char): Boolean {
            if (index >= text.length || text[index] != expected) return false
            index += 1
            return true
        }

        private fun Char.isHexDigit(): Boolean =
            this in '0'..'9' || this in 'a'..'f' || this in 'A'..'F'
    }
}

internal object SymbolicProjectionContract {
    private val outcomes = setOf(
        "unknown",
        "pending",
        "success",
        "cancelled",
        "interrupted",
        "error",
    )
    private val terminalOutcomes = setOf("success", "cancelled", "interrupted", "error")
    private val dialogueActPattern = Regex("^[a-z][a-z0-9_.-]{0,127}$")
    private val verifiedOutcomeV1RefPattern = Regex(
        "^zara\\.verified-outcome/v1:(effect|outcome):[A-Za-z0-9][A-Za-z0-9._:/#-]{0,383}$"
    )
    private val verifiedOutcomeV2RefPattern = Regex(
        "^zara\\.verified-outcome/v2:([1-9][0-9]*):(effect|outcome):" +
            "[A-Za-z0-9][A-Za-z0-9._:/#-]{0,383}$"
    )

    private fun verifiedOutcomeRuntimeGeneration(reference: String): Long? {
        if (verifiedOutcomeV1RefPattern.matches(reference)) return null
        val match = verifiedOutcomeV2RefPattern.matchEntire(reference)
            ?: throw IllegalArgumentException("invalid verified outcome reference: $reference")
        return match.groupValues[1].toLongOrNull()
            ?: throw IllegalArgumentException("invalid verified outcome reference: $reference")
    }

    private fun validateVerifiedOutcomeTransition(
        currentRefs: List<String>,
        proposedRefs: List<String>,
        runtimeGeneration: Long,
        isNewTurn: Boolean,
    ): Set<String> {
        val currentSet = currentRefs.toSet()
        val proposedSet = proposedRefs.toSet()
        val removed = currentSet - proposedSet
        val added = proposedSet - currentSet
        val currentHasV2 = currentRefs.any { verifiedOutcomeRuntimeGeneration(it) != null }

        if (currentHasV2) {
            added.forEach { reference ->
                check(verifiedOutcomeRuntimeGeneration(reference) != null) {
                    "retired verified outcome replay rejected"
                }
            }
        }

        added.forEach { reference ->
            val evidenceGeneration = verifiedOutcomeRuntimeGeneration(reference)
            if (evidenceGeneration != null && evidenceGeneration != runtimeGeneration) {
                if (removed.isNotEmpty()) {
                    throw IllegalStateException("retired verified outcome replay rejected")
                }
                throw IllegalStateException("verified outcome generation mismatch rejected")
            }
        }

        if (!isNewTurn) {
            check(removed.isEmpty()) { "verified outcome evidence rewind rejected" }
            return added
        }

        if (removed.isNotEmpty()) {
            check(
                currentRefs.size == VERIFIED_OUTCOME_WINDOW &&
                    proposedRefs.size == VERIFIED_OUTCOME_WINDOW &&
                    removed.size == added.size &&
                    added.isNotEmpty()
            ) { "verified outcome evidence rewind rejected" }
            val dropCount = removed.size
            val retainedCount = VERIFIED_OUTCOME_WINDOW - dropCount
            check(proposedRefs.take(retainedCount) == currentRefs.drop(dropCount)) {
                "verified outcome evidence rewind rejected"
            }
            val appendedRefs = proposedRefs.drop(retainedCount)
            check(appendedRefs.toSet() == added) {
                "verified outcome evidence rewind rejected"
            }
            appendedRefs.forEach { reference ->
                val evidenceGeneration = verifiedOutcomeRuntimeGeneration(reference)
                if (evidenceGeneration == null) {
                    if (currentHasV2) {
                        throw IllegalStateException("retired verified outcome replay rejected")
                    }
                    throw IllegalStateException(
                        "verified outcome compaction requires generation-bound evidence"
                    )
                }
            }
            return added
        }

        return added
    }

    fun validatePayload(projection: SymbolicConversationProjection) {
        require(projection.conversationId.isNotEmpty()) { "conversationId must not be empty" }
        require(projection.turnId == null || projection.turnId.length in 1..512) {
            "turnId must be null or 1..512 characters"
        }
        require(projection.outcome in outcomes) { "unsupported symbolic outcome: ${projection.outcome}" }
        require(projection.projectionGeneration >= 1L) { "projectionGeneration must be >= 1" }
        require(projection.runtimeGeneration >= 0L) { "runtimeGeneration must be >= 0" }
        require(projection.projectGeneration >= 0L) { "projectGeneration must be >= 0" }
        require(projection.maxModelCalls >= 0L) { "maxModelCalls must be >= 0" }
        require(projection.providerCalls >= 0L) { "providerCalls must be >= 0" }
        require(projection.modelCalls >= 0L) { "modelCalls must be >= 0" }
        require(projection.modelCalls <= projection.maxModelCalls) {
            "modelCalls must not exceed maxModelCalls"
        }
        require((projection.projectId?.length ?: 0) <= 512) { "projectId exceeds 512 characters" }
        require(dialogueActPattern.matches(projection.dialogueAct)) {
            "dialogueAct must be a normalized symbolic act token"
        }
        require(projection.verifiedOutcomeRefs.size <= VERIFIED_OUTCOME_WINDOW) {
            "verifiedOutcomeRefs exceeds 64 entries"
        }
        require(projection.verifiedOutcomeRefs.distinct().size == projection.verifiedOutcomeRefs.size) {
            "verifiedOutcomeRefs must be unique"
        }
        projection.verifiedOutcomeRefs.forEach { ref ->
            val evidenceGeneration = verifiedOutcomeRuntimeGeneration(ref)
            require(evidenceGeneration == null || evidenceGeneration <= projection.runtimeGeneration) {
                "verified outcome generation must not exceed runtimeGeneration"
            }
        }
        require(projection.dialogueAct != "verified" || projection.verifiedOutcomeRefs.isNotEmpty()) {
            "verified projection requires verified outcome evidence"
        }
        require(
            projection.rendererProvenance.isEmpty() ||
                projection.rendererProvenance == SYMBOLIC_RENDERER_ID
        ) {
            "rendererProvenance must be empty or the canonical symbolic renderer"
        }
        require(projection.outcome != "success" || projection.rendererProvenance == SYMBOLIC_RENDERER_ID) {
            "successful projection requires canonical symbolic renderer"
        }
        PortableJsonValidator.requireObject(projection.dialogueStateJson, "dialogueStateJson")
        PortableJsonValidator.requireObjectArray(projection.discourseEntitiesJson, "discourseEntitiesJson")
        PortableJsonValidator.requireObjectArray(projection.unresolvedQuestionsJson, "unresolvedQuestionsJson")
        PortableJsonValidator.requireObjectArray(projection.expertEvidenceJson, "expertEvidenceJson")
        PortableJsonValidator.requireObjectArray(projection.verifiedFactsJson, "verifiedFactsJson")
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
            if (proposed.dialogueAct == "verified") {
                val v2Generations = proposed.verifiedOutcomeRefs
                    .mapNotNull(::verifiedOutcomeRuntimeGeneration)
                    .toSet()
                check(proposed.runtimeGeneration in v2Generations) {
                    "verified projection requires fresh outcome evidence"
                }
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
        check(!(current.turnId != null && proposed.turnId == null)) {
            "turnId rewind rejected"
        }
        val isNewTurn = proposed.turnId != current.turnId
        val freshVerifiedOutcomeRefs = validateVerifiedOutcomeTransition(
            current.verifiedOutcomeRefs,
            proposed.verifiedOutcomeRefs,
            runtimeGeneration = proposed.runtimeGeneration,
            isNewTurn = isNewTurn,
        )
        if (proposed.turnId == current.turnId) {
            if (current.turnId != null) {
                check(proposed.runtimeGeneration == current.runtimeGeneration) {
                    "same turn must preserve runtimeGeneration"
                }
            }
            if (proposed.dialogueAct == "verified") {
                check(
                    freshVerifiedOutcomeRefs.any { reference ->
                        verifiedOutcomeRuntimeGeneration(reference) == proposed.runtimeGeneration
                    }
                ) {
                    "verified projection requires fresh outcome evidence"
                }
            }
            check(current.outcome !in terminalOutcomes) {
                "terminal turn projection is immutable"
            }
        } else {
            check(proposed.runtimeGeneration > current.runtimeGeneration) {
                "new turn must advance runtimeGeneration"
            }
            if (proposed.dialogueAct == "verified") {
                check(
                    freshVerifiedOutcomeRefs.any { reference ->
                        verifiedOutcomeRuntimeGeneration(reference) == proposed.runtimeGeneration
                    }
                ) {
                    "verified projection requires fresh outcome evidence"
                }
            }
        }
        check(current.providersEnabled || !proposed.providersEnabled) {
            "provider policy widening rejected"
        }
        check(proposed.maxModelCalls <= current.maxModelCalls) {
            "model-call budget widening rejected"
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
        val providersEnabledValue = cursor.exactStoredLong("providers_enabled")
        check(providersEnabledValue == 0L || providersEnabledValue == 1L) {
            "stored providers_enabled must be SQLite integer 0 or 1"
        }
        val projection = SymbolicConversationProjection(
            conversationId = cursor.getString(cursor.getColumnIndexOrThrow("conversation_id")),
            projectionGeneration = cursor.exactStoredLong("projection_generation", minimum = 1L),
            runtimeGeneration = cursor.exactStoredLong("runtime_generation"),
            turnId = cursor.nullableString("turn_id"),
            outcome = cursor.getString(cursor.getColumnIndexOrThrow("outcome")),
            projectId = cursor.nullableString("project_id"),
            projectGeneration = cursor.exactStoredLong("project_generation"),
            dialogueAct = cursor.getString(cursor.getColumnIndexOrThrow("dialogue_act")),
            dialogueStateJson = cursor.getString(cursor.getColumnIndexOrThrow("dialogue_state_json")),
            discourseEntitiesJson = cursor.getString(cursor.getColumnIndexOrThrow("discourse_entities_json")),
            unresolvedQuestionsJson = cursor.getString(cursor.getColumnIndexOrThrow("unresolved_questions_json")),
            expertEvidenceJson = cursor.getString(cursor.getColumnIndexOrThrow("expert_evidence_json")),
            verifiedFactsJson = cursor.getString(cursor.getColumnIndexOrThrow("verified_facts_json")),
            verifiedOutcomeRefs = cursor.getString(cursor.getColumnIndexOrThrow("verified_outcome_refs"))
                .decodeVerifiedOutcomeRefs(),
            rendererProvenance = cursor.getString(cursor.getColumnIndexOrThrow("renderer_provenance")),
            providersEnabled = providersEnabledValue == 1L,
            maxModelCalls = cursor.exactStoredLong("max_model_calls"),
            providerCalls = cursor.exactStoredLong("provider_calls"),
            modelCalls = cursor.exactStoredLong("model_calls"),
            updatedAt = cursor.getString(cursor.getColumnIndexOrThrow("updated_at")),
        )
        SymbolicProjectionContract.validatePayload(projection)
        projection
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
        if (stored.turnId == null) putNull("turn_id") else put("turn_id", stored.turnId)
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

private fun String.decodeVerifiedOutcomeRefs(): List<String> =
    if (isEmpty()) emptyList() else split('\n')

private fun Cursor.exactStoredLong(column: String, minimum: Long = 0L): Long {
    val index = getColumnIndexOrThrow(column)
    check(getType(index) == Cursor.FIELD_TYPE_INTEGER) {
        "stored $column must use SQLite integer storage"
    }
    val value = getLong(index)
    check(value >= minimum) { "stored $column must be >= $minimum" }
    return value
}

private fun Cursor.nullableString(column: String): String? {
    val index = getColumnIndexOrThrow(column)
    return if (isNull(index)) null else getString(index)
}
