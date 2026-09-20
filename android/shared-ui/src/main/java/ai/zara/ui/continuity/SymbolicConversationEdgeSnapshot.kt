package ai.zara.ui.continuity

import java.io.ByteArrayInputStream
import java.io.ByteArrayOutputStream
import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.IOException
import java.nio.charset.StandardCharsets

/**
 * Read-only edge projection of Zara's canonical conversation truth.
 *
 * This value is transport/presentation data. It is not a conversation store,
 * memory database, expert registry, or authority grant. Producers must derive
 * it from the canonical conversation projection owned by the active runtime.
 */
data class SymbolicConversationEdgeSnapshot(
    val principalId: String,
    val conversationId: String,
    val projectionGeneration: Long,
    val runtimeGeneration: Long,
    val projectId: String? = null,
    val projectGeneration: Long = 0,
    val dialogueAct: String = "",
    val discourseEntityRefs: List<String> = emptyList(),
    val unresolvedQuestionRefs: List<String> = emptyList(),
    val expertEvidenceRefs: List<String> = emptyList(),
    val verifiedOutcomeRefs: List<String> = emptyList(),
    val rendererProvenance: String = "",
    val providersEnabled: Boolean = false,
    val maxModelCalls: Long = 0,
    val modelCalls: Long = 0,
    val providerCalls: Long = 0,
) {
    fun validate() {
        requireBoundedText(principalId, MAX_ID_CHARS, "principalId", allowBlank = false)
        requireBoundedText(conversationId, MAX_ID_CHARS, "conversationId", allowBlank = false)
        require(projectionGeneration >= 1) { "projectionGeneration must be >= 1" }
        require(runtimeGeneration >= 0) { "runtimeGeneration must be >= 0" }
        require(projectGeneration >= 0) { "projectGeneration must be >= 0" }
        projectId?.let {
            requireBoundedText(it, MAX_ID_CHARS, "projectId", allowBlank = false)
        }
        requireBoundedText(dialogueAct, MAX_ACT_CHARS, "dialogueAct", allowBlank = true)
        validateRefs(discourseEntityRefs, "discourseEntityRefs")
        validateRefs(unresolvedQuestionRefs, "unresolvedQuestionRefs")
        validateRefs(expertEvidenceRefs, "expertEvidenceRefs")
        validateRefs(verifiedOutcomeRefs, "verifiedOutcomeRefs")
        requireBoundedText(
            rendererProvenance,
            MAX_RENDERER_CHARS,
            "rendererProvenance",
            allowBlank = true,
        )
        require(maxModelCalls >= 0) { "max model calls must be >= 0" }
        require(modelCalls >= 0) { "model calls must be >= 0" }
        require(providerCalls >= 0) { "provider calls must be >= 0" }
        require(modelCalls <= maxModelCalls) { "model calls exceed declared max model calls" }
    }

    fun assertPureSymbolic() {
        validate()
        check(!providersEnabled) { "pure-symbolic edge projection has providers enabled" }
        check(maxModelCalls == 0L) {
            "pure-symbolic edge projection max model calls must be 0: $maxModelCalls"
        }
        check(modelCalls == 0L) { "pure-symbolic edge projection recorded model calls: $modelCalls" }
        check(providerCalls == 0L) {
            "pure-symbolic edge projection recorded provider calls: $providerCalls"
        }
        check(dialogueAct in ZARA_SYMBOLIC_DIALOGUE_V1_ACTS) {
            "pure-symbolic edge projection dialogueAct is not a ZARA-SYMBOLIC-DIALOGUE/1 act: $dialogueAct"
        }
        check(rendererProvenance == ZARA_SYMBOLIC_DIALOGUE_V1_RENDERER) {
            "pure-symbolic edge projection rendererProvenance must be $ZARA_SYMBOLIC_DIALOGUE_V1_RENDERER: $rendererProvenance"
        }
        check(dialogueAct != "verified" || verifiedOutcomeRefs.isNotEmpty()) {
            "pure-symbolic verified edge projection requires verified outcome evidence"
        }
        check(dialogueAct != "expert_answer" || expertEvidenceRefs.isNotEmpty()) {
            "pure-symbolic expert_answer edge projection requires expert evidence"
        }
    }

    private fun validateRefs(values: List<String>, label: String) {
        require(values.size <= MAX_REFS) { "$label exceeds $MAX_REFS entries" }
        require(values.distinct().size == values.size) { "$label contains duplicate references" }
        values.forEach { value ->
            requireBoundedText(value, MAX_REF_CHARS, label, allowBlank = false)
        }
    }

    private fun requireBoundedText(value: String, maxChars: Int, label: String, allowBlank: Boolean) {
        if (!allowBlank) require(value.isNotBlank()) { "$label must not be blank" }
        require(value.length <= maxChars) { "$label exceeds $maxChars characters" }
        require(value.none(Char::isISOControl)) { "$label contains control characters" }
    }

    companion object {
        const val MAX_REFS = 16
        internal const val MAX_ID_CHARS = 128
        internal const val MAX_ACT_CHARS = 96
        internal const val MAX_REF_CHARS = 128
        internal const val MAX_RENDERER_CHARS = 256
        internal const val ZARA_SYMBOLIC_DIALOGUE_V1_RENDERER = "symbolic-dcg/v1"
        internal val ZARA_SYMBOLIC_DIALOGUE_V1_ACTS = setOf(
            "greeting",
            "help",
            "acknowledgement",
            "cancelled",
            "clarify",
            "choose",
            "invalid",
            "dispatch_required",
            "verified",
            "denied",
            "unavailable",
            "error",
            "expert_answer",
            "unsupported",
        )
    }
}

/** Dependency-free bounded wire codec suitable for phone -> Wear/edge projection. */
object SymbolicConversationEdgeCodec {
    const val MAX_WIRE_BYTES = 32 * 1024
    private const val MAGIC = "ZARA-SYMBOLIC-EDGE/1"

    fun encode(snapshot: SymbolicConversationEdgeSnapshot): ByteArray {
        snapshot.validate()
        val bytes = ByteArrayOutputStream()
        DataOutputStream(bytes).use { output ->
            output.writeString(MAGIC, MAGIC.length)
            output.writeString(
                snapshot.principalId,
                SymbolicConversationEdgeSnapshot.MAX_ID_CHARS,
            )
            output.writeString(
                snapshot.conversationId,
                SymbolicConversationEdgeSnapshot.MAX_ID_CHARS,
            )
            output.writeLong(snapshot.projectionGeneration)
            output.writeLong(snapshot.runtimeGeneration)
            output.writeNullableString(
                snapshot.projectId,
                SymbolicConversationEdgeSnapshot.MAX_ID_CHARS,
            )
            output.writeLong(snapshot.projectGeneration)
            output.writeString(
                snapshot.dialogueAct,
                SymbolicConversationEdgeSnapshot.MAX_ACT_CHARS,
            )
            output.writeRefs(snapshot.discourseEntityRefs)
            output.writeRefs(snapshot.unresolvedQuestionRefs)
            output.writeRefs(snapshot.expertEvidenceRefs)
            output.writeRefs(snapshot.verifiedOutcomeRefs)
            output.writeString(
                snapshot.rendererProvenance,
                SymbolicConversationEdgeSnapshot.MAX_RENDERER_CHARS,
            )
            output.writeBoolean(snapshot.providersEnabled)
            output.writeLong(snapshot.maxModelCalls)
            output.writeLong(snapshot.modelCalls)
            output.writeLong(snapshot.providerCalls)
        }
        return bytes.toByteArray().also { encoded ->
            require(encoded.size in 1..MAX_WIRE_BYTES) {
                "symbolic edge snapshot exceeds $MAX_WIRE_BYTES wire bytes"
            }
        }
    }

    fun decode(encoded: ByteArray): SymbolicConversationEdgeSnapshot {
        require(encoded.size in 1..MAX_WIRE_BYTES) {
            "symbolic edge snapshot wire size is invalid"
        }
        try {
            DataInputStream(ByteArrayInputStream(encoded)).use { input ->
                require(input.readString(MAGIC.length) == MAGIC) {
                    "symbolic edge snapshot magic is invalid"
                }
                val snapshot = SymbolicConversationEdgeSnapshot(
                    principalId = input.readString(SymbolicConversationEdgeSnapshot.MAX_ID_CHARS),
                    conversationId = input.readString(SymbolicConversationEdgeSnapshot.MAX_ID_CHARS),
                    projectionGeneration = input.readLong(),
                    runtimeGeneration = input.readLong(),
                    projectId = input.readNullableString(SymbolicConversationEdgeSnapshot.MAX_ID_CHARS),
                    projectGeneration = input.readLong(),
                    dialogueAct = input.readString(SymbolicConversationEdgeSnapshot.MAX_ACT_CHARS),
                    discourseEntityRefs = input.readRefs(),
                    unresolvedQuestionRefs = input.readRefs(),
                    expertEvidenceRefs = input.readRefs(),
                    verifiedOutcomeRefs = input.readRefs(),
                    rendererProvenance = input.readString(SymbolicConversationEdgeSnapshot.MAX_RENDERER_CHARS),
                    providersEnabled = input.readBoolean(),
                    maxModelCalls = input.readLong(),
                    modelCalls = input.readLong(),
                    providerCalls = input.readLong(),
                )
                require(input.read() == -1) { "symbolic edge snapshot contains trailing bytes" }
                snapshot.validate()
                return snapshot
            }
        } catch (error: IOException) {
            throw IllegalArgumentException("symbolic edge snapshot is truncated or malformed", error)
        }
    }

    private fun DataOutputStream.writeRefs(values: List<String>) {
        writeInt(values.size)
        values.forEach { value ->
            writeString(value, SymbolicConversationEdgeSnapshot.MAX_REF_CHARS)
        }
    }

    private fun DataInputStream.readRefs(): List<String> {
        val count = readInt()
        require(count in 0..SymbolicConversationEdgeSnapshot.MAX_REFS) {
            "symbolic edge reference count is invalid"
        }
        return List(count) {
            readString(SymbolicConversationEdgeSnapshot.MAX_REF_CHARS)
        }
    }

    private fun DataOutputStream.writeNullableString(value: String?, maxChars: Int) {
        writeBoolean(value != null)
        if (value != null) writeString(value, maxChars)
    }

    private fun DataInputStream.readNullableString(maxChars: Int): String? =
        if (readBoolean()) readString(maxChars) else null

    private fun DataOutputStream.writeString(value: String, maxChars: Int) {
        require(value.length <= maxChars)
        val encoded = value.toByteArray(StandardCharsets.UTF_8)
        require(encoded.size <= maxChars * 4)
        writeInt(encoded.size)
        write(encoded)
    }

    private fun DataInputStream.readString(maxChars: Int): String {
        val size = readInt()
        require(size in 0..(maxChars * 4)) { "symbolic edge string byte length is invalid" }
        val encoded = ByteArray(size)
        readFully(encoded)
        return String(encoded, StandardCharsets.UTF_8).also { value ->
            require(value.length <= maxChars) { "symbolic edge string exceeds character bound" }
        }
    }
}
