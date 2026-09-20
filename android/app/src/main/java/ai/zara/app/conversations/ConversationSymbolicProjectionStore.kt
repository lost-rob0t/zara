package ai.zara.app.conversations

import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption

private const val SYMBOLIC_PROJECTION_MAGIC = "ZARA-SYMBOLIC-CONVERSATIONS/1"
private const val MAX_SYMBOLIC_STORE_BYTES = 2 * 1024 * 1024
private const val MAX_SYMBOLIC_CONVERSATIONS = 256
private const val MAX_SYMBOLIC_TURNS = 512
private const val MAX_ID_CHARS = 128
private const val MAX_DIALOGUE_ACT_CHARS = 128
private const val MAX_INTENT_FRAME_CHARS = 4 * 1024
private const val MAX_RENDERER_CHARS = 128
private const val MAX_REF_CHARS = 512
private const val MAX_REFS = 64

enum class SymbolicTurnStatus {
    Running,
    Clarifying,
    Complete,
    Cancelled,
    Interrupted,
    Error,
    Unknown,
}

data class SymbolicTurnProjection(
    val turnId: String,
    val runtimeGeneration: Long,
    val projectId: String? = null,
    val projectGeneration: Long? = null,
    val dialogueAct: String,
    val intentFrame: String = "",
    val discourseEntityRefs: List<String> = emptyList(),
    val unresolvedSlots: List<String> = emptyList(),
    val expertEvidenceRefs: List<String> = emptyList(),
    val verifiedOutcomeRefs: List<String> = emptyList(),
    val renderer: String,
    val status: SymbolicTurnStatus,
    val modelCalls: Int = 0,
    val providerCalls: Int = 0,
) {
    init {
        requireBoundedToken(turnId, MAX_ID_CHARS, "Turn id")
        require(runtimeGeneration >= 0) { "Runtime generation must be non-negative" }
        projectId?.let { requireBoundedToken(it, MAX_ID_CHARS, "Project id") }
        projectGeneration?.let { require(it >= 0) { "Project generation must be non-negative" } }
        requireBoundedToken(dialogueAct, MAX_DIALOGUE_ACT_CHARS, "Dialogue act")
        requireBoundedText(intentFrame, MAX_INTENT_FRAME_CHARS, "Intent frame", allowBlank = true)
        requireBoundedRefs(discourseEntityRefs, "Discourse entity references")
        requireBoundedRefs(unresolvedSlots, "Unresolved slots")
        requireBoundedRefs(expertEvidenceRefs, "Expert evidence references")
        requireBoundedRefs(verifiedOutcomeRefs, "Verified outcome references")
        requireBoundedToken(renderer, MAX_RENDERER_CHARS, "Renderer")
        require(modelCalls == 0) { "Pure symbolic turns must record model_calls=0" }
        require(providerCalls == 0) { "Pure symbolic turns must record provider_calls=0" }
    }
}

data class SymbolicConversationProjection(
    val conversationId: String,
    val turns: List<SymbolicTurnProjection> = emptyList(),
)

data class SymbolicProjectionState(
    val conversations: List<SymbolicConversationProjection> = emptyList(),
    val loadFailure: String? = null,
) {
    fun conversation(conversationId: String): SymbolicConversationProjection? =
        conversations.firstOrNull { it.conversationId == conversationId }
}

/**
 * Durable structured projection keyed by the canonical Android conversation id.
 *
 * This store deliberately contains no user or assistant transcript text and therefore cannot
 * become a second conversation-history authority. The canonical ConversationStore owns transcript
 * lifecycle/history. This file only persists replayable symbolic dialogue metadata required to
 * reconstruct follow-ups, clarification and evidence references with providers disabled.
 */
class ConversationSymbolicProjectionStore(private val file: File) {
    @Volatile
    private var current: SymbolicProjectionState = load()

    @Synchronized
    fun state(): SymbolicProjectionState = current

    @Synchronized
    fun conversation(conversationId: String): SymbolicConversationProjection? =
        current.conversation(normalizeToken(conversationId, MAX_ID_CHARS, "Conversation id"))

    @Synchronized
    fun record(
        conversationId: String,
        turn: SymbolicTurnProjection,
    ): SymbolicProjectionState {
        ensureHealthy()
        val id = normalizeToken(conversationId, MAX_ID_CHARS, "Conversation id")
        val existingConversation = current.conversation(id)
            ?: SymbolicConversationProjection(conversationId = id)
        val latest = existingConversation.turns.lastOrNull()

        latest?.let { currentTurn ->
            check(turn.runtimeGeneration >= currentTurn.runtimeGeneration) {
                "Stale runtime generation rejected"
            }
            fenceProjectGeneration(currentTurn, turn)
        }

        val existingIndex = existingConversation.turns.indexOfFirst { it.turnId == turn.turnId }
        val nextTurns = if (existingIndex >= 0) {
            val existingTurn = existingConversation.turns[existingIndex]
            if (existingTurn == turn) return current
            check(turn.runtimeGeneration == existingTurn.runtimeGeneration) {
                "Turn runtime generation cannot change"
            }
            check(turn.projectId == existingTurn.projectId &&
                turn.projectGeneration == existingTurn.projectGeneration
            ) {
                "Turn project scope cannot change"
            }
            check(canTransition(existingTurn.status, turn.status)) {
                "Terminal or stale symbolic turn update rejected: ${existingTurn.status} -> ${turn.status}"
            }
            existingConversation.turns.toMutableList().apply { this[existingIndex] = turn }
        } else {
            check(existingConversation.turns.size < MAX_SYMBOLIC_TURNS) {
                "Symbolic turn limit reached"
            }
            existingConversation.turns + turn
        }

        val updatedConversation = existingConversation.copy(turns = nextTurns)
        val conversations = current.conversations.toMutableList()
        val conversationIndex = conversations.indexOfFirst { it.conversationId == id }
        if (conversationIndex >= 0) {
            conversations[conversationIndex] = updatedConversation
        } else {
            check(conversations.size < MAX_SYMBOLIC_CONVERSATIONS) {
                "Symbolic conversation limit reached"
            }
            conversations += updatedConversation
        }
        return commit(SymbolicProjectionState(conversations = conversations))
    }

    private fun fenceProjectGeneration(
        latest: SymbolicTurnProjection,
        incoming: SymbolicTurnProjection,
    ) {
        val latestGeneration = latest.projectGeneration ?: return
        val incomingGeneration = incoming.projectGeneration
            ?: throw IllegalStateException("Project generation cannot disappear from an active scoped conversation")
        check(incomingGeneration >= latestGeneration) {
            "Stale project generation rejected"
        }
        if (incoming.projectId != latest.projectId) {
            check(incomingGeneration > latestGeneration) {
                "Project switch requires a newer project generation"
            }
        }
    }

    private fun canTransition(from: SymbolicTurnStatus, to: SymbolicTurnStatus): Boolean =
        when (from) {
            SymbolicTurnStatus.Running -> to != SymbolicTurnStatus.Running
            SymbolicTurnStatus.Clarifying ->
                to != SymbolicTurnStatus.Running && to != SymbolicTurnStatus.Clarifying
            SymbolicTurnStatus.Complete,
            SymbolicTurnStatus.Cancelled,
            SymbolicTurnStatus.Interrupted,
            SymbolicTurnStatus.Error,
            SymbolicTurnStatus.Unknown,
            -> false
        }

    private fun ensureHealthy() {
        check(current.loadFailure == null) {
            "Symbolic conversation projection is corrupt or unsupported; preserve it for recovery"
        }
    }

    private fun commit(next: SymbolicProjectionState): SymbolicProjectionState {
        val clean = next.copy(loadFailure = null)
        persist(clean)
        current = clean
        return current
    }

    private fun load(): SymbolicProjectionState {
        if (!file.exists()) return SymbolicProjectionState()
        if (!file.isFile || file.length() !in 1..MAX_SYMBOLIC_STORE_BYTES.toLong()) {
            return degradedState()
        }
        return try {
            val (loaded, recoveredRunningTurn) =
                DataInputStream(FileInputStream(file).buffered()).use { input ->
                    require(input.readUTF() == SYMBOLIC_PROJECTION_MAGIC)
                    val conversationCount = input.readInt()
                    require(conversationCount in 0..MAX_SYMBOLIC_CONVERSATIONS)
                    var recoveredRunningTurn = false
                    val conversations = buildList(conversationCount) {
                        repeat(conversationCount) {
                            val conversationId = input.readBoundedString(MAX_ID_CHARS)
                            requireBoundedToken(conversationId, MAX_ID_CHARS, "Conversation id")
                            val turnCount = input.readInt()
                            require(turnCount in 0..MAX_SYMBOLIC_TURNS)
                            val turns = buildList(turnCount) {
                                repeat(turnCount) {
                                    val turn = input.readTurn()
                                    if (turn.status == SymbolicTurnStatus.Running) {
                                        recoveredRunningTurn = true
                                        add(turn.copy(status = SymbolicTurnStatus.Interrupted))
                                    } else {
                                        add(turn)
                                    }
                                }
                            }
                            require(turns.map { it.turnId }.toSet().size == turns.size)
                            add(SymbolicConversationProjection(conversationId, turns))
                        }
                    }
                    require(conversations.map { it.conversationId }.toSet().size == conversations.size)
                    require(input.read() == -1)
                    SymbolicProjectionState(conversations = conversations) to recoveredRunningTurn
                }
            if (recoveredRunningTurn) persist(loaded)
            loaded
        } catch (_: Exception) {
            degradedState()
        }
    }

    private fun DataInputStream.readTurn(): SymbolicTurnProjection {
        val turnId = readBoundedString(MAX_ID_CHARS)
        val runtimeGeneration = readLong()
        val projectId = if (readBoolean()) readBoundedString(MAX_ID_CHARS) else null
        val projectGeneration = if (readBoolean()) readLong() else null
        val dialogueAct = readBoundedString(MAX_DIALOGUE_ACT_CHARS)
        val intentFrame = readBoundedString(MAX_INTENT_FRAME_CHARS)
        val discourseEntityRefs = readRefs()
        val unresolvedSlots = readRefs()
        val expertEvidenceRefs = readRefs()
        val verifiedOutcomeRefs = readRefs()
        val renderer = readBoundedString(MAX_RENDERER_CHARS)
        val status = SymbolicTurnStatus.valueOf(readBoundedString(32))
        val modelCalls = readInt()
        val providerCalls = readInt()
        return SymbolicTurnProjection(
            turnId = turnId,
            runtimeGeneration = runtimeGeneration,
            projectId = projectId,
            projectGeneration = projectGeneration,
            dialogueAct = dialogueAct,
            intentFrame = intentFrame,
            discourseEntityRefs = discourseEntityRefs,
            unresolvedSlots = unresolvedSlots,
            expertEvidenceRefs = expertEvidenceRefs,
            verifiedOutcomeRefs = verifiedOutcomeRefs,
            renderer = renderer,
            status = status,
            modelCalls = modelCalls,
            providerCalls = providerCalls,
        )
    }

    private fun persist(state: SymbolicProjectionState) {
        val directory = file.absoluteFile.parentFile
            ?: throw IllegalStateException("Symbolic projection path has no parent directory")
        check(directory.exists() || directory.mkdirs()) {
            "Symbolic projection directory could not be created"
        }
        val temp = Files.createTempFile(directory.toPath(), ".${file.name}.", ".tmp").toFile()
        try {
            DataOutputStream(FileOutputStream(temp).buffered()).use { output ->
                output.writeUTF(SYMBOLIC_PROJECTION_MAGIC)
                output.writeInt(state.conversations.size)
                state.conversations.forEach { conversation ->
                    output.writeBoundedString(conversation.conversationId, MAX_ID_CHARS)
                    output.writeInt(conversation.turns.size)
                    conversation.turns.forEach(output::writeTurn)
                }
            }
            check(temp.length() in 1..MAX_SYMBOLIC_STORE_BYTES.toLong()) {
                "Symbolic conversation projection exceeds the bounded store size"
            }
            replace(temp, file)
        } finally {
            if (temp.exists()) temp.delete()
        }
    }

    private fun DataOutputStream.writeTurn(turn: SymbolicTurnProjection) {
        writeBoundedString(turn.turnId, MAX_ID_CHARS)
        writeLong(turn.runtimeGeneration)
        writeBoolean(turn.projectId != null)
        turn.projectId?.let { writeBoundedString(it, MAX_ID_CHARS) }
        writeBoolean(turn.projectGeneration != null)
        turn.projectGeneration?.let(::writeLong)
        writeBoundedString(turn.dialogueAct, MAX_DIALOGUE_ACT_CHARS)
        writeBoundedString(turn.intentFrame, MAX_INTENT_FRAME_CHARS)
        writeRefs(turn.discourseEntityRefs)
        writeRefs(turn.unresolvedSlots)
        writeRefs(turn.expertEvidenceRefs)
        writeRefs(turn.verifiedOutcomeRefs)
        writeBoundedString(turn.renderer, MAX_RENDERER_CHARS)
        writeBoundedString(turn.status.name, 32)
        writeInt(turn.modelCalls)
        writeInt(turn.providerCalls)
    }

    private fun DataOutputStream.writeRefs(refs: List<String>) {
        writeInt(refs.size)
        refs.forEach { writeBoundedString(it, MAX_REF_CHARS) }
    }

    private fun DataInputStream.readRefs(): List<String> {
        val count = readInt()
        require(count in 0..MAX_REFS)
        return buildList(count) {
            repeat(count) { add(readBoundedString(MAX_REF_CHARS)) }
        }
    }

    private fun DataOutputStream.writeBoundedString(value: String, maxChars: Int) {
        require(value.length <= maxChars)
        val bytes = value.toByteArray(StandardCharsets.UTF_8)
        require(bytes.size <= maxChars * 4)
        writeInt(bytes.size)
        write(bytes)
    }

    private fun DataInputStream.readBoundedString(maxChars: Int): String {
        val size = readInt()
        require(size in 0..(maxChars * 4))
        val bytes = ByteArray(size)
        readFully(bytes)
        val value = String(bytes, StandardCharsets.UTF_8)
        require(value.length <= maxChars)
        return value
    }

    private fun replace(source: File, destination: File) {
        try {
            Files.move(
                source.toPath(),
                destination.toPath(),
                StandardCopyOption.ATOMIC_MOVE,
                StandardCopyOption.REPLACE_EXISTING,
            )
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(source.toPath(), destination.toPath(), StandardCopyOption.REPLACE_EXISTING)
        }
    }

    private fun degradedState(): SymbolicProjectionState = SymbolicProjectionState(
        loadFailure = "Symbolic conversation projection is corrupt or unsupported",
    )
}

private fun normalizeToken(raw: String, maxChars: Int, label: String): String {
    val value = raw.trim()
    requireBoundedToken(value, maxChars, label)
    return value
}

private fun requireBoundedToken(value: String, maxChars: Int, label: String) {
    require(value.isNotBlank()) { "$label is required" }
    require(value.length <= maxChars) { "$label is too long" }
    require(value.none(Char::isISOControl)) { "$label contains control characters" }
}

private fun requireBoundedText(
    value: String,
    maxChars: Int,
    label: String,
    allowBlank: Boolean,
) {
    if (!allowBlank) require(value.isNotBlank()) { "$label is required" }
    require(value.length <= maxChars) { "$label is too long" }
    require(value.none { it == '\u0000' }) { "$label contains invalid NUL" }
}

private fun requireBoundedRefs(refs: List<String>, label: String) {
    require(refs.size <= MAX_REFS) { "$label exceed the reference limit" }
    require(refs.distinct().size == refs.size) { "$label contain duplicates" }
    refs.forEach { requireBoundedToken(it, MAX_REF_CHARS, label) }
}
