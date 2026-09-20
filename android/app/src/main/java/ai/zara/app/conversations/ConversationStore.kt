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
import java.util.UUID
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicLong

private const val CONVERSATION_STORE_MAGIC = "ZARA-CONVERSATIONS/1"
private const val MAX_CONVERSATION_STORE_BYTES = 4 * 1024 * 1024
private const val MAX_CONVERSATIONS = 256
private const val MAX_TURNS_PER_CONVERSATION = 512
private const val MAX_TITLE_CHARS = 120
private const val MAX_CONVERSATION_ID_CHARS = 128
private const val MAX_PROJECT_ID_CHARS = 128
private const val MAX_REMOTE_CONVERSATION_ID_CHARS = 256
private const val MAX_TEXT_CHARS = 64 * 1024
private const val DEFAULT_TITLE = "New chat"
private const val INTERRUPTED_MESSAGE = "Interrupted before completion."

private data class ConversationStoreLease(
    val path: String,
    val generation: Long,
)

private object ConversationStoreLeaseRegistry {
    private val generations = ConcurrentHashMap<String, AtomicLong>()

    fun observe(file: File): ConversationStoreLease {
        val path = file.absoluteFile.path
        val generation = generations
            .computeIfAbsent(path) { AtomicLong(0) }
            .get()
        return ConversationStoreLease(path = path, generation = generation)
    }

    fun advance(file: File): ConversationStoreLease {
        val path = file.absoluteFile.path
        val generation = generations
            .computeIfAbsent(path) { AtomicLong(0) }
            .incrementAndGet()
        return ConversationStoreLease(path = path, generation = generation)
    }

    fun requireCurrent(lease: ConversationStoreLease) {
        val currentGeneration = generations[lease.path]?.get()
        check(currentGeneration == lease.generation) {
            "Conversation store instance is stale after lifecycle recreation"
        }
    }
}

enum class ConversationStatus {
    Empty,
    Running,
    Success,
    Failed,
    Interrupted,
}

data class ConversationTurn(
    val userText: String,
    val assistantText: String? = null,
    val success: Boolean? = null,
)

data class ConversationRecord(
    val id: String,
    val title: String = DEFAULT_TITLE,
    val pinned: Boolean = false,
    val projectId: String? = null,
    val remoteConversationId: String? = null,
    val status: ConversationStatus = ConversationStatus.Empty,
    val turns: List<ConversationTurn> = emptyList(),
    val createdAtEpochMs: Long,
    val updatedAtEpochMs: Long,
) {
    val localConversationId: String
        get() = "local-chat:$id"
}

data class ConversationState(
    val conversations: List<ConversationRecord> = emptyList(),
    val selectedConversationId: String? = null,
    val loadFailure: String? = null,
) {
    val selectedConversation: ConversationRecord?
        get() = selectedConversationId?.let(::conversation)

    val pinnedConversations: List<ConversationRecord>
        get() = conversations
            .asSequence()
            .filter { it.pinned }
            .sortedWith(compareByDescending<ConversationRecord> { it.updatedAtEpochMs }.thenByDescending { it.createdAtEpochMs })
            .toList()

    val recentConversations: List<ConversationRecord>
        get() = conversations
            .asSequence()
            .filterNot { it.pinned }
            .sortedWith(compareByDescending<ConversationRecord> { it.updatedAtEpochMs }.thenByDescending { it.createdAtEpochMs })
            .toList()

    fun conversation(id: String): ConversationRecord? = conversations.firstOrNull { it.id == id }
}

class ConversationStore(
    private val file: File,
    private val idFactory: () -> String = { UUID.randomUUID().toString() },
    private val clock: () -> Long = { System.currentTimeMillis() },
) {
    private var lease = ConversationStoreLeaseRegistry.observe(file)

    @Volatile
    private var current: ConversationState

    init {
        val (loaded, recoveredRunningTurn) = load()
        current = loaded
        if (recoveredRunningTurn) {
            lease = ConversationStoreLeaseRegistry.advance(file)
        }
    }

    @Synchronized
    fun state(): ConversationState = current

    @Synchronized
    fun create(projectId: String? = null): ConversationRecord {
        ensureHealthy()
        check(current.conversations.size < MAX_CONVERSATIONS) { "Conversation limit reached" }
        val id = normalizeId(idFactory())
        require(current.conversation(id) == null) { "Conversation id already exists" }
        val normalizedProjectId = normalizeOptionalId(projectId, MAX_PROJECT_ID_CHARS, "Project id")
        val now = clock()
        val conversation = ConversationRecord(
            id = id,
            projectId = normalizedProjectId,
            createdAtEpochMs = now,
            updatedAtEpochMs = now,
        )
        commit(
            current.copy(
                conversations = current.conversations + conversation,
                selectedConversationId = id,
            )
        )
        return conversation
    }

    @Synchronized
    fun select(conversationId: String): ConversationState {
        ensureHealthy()
        val id = normalizeId(conversationId)
        require(current.conversation(id) != null) { "Unknown conversation: $id" }
        return commit(current.copy(selectedConversationId = id))
    }

    @Synchronized
    fun rename(conversationId: String, rawTitle: String): ConversationState {
        ensureHealthy()
        val title = normalizeTitle(rawTitle)
        return updateConversation(conversationId) { conversation ->
            conversation.copy(title = title, updatedAtEpochMs = clock())
        }
    }

    @Synchronized
    fun setPinned(conversationId: String, pinned: Boolean): ConversationState {
        ensureHealthy()
        return updateConversation(conversationId) { conversation ->
            conversation.copy(pinned = pinned, updatedAtEpochMs = clock())
        }
    }

    @Synchronized
    fun moveToProject(conversationId: String, projectId: String?): ConversationState {
        ensureHealthy()
        val normalizedProjectId = normalizeOptionalId(projectId, MAX_PROJECT_ID_CHARS, "Project id")
        return updateConversation(conversationId) { conversation ->
            conversation.copy(projectId = normalizedProjectId, updatedAtEpochMs = clock())
        }
    }

    @Synchronized
    fun beginTurn(conversationId: String, rawUserText: String): ConversationState {
        ensureHealthy()
        val userText = normalizeText(rawUserText, "User text")
        return updateConversation(conversationId) { conversation ->
            check(conversation.status != ConversationStatus.Running) {
                "Conversation already has a running turn"
            }
            check(conversation.turns.size < MAX_TURNS_PER_CONVERSATION) {
                "Conversation turn limit reached"
            }
            val title = if (conversation.turns.isEmpty() && conversation.title == DEFAULT_TITLE) {
                deriveTitle(userText)
            } else {
                conversation.title
            }
            conversation.copy(
                title = title,
                status = ConversationStatus.Running,
                turns = conversation.turns + ConversationTurn(userText = userText),
                updatedAtEpochMs = clock(),
            )
        }
    }

    @Synchronized
    fun completeTurn(
        conversationId: String,
        assistantText: String,
        success: Boolean,
        remoteConversationId: String? = null,
    ): ConversationState {
        ensureHealthy()
        val response = normalizeText(assistantText, "Assistant text", allowBlank = true)
        val normalizedRemoteId = normalizeOptionalId(
            remoteConversationId,
            MAX_REMOTE_CONVERSATION_ID_CHARS,
            "Remote conversation id",
        )
        return updateConversation(conversationId) { conversation ->
            check(conversation.turns.isNotEmpty()) { "Conversation has no running turn" }
            val index = conversation.turns.lastIndex
            val pending = conversation.turns[index]
            check(pending.success == null) { "Conversation has no running turn" }
            val turns = conversation.turns.toMutableList()
            turns[index] = pending.copy(assistantText = response, success = success)
            conversation.copy(
                remoteConversationId = normalizedRemoteId ?: conversation.remoteConversationId,
                status = if (success) ConversationStatus.Success else ConversationStatus.Failed,
                turns = turns,
                updatedAtEpochMs = clock(),
            )
        }
    }

    @Synchronized
    fun failTurn(conversationId: String, message: String): ConversationState =
        completeTurn(conversationId, message, success = false)

    private fun updateConversation(
        conversationId: String,
        transform: (ConversationRecord) -> ConversationRecord,
    ): ConversationState {
        val id = normalizeId(conversationId)
        require(current.conversation(id) != null) { "Unknown conversation: $id" }
        val conversations = current.conversations.map { conversation ->
            if (conversation.id == id) transform(conversation) else conversation
        }
        return commit(current.copy(conversations = conversations))
    }

    private fun ensureHealthy() {
        ConversationStoreLeaseRegistry.requireCurrent(lease)
        check(current.loadFailure == null) {
            "Conversation history is degraded; preserve the file for recovery before changing chats"
        }
    }

    private fun commit(next: ConversationState): ConversationState {
        val clean = next.copy(loadFailure = null)
        persist(clean)
        current = clean
        return current
    }

    private fun load(): Pair<ConversationState, Boolean> {
        if (!file.exists()) return ConversationState() to false
        if (!file.isFile || file.length() !in 1..MAX_CONVERSATION_STORE_BYTES.toLong()) {
            return degradedState() to false
        }
        return try {
            val (loaded, recoveredRunningTurn) =
                DataInputStream(FileInputStream(file).buffered()).use { input ->
                    require(input.readUTF() == CONVERSATION_STORE_MAGIC)
                    val selectedConversationId =
                        input.readBoundedString(MAX_CONVERSATION_ID_CHARS).ifEmpty { null }
                    val count = input.readInt()
                    require(count in 0..MAX_CONVERSATIONS)
                    var recoveredRunningTurn = false
                    val conversations = buildList(count) {
                        repeat(count) {
                            val conversation = input.readConversation()
                            if (conversation.status == ConversationStatus.Running) {
                                recoveredRunningTurn = true
                            }
                            add(conversation.recoverInterrupted())
                        }
                    }
                    require(conversations.map { it.id }.toSet().size == conversations.size)
                    require(
                        selectedConversationId == null ||
                            conversations.any { it.id == selectedConversationId }
                    )
                    require(input.read() == -1)
                    ConversationState(
                        conversations = conversations,
                        selectedConversationId = selectedConversationId,
                    ) to recoveredRunningTurn
                }
            if (recoveredRunningTurn) {
                persist(loaded)
            }
            loaded to recoveredRunningTurn
        } catch (_: Exception) {
            degradedState() to false
        }
    }

    private fun DataInputStream.readConversation(): ConversationRecord {
        val id = normalizeId(readBoundedString(MAX_CONVERSATION_ID_CHARS))
        val title = normalizeTitle(readBoundedString(MAX_TITLE_CHARS))
        val pinned = readBoolean()
        val projectId = readBoundedString(MAX_PROJECT_ID_CHARS).ifEmpty { null }
            ?.let { normalizeOptionalId(it, MAX_PROJECT_ID_CHARS, "Project id") }
        val remoteConversationId = readBoundedString(MAX_REMOTE_CONVERSATION_ID_CHARS).ifEmpty { null }
            ?.let {
                normalizeOptionalId(
                    it,
                    MAX_REMOTE_CONVERSATION_ID_CHARS,
                    "Remote conversation id",
                )
            }
        val status = ConversationStatus.valueOf(readBoundedString(32))
        val createdAt = readLong()
        val updatedAt = readLong()
        require(createdAt >= 0)
        require(updatedAt >= 0)
        val turnCount = readInt()
        require(turnCount in 0..MAX_TURNS_PER_CONVERSATION)
        val turns = buildList(turnCount) {
            repeat(turnCount) {
                val userText = normalizeText(readBoundedString(MAX_TEXT_CHARS), "User text")
                val assistantPresent = readBoolean()
                val assistantText = if (assistantPresent) {
                    normalizeText(readBoundedString(MAX_TEXT_CHARS), "Assistant text", allowBlank = true)
                } else {
                    null
                }
                val success = when (val raw = readByte().toInt()) {
                    -1 -> null
                    0 -> false
                    1 -> true
                    else -> error("Invalid turn status: $raw")
                }
                require((success == null) == (assistantText == null))
                add(ConversationTurn(userText, assistantText, success))
            }
        }
        return ConversationRecord(
            id = id,
            title = title,
            pinned = pinned,
            projectId = projectId,
            remoteConversationId = remoteConversationId,
            status = status,
            turns = turns,
            createdAtEpochMs = createdAt,
            updatedAtEpochMs = updatedAt,
        )
    }

    private fun ConversationRecord.recoverInterrupted(): ConversationRecord {
        if (status != ConversationStatus.Running) return this
        val turns = turns.toMutableList()
        val pendingIndex = turns.indexOfLast { it.success == null }
        if (pendingIndex >= 0) {
            val pending = turns[pendingIndex]
            turns[pendingIndex] = pending.copy(
                assistantText = INTERRUPTED_MESSAGE,
                success = false,
            )
        }
        return copy(status = ConversationStatus.Interrupted, turns = turns)
    }

    private fun persist(state: ConversationState) {
        val directory = file.absoluteFile.parentFile
            ?: throw IllegalStateException("Conversation store path has no parent directory")
        check(directory.exists() || directory.mkdirs()) {
            "Conversation store directory could not be created"
        }
        val temp = Files.createTempFile(directory.toPath(), ".${file.name}.", ".tmp").toFile()
        try {
            DataOutputStream(FileOutputStream(temp).buffered()).use { output ->
                output.writeUTF(CONVERSATION_STORE_MAGIC)
                output.writeBoundedString(state.selectedConversationId.orEmpty(), MAX_CONVERSATION_ID_CHARS)
                output.writeInt(state.conversations.size)
                state.conversations.forEach { output.writeConversation(it) }
            }
            check(temp.length() in 1..MAX_CONVERSATION_STORE_BYTES.toLong()) {
                "Conversation history exceeds the bounded store size"
            }
            replace(temp, file)
        } finally {
            if (temp.exists()) temp.delete()
        }
    }

    private fun DataOutputStream.writeConversation(conversation: ConversationRecord) {
        writeBoundedString(conversation.id, MAX_CONVERSATION_ID_CHARS)
        writeBoundedString(conversation.title, MAX_TITLE_CHARS)
        writeBoolean(conversation.pinned)
        writeBoundedString(conversation.projectId.orEmpty(), MAX_PROJECT_ID_CHARS)
        writeBoundedString(conversation.remoteConversationId.orEmpty(), MAX_REMOTE_CONVERSATION_ID_CHARS)
        writeBoundedString(conversation.status.name, 32)
        writeLong(conversation.createdAtEpochMs)
        writeLong(conversation.updatedAtEpochMs)
        writeInt(conversation.turns.size)
        conversation.turns.forEach { turn ->
            writeBoundedString(turn.userText, MAX_TEXT_CHARS)
            writeBoolean(turn.assistantText != null)
            turn.assistantText?.let { writeBoundedString(it, MAX_TEXT_CHARS) }
            writeByte(
                when (turn.success) {
                    null -> -1
                    false -> 0
                    true -> 1
                }
            )
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

    private fun deriveTitle(text: String): String {
        val compact = text.trim().replace(Regex("\\s+"), " ")
        if (compact.length <= 48) return compact
        return compact.take(47).trimEnd() + "…"
    }

    private fun normalizeTitle(rawTitle: String): String {
        val title = rawTitle.trim()
        require(title.isNotEmpty()) { "Conversation title is required" }
        require(title.length <= MAX_TITLE_CHARS) { "Conversation title is too long" }
        require(title.none(Char::isISOControl)) { "Conversation title contains control characters" }
        return title
    }

    private fun normalizeId(rawId: String): String =
        normalizeOptionalId(rawId, MAX_CONVERSATION_ID_CHARS, "Conversation id")
            ?: throw IllegalArgumentException("Conversation id is required")

    private fun normalizeOptionalId(rawId: String?, maxChars: Int, label: String): String? {
        val id = rawId?.trim() ?: return null
        if (id.isEmpty()) return null
        require(id.length <= maxChars) { "$label is too long" }
        require(id.none(Char::isISOControl)) { "$label contains control characters" }
        return id
    }

    private fun normalizeText(rawText: String, label: String, allowBlank: Boolean = false): String {
        val text = rawText.trim()
        if (!allowBlank) require(text.isNotEmpty()) { "$label is required" }
        require(text.length <= MAX_TEXT_CHARS) { "$label is too long" }
        return text
    }

    private fun degradedState(): ConversationState = ConversationState(
        loadFailure = "Conversation history is corrupt or unsupported",
    )
}
