package ai.zara.app.conversations

import ai.zara.app.history.HistoryMessage
import ai.zara.app.history.HistoryMessageRole
import ai.zara.app.history.HistoryMessageStatus
import ai.zara.app.history.PortableConversationStore
import ai.zara.app.history.fencePendingSymbolicProject
import java.io.DataInputStream
import java.io.DataOutputStream
import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream
import java.nio.charset.StandardCharsets
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.Files
import java.nio.file.StandardCopyOption
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.util.UUID

private const val UI_METADATA_MAGIC = "ZARA-CONVERSATION-UI/1"
private const val UI_METADATA_LOAD_FAILURE = "Conversation UI metadata is corrupt or unsupported"
private const val MAX_UI_METADATA_BYTES = 512 * 1024
private const val MAX_CONVERSATIONS = 256
private const val MAX_ID_CHARS = 256
private const val MAX_PROJECT_ID_CHARS = 512
private const val DEFAULT_TITLE = "New chat"
private const val INTERRUPTED_MESSAGE = "Interrupted before completion."

private data class ConversationUiMetadata(
    val pinned: Boolean = false,
    val projectId: String? = null,
    val remoteConversationId: String? = null,
)

private data class ConversationUiMetadataState(
    val selectedConversationId: String? = null,
    val conversations: Map<String, ConversationUiMetadata> = emptyMap(),
)

/**
 * Android chat facade over Zara's canonical portable conversation database.
 *
 * Message history, lifecycle recovery, and symbolic projection ownership live exclusively in
 * [PortableConversationStore] (`zara.db`). The small sidecar handled here contains UI-only
 * metadata that the portable history ABI does not model yet: selection, pinning, project binding,
 * and a remote server conversation id. It never stores user or assistant message content.
 *
 * A legacy `conversations.bin` can be supplied once for migration. Its messages are copied into
 * the canonical store and the legacy file is retired to a `.migrated` backup before this facade is
 * used for further writes, so Android never runs two writable history owners in parallel.
 */
class CanonicalConversationStore(
    private val history: PortableConversationStore,
    private val metadataFile: File,
    legacyFile: File? = null,
    private val idFactory: () -> String = { UUID.randomUUID().toString() },
) {
    @Volatile
    private var metadataLoadFailure: String? = null

    @Volatile
    private var metadata: ConversationUiMetadataState = loadMetadata()

    init {
        if (metadataLoadFailure == null) {
            migrateLegacyIfNeeded(legacyFile)
        }
        recoverInterruptedTurns()
        if (metadataLoadFailure == null) {
            pruneMetadata()
        }
    }

    @Synchronized
    fun state(): ConversationState = snapshot()

    @Synchronized
    fun create(projectId: String? = null): ConversationRecord {
        ensureMetadataHealthy()
        val cleanProjectId = normalizeOptionalId(projectId, MAX_PROJECT_ID_CHARS, "Project id")
        check(history.listConversations(limit = MAX_CONVERSATIONS).size < MAX_CONVERSATIONS) {
            "Conversation limit reached"
        }
        val id = normalizeId(idFactory())
        require(history.getConversation(id) == null) { "Conversation id already exists" }
        history.createConversation(DEFAULT_TITLE, conversationId = id)
        metadata = metadata.copy(
            selectedConversationId = id,
            conversations = metadata.conversations +
                (id to ConversationUiMetadata(projectId = cleanProjectId)),
        )
        persistMetadata()
        return checkNotNull(snapshot().conversation(id))
    }

    @Synchronized
    fun select(conversationId: String): ConversationState {
        ensureMetadataHealthy()
        val id = normalizeId(conversationId)
        require(history.getConversation(id) != null) { "Unknown conversation: $id" }
        metadata = metadata.copy(selectedConversationId = id)
        persistMetadata()
        return snapshot()
    }

    @Synchronized
    fun rename(conversationId: String, rawTitle: String): ConversationState {
        val id = requireConversation(conversationId)
        val title = rawTitle.trim()
        require(title.isNotEmpty()) { "Conversation title is required" }
        require(title.length <= 120) { "Conversation title is too long" }
        history.renameConversation(id, title)
        return snapshot()
    }

    @Synchronized
    fun setPinned(conversationId: String, pinned: Boolean): ConversationState {
        val id = requireConversation(conversationId)
        updateMetadata(id) { it.copy(pinned = pinned) }
        return snapshot()
    }

    @Synchronized
    fun moveToProject(conversationId: String, projectId: String?): ConversationState {
        val id = requireConversation(conversationId)
        val cleanProjectId = normalizeOptionalId(projectId, MAX_PROJECT_ID_CHARS, "Project id")
        history.fencePendingSymbolicProject(id, cleanProjectId)
        updateMetadata(id) { it.copy(projectId = cleanProjectId) }
        return snapshot()
    }

    @Synchronized
    fun beginTurn(conversationId: String, rawUserText: String): ConversationState {
        val id = requireConversation(conversationId)
        val userText = normalizeText(rawUserText, "User text")
        val messages = history.loadMessages(id)
        check(messages.none { it.role == HistoryMessageRole.Assistant && it.status.isRunning() }) {
            "Conversation already has a running turn"
        }
        val turnId = UUID.randomUUID().toString()
        val now = PortableConversationStore.nowIso()
        var sequence = history.nextSequence(id)
        history.saveMessage(
            HistoryMessage(
                id = UUID.randomUUID().toString(),
                conversationId = id,
                sequence = sequence++,
                role = HistoryMessageRole.User,
                content = userText,
                status = HistoryMessageStatus.Complete,
                createdAt = now,
                updatedAt = now,
                turnId = turnId,
            )
        )
        history.saveMessage(
            HistoryMessage(
                id = UUID.randomUUID().toString(),
                conversationId = id,
                sequence = sequence,
                role = HistoryMessageRole.Assistant,
                content = "",
                status = HistoryMessageStatus.Pending,
                createdAt = now,
                updatedAt = now,
                turnId = turnId,
            )
        )
        val conversation = requireNotNull(history.getConversation(id))
        if (messages.isEmpty() && conversation.title == DEFAULT_TITLE) {
            history.renameConversation(id, deriveTitle(userText))
        }
        return snapshot()
    }

    @Synchronized
    fun runningTurnId(conversationId: String): String? {
        val id = requireConversation(conversationId)
        return history.loadMessages(id).lastOrNull {
            it.role == HistoryMessageRole.Assistant && it.status.isRunning()
        }?.turnId
    }

    @Synchronized
    fun completeTurn(
        conversationId: String,
        assistantText: String,
        success: Boolean,
        expectedTurnId: String? = null,
        remoteConversationId: String? = null,
    ): ConversationState {
        val id = requireConversation(conversationId)
        val response = normalizeText(assistantText, "Assistant text", allowBlank = true)
        val expectedTurn = expectedTurnId?.let(::normalizeId)
        val messages = history.loadMessages(id)
        val pending = if (expectedTurn == null) {
            messages.lastOrNull {
                it.role == HistoryMessageRole.Assistant && it.status.isRunning()
            }
        } else {
            messages.lastOrNull {
                it.role == HistoryMessageRole.Assistant &&
                    it.turnId == expectedTurnId &&
                    it.status.isRunning()
            }
        }
        if (pending != null) {
            history.saveMessage(
                pending.copy(
                    content = response,
                    status = if (success) HistoryMessageStatus.Complete else HistoryMessageStatus.Error,
                    error = if (success) "" else response,
                )
            )
        } else {
            val terminal = if (expectedTurn == null) {
                messages.lastOrNull { it.role == HistoryMessageRole.Assistant }
            } else {
                messages.lastOrNull {
                    it.role == HistoryMessageRole.Assistant && it.turnId == expectedTurnId
                }
            } ?: error("Conversation has no assistant turn matching the expected turn id")
            val expectedStatus = if (success) {
                HistoryMessageStatus.Complete
            } else {
                HistoryMessageStatus.Error
            }
            check(terminal.status == expectedStatus && terminal.content == response) {
                "Conversation has no matching running or terminal turn"
            }
        }
        val cleanRemoteId = normalizeOptionalId(remoteConversationId, MAX_ID_CHARS, "Remote conversation id")
        if (cleanRemoteId != null) {
            updateMetadata(id) { it.copy(remoteConversationId = cleanRemoteId) }
        }
        return snapshot()
    }

    @Synchronized
    fun failTurn(
        conversationId: String,
        message: String,
        expectedTurnId: String? = null,
    ): ConversationState = completeTurn(
        conversationId = conversationId,
        assistantText = message,
        success = false,
        expectedTurnId = expectedTurnId,
    )

    private fun recoverInterruptedTurns() {
        history.listConversations(limit = MAX_CONVERSATIONS).forEach { conversation ->
            history.loadState(conversation.id)
        }
    }

    private fun snapshot(): ConversationState {
        if (metadataLoadFailure != null) {
            return ConversationState(loadFailure = metadataLoadFailure)
        }
        val rows = history.listConversations(limit = MAX_CONVERSATIONS)
        val records = rows.map { conversation ->
            val ui = metadata.conversations[conversation.id] ?: ConversationUiMetadata()
            val messages = history.loadMessages(conversation.id)
            ConversationRecord(
                id = conversation.id,
                title = conversation.title,
                pinned = ui.pinned,
                projectId = ui.projectId,
                remoteConversationId = ui.remoteConversationId,
                status = statusOf(messages),
                turns = turnsOf(messages),
                createdAtEpochMs = isoToEpochMs(conversation.createdAt),
                updatedAtEpochMs = isoToEpochMs(conversation.updatedAt),
            )
        }
        val selected = metadata.selectedConversationId?.takeIf { selectedId ->
            records.any { it.id == selectedId }
        }
        return ConversationState(
            conversations = records,
            selectedConversationId = selected,
        )
    }

    private fun turnsOf(messages: List<HistoryMessage>): List<ConversationTurn> {
        val turns = mutableListOf<ConversationTurn>()
        messages.forEach { message ->
            when (message.role) {
                HistoryMessageRole.User -> turns += ConversationTurn(userText = message.content)
                HistoryMessageRole.Assistant -> {
                    val index = turns.indexOfLast { it.assistantText == null }
                    if (index >= 0) {
                        val visibleText = when {
                            message.status == HistoryMessageStatus.Cancelled && message.content.isBlank() ->
                                message.error.ifBlank { INTERRUPTED_MESSAGE }
                            message.status.isRunning() -> null
                            else -> message.content
                        }
                        val success = when (message.status) {
                            HistoryMessageStatus.Complete -> true
                            HistoryMessageStatus.Error,
                            HistoryMessageStatus.Cancelled,
                            -> false
                            HistoryMessageStatus.Pending,
                            HistoryMessageStatus.Streaming,
                            -> null
                        }
                        turns[index] = turns[index].copy(
                            assistantText = visibleText,
                            success = success,
                        )
                    }
                }
                HistoryMessageRole.System,
                HistoryMessageRole.Tool,
                -> Unit
            }
        }
        return turns
    }

    private fun statusOf(messages: List<HistoryMessage>): ConversationStatus {
        if (messages.isEmpty()) return ConversationStatus.Empty
        val assistant = messages.lastOrNull { it.role == HistoryMessageRole.Assistant }
            ?: return ConversationStatus.Running
        return when (assistant.status) {
            HistoryMessageStatus.Pending,
            HistoryMessageStatus.Streaming,
            -> ConversationStatus.Running
            HistoryMessageStatus.Complete -> ConversationStatus.Success
            HistoryMessageStatus.Error -> ConversationStatus.Failed
            HistoryMessageStatus.Cancelled -> ConversationStatus.Interrupted
        }
    }

    private fun migrateLegacyIfNeeded(legacyFile: File?) {
        if (legacyFile == null || !legacyFile.isFile) return
        val legacyState = ConversationStore(legacyFile).state()
        legacyState.conversations.forEach { legacy ->
            if (history.getConversation(legacy.id) == null) {
                history.createConversation(legacy.title, conversationId = legacy.id)
                importLegacyTurns(legacy)
            }
            metadata = metadata.copy(
                conversations = metadata.conversations + (
                    legacy.id to ConversationUiMetadata(
                        pinned = legacy.pinned,
                        projectId = legacy.projectId,
                        remoteConversationId = legacy.remoteConversationId,
                    )
                )
            )
        }
        metadata = metadata.copy(
            selectedConversationId = legacyState.selectedConversationId ?: metadata.selectedConversationId,
        )
        persistMetadata()
        retireLegacyFile(legacyFile)
    }

    private fun importLegacyTurns(conversation: ConversationRecord) {
        var sequence = history.nextSequence(conversation.id)
        conversation.turns.forEachIndexed { index, turn ->
            val turnId = "legacy-${index + 1}"
            val now = PortableConversationStore.nowIso()
            history.saveMessage(
                HistoryMessage(
                    id = UUID.randomUUID().toString(),
                    conversationId = conversation.id,
                    sequence = sequence++,
                    role = HistoryMessageRole.User,
                    content = turn.userText,
                    status = HistoryMessageStatus.Complete,
                    createdAt = now,
                    updatedAt = now,
                    turnId = turnId,
                )
            )
            val assistantText = turn.assistantText
            if (assistantText != null) {
                val interrupted = conversation.status == ConversationStatus.Interrupted &&
                    index == conversation.turns.lastIndex
                history.saveMessage(
                    HistoryMessage(
                        id = UUID.randomUUID().toString(),
                        conversationId = conversation.id,
                        sequence = sequence++,
                        role = HistoryMessageRole.Assistant,
                        content = assistantText,
                        status = when {
                            interrupted -> HistoryMessageStatus.Cancelled
                            turn.success == true -> HistoryMessageStatus.Complete
                            else -> HistoryMessageStatus.Error
                        },
                        error = if (turn.success == true) "" else assistantText,
                        createdAt = now,
                        updatedAt = now,
                        turnId = turnId,
                    )
                )
            }
        }
    }

    private fun retireLegacyFile(file: File) {
        val backup = File(file.parentFile, "${file.name}.migrated")
        if (backup.exists()) return
        try {
            Files.move(file.toPath(), backup.toPath(), StandardCopyOption.ATOMIC_MOVE)
        } catch (_: AtomicMoveNotSupportedException) {
            Files.move(file.toPath(), backup.toPath())
        }
    }

    private fun pruneMetadata() {
        val ids = history.listConversations(limit = MAX_CONVERSATIONS).mapTo(mutableSetOf()) { it.id }
        val pruned = metadata.conversations.filterKeys(ids::contains)
        val selected = metadata.selectedConversationId?.takeIf(ids::contains)
        if (pruned != metadata.conversations || selected != metadata.selectedConversationId) {
            metadata = metadata.copy(selectedConversationId = selected, conversations = pruned)
            persistMetadata()
        }
    }

    private fun requireConversation(rawConversationId: String): String {
        ensureMetadataHealthy()
        val id = normalizeId(rawConversationId)
        require(history.getConversation(id) != null) { "Unknown conversation: $id" }
        return id
    }

    private fun updateMetadata(
        conversationId: String,
        transform: (ConversationUiMetadata) -> ConversationUiMetadata,
    ) {
        val current = metadata.conversations[conversationId] ?: ConversationUiMetadata()
        metadata = metadata.copy(
            conversations = metadata.conversations + (conversationId to transform(current)),
        )
        persistMetadata()
    }

    private fun loadMetadata(): ConversationUiMetadataState {
        if (!metadataFile.exists()) return ConversationUiMetadataState()
        if (!metadataFile.isFile || metadataFile.length() !in 1..MAX_UI_METADATA_BYTES.toLong()) {
            metadataLoadFailure = UI_METADATA_LOAD_FAILURE
            return ConversationUiMetadataState()
        }
        return try {
            DataInputStream(FileInputStream(metadataFile).buffered()).use { input ->
                require(input.readUTF() == UI_METADATA_MAGIC)
                val selected = input.readBoundedString(MAX_ID_CHARS).ifEmpty { null }
                val count = input.readInt()
                require(count in 0..MAX_CONVERSATIONS)
                val rows = buildMap {
                    repeat(count) {
                        val id = normalizeId(input.readBoundedString(MAX_ID_CHARS))
                        val pinned = input.readBoolean()
                        val projectId = input.readBoundedString(MAX_PROJECT_ID_CHARS).ifEmpty { null }
                        val remoteId = input.readBoundedString(MAX_ID_CHARS).ifEmpty { null }
                        put(id, ConversationUiMetadata(pinned, projectId, remoteId))
                    }
                }
                require(input.read() == -1)
                ConversationUiMetadataState(selected, rows)
            }
        } catch (_: Exception) {
            metadataLoadFailure = UI_METADATA_LOAD_FAILURE
            ConversationUiMetadataState()
        }
    }

    private fun persistMetadata() {
        ensureMetadataHealthy()
        val directory = metadataFile.absoluteFile.parentFile
            ?: error("Conversation UI metadata path has no parent")
        check(directory.exists() || directory.mkdirs()) {
            "Conversation UI metadata directory could not be created"
        }
        val temp = Files.createTempFile(directory.toPath(), ".${metadataFile.name}.", ".tmp").toFile()
        try {
            DataOutputStream(FileOutputStream(temp).buffered()).use { output ->
                output.writeUTF(UI_METADATA_MAGIC)
                output.writeBoundedString(metadata.selectedConversationId.orEmpty(), MAX_ID_CHARS)
                output.writeInt(metadata.conversations.size)
                metadata.conversations.toSortedMap().forEach { (id, row) ->
                    output.writeBoundedString(id, MAX_ID_CHARS)
                    output.writeBoolean(row.pinned)
                    output.writeBoundedString(row.projectId.orEmpty(), MAX_PROJECT_ID_CHARS)
                    output.writeBoundedString(row.remoteConversationId.orEmpty(), MAX_ID_CHARS)
                }
            }
            check(temp.length() in 1..MAX_UI_METADATA_BYTES.toLong()) {
                "Conversation UI metadata exceeds bounded size"
            }
            try {
                Files.move(
                    temp.toPath(),
                    metadataFile.toPath(),
                    StandardCopyOption.REPLACE_EXISTING,
                    StandardCopyOption.ATOMIC_MOVE,
                )
            } catch (_: AtomicMoveNotSupportedException) {
                Files.move(temp.toPath(), metadataFile.toPath(), StandardCopyOption.REPLACE_EXISTING)
            }
        } finally {
            if (temp.exists()) temp.delete()
        }
    }

    private fun ensureMetadataHealthy() {
        metadataLoadFailure?.let { error(it) }
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
        require(size in 0..maxChars * 4)
        val bytes = ByteArray(size)
        readFully(bytes)
        val value = String(bytes, StandardCharsets.UTF_8)
        require(value.length <= maxChars)
        return value
    }

    private fun normalizeId(raw: String): String {
        val value = raw.trim()
        require(value.isNotEmpty()) { "Conversation id is required" }
        require(value.length <= MAX_ID_CHARS) { "Conversation id is too long" }
        require(value.none(Char::isISOControl)) { "Conversation id contains control characters" }
        return value
    }

    private fun normalizeOptionalId(raw: String?, maxChars: Int, name: String): String? {
        if (raw == null) return null
        val value = raw.trim()
        if (value.isEmpty()) return null
        require(value.length <= maxChars) { "$name is too long" }
        require(value.none(Char::isISOControl)) { "$name contains control characters" }
        return value
    }

    private fun normalizeText(raw: String, name: String, allowBlank: Boolean = false): String {
        val value = raw.trim()
        if (!allowBlank) require(value.isNotEmpty()) { "$name is required" }
        require(value.length <= 64 * 1024) { "$name is too long" }
        return value
    }

    private fun deriveTitle(text: String): String {
        val singleLine = text.trim().split(Regex("\\s+")).filter(String::isNotEmpty).joinToString(" ")
        return if (singleLine.length <= 60) singleLine else singleLine.take(57).trimEnd() + "…"
    }

    private fun isoToEpochMs(value: String): Long = runCatching {
        LocalDateTime.parse(value, DateTimeFormatter.ISO_LOCAL_DATE_TIME)
            .toInstant(ZoneOffset.UTC)
            .toEpochMilli()
    }.getOrDefault(0L)

    private fun HistoryMessageStatus.isRunning(): Boolean =
        this == HistoryMessageStatus.Pending || this == HistoryMessageStatus.Streaming
}
