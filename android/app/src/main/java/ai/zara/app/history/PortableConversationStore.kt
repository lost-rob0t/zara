package ai.zara.app.history

import android.content.ContentValues
import android.content.Context
import android.database.Cursor
import android.database.sqlite.SQLiteDatabase
import android.database.sqlite.SQLiteOpenHelper
import java.time.Instant
import java.time.LocalDateTime
import java.time.ZoneOffset
import java.time.format.DateTimeFormatter
import java.util.Locale
import java.util.UUID

enum class HistoryMessageRole(val wireName: String) {
    User("user"),
    Assistant("assistant"),
    System("system"),
    Tool("tool");

    companion object {
        fun fromWire(value: String): HistoryMessageRole = entries.first { it.wireName == value }
    }
}

enum class HistoryMessageStatus(val wireName: String) {
    Pending("pending"),
    Streaming("streaming"),
    Complete("complete"),
    Error("error"),
    Cancelled("cancelled");

    companion object {
        fun fromWire(value: String): HistoryMessageStatus = entries.first { it.wireName == value }
    }
}

data class HistoryConversation(
    val id: String,
    val title: String,
    val createdAt: String,
    val updatedAt: String,
    val provider: String = "",
    val model: String = "",
)

data class HistoryMessage(
    val id: String,
    val conversationId: String,
    val sequence: Int,
    val role: HistoryMessageRole,
    val content: String,
    val status: HistoryMessageStatus,
    val createdAt: String,
    val updatedAt: String,
    val turnId: String? = null,
    val error: String = "",
    val toolRunId: String? = null,
)

data class HistoryConversationState(
    val conversation: HistoryConversation,
    val messages: List<HistoryMessage>,
)

internal object ConversationHistoryContract {
    const val schemaVersion = 4
    const val localPrincipalId = "local:owner"
    const val legacyLocalPrincipalId = "__zara_legacy_local_owner__"
    const val schemaAsset = "database/conversation_schema.sql"
    const val databaseName = "zara.db"
    const val interruptedError = "Interrupted when Zara stopped."

    fun deriveTitle(text: String): String {
        val singleLine = text.trim().split(Regex("\\s+")).filter(String::isNotEmpty).joinToString(" ")
        if (singleLine.codePointCount(0, singleLine.length) <= 60) return singleLine
        val end = singleLine.offsetByCodePoints(0, 57)
        return singleLine.substring(0, end).trimEnd() + "…"
    }

    fun canPersistTransition(
        current: HistoryMessageStatus,
        requested: HistoryMessageStatus,
    ): Boolean = when (current) {
        HistoryMessageStatus.Pending,
        HistoryMessageStatus.Streaming,
        -> true
        HistoryMessageStatus.Complete,
        HistoryMessageStatus.Error,
        HistoryMessageStatus.Cancelled,
        -> false
    }
}

class PortableConversationStore(context: Context) : SQLiteOpenHelper(
    context.applicationContext,
    ConversationHistoryContract.databaseName,
    null,
    ConversationHistoryContract.schemaVersion,
) {
    private val appContext = context.applicationContext

    override fun onConfigure(db: SQLiteDatabase) {
        super.onConfigure(db)
        db.setForeignKeyConstraintsEnabled(true)
    }

    override fun onCreate(db: SQLiteDatabase) {
        installSchema(db)
    }

    override fun onUpgrade(db: SQLiteDatabase, oldVersion: Int, newVersion: Int) {
        require(newVersion <= ConversationHistoryContract.schemaVersion) {
            "Unsupported conversation database version $newVersion"
        }
        installSchema(db)
    }

    override fun onOpen(db: SQLiteDatabase) {
        super.onOpen(db)
        installSchema(db)
        migrateLegacyLocalRows(db)
    }

    @Synchronized
    fun createConversation(
        title: String = "New chat",
        conversationId: String = UUID.randomUUID().toString().replace("-", ""),
    ): HistoryConversation {
        val cleanTitle = title.trim().ifEmpty { "New chat" }
        val now = nowIso()
        val values = ContentValues().apply {
            put("id", conversationId)
            put("title", cleanTitle)
            put("created_at", now)
            put("updated_at", now)
            put("provider", "")
            put("model", "")
            put("principal_id", ConversationHistoryContract.localPrincipalId)
        }
        writableDatabase.insertOrThrow("desktop_conversations", null, values)
        return HistoryConversation(conversationId, cleanTitle, now, now)
    }

    @Synchronized
    fun getConversation(conversationId: String): HistoryConversation? =
        readableDatabase.query(
            "desktop_conversations",
            null,
            "id = ? AND principal_id = ?",
            arrayOf(conversationId, ConversationHistoryContract.localPrincipalId),
            null,
            null,
            null,
            "1",
        ).use { cursor ->
            if (!cursor.moveToFirst()) null else cursor.toConversation()
        }

    @Synchronized
    fun listConversations(query: String = "", limit: Int = 100): List<HistoryConversation> {
        require(limit >= 1) { "limit must be >= 1" }
        val cleanQuery = query.trim().lowercase(Locale.ROOT)
        val db = readableDatabase
        val cursor = if (cleanQuery.isEmpty()) {
            db.rawQuery(
                """
                SELECT * FROM desktop_conversations
                WHERE principal_id = ?
                ORDER BY updated_at DESC
                LIMIT ?
                """.trimIndent(),
                arrayOf(ConversationHistoryContract.localPrincipalId, limit.toString()),
            )
        } else {
            val pattern = "%$cleanQuery%"
            db.rawQuery(
                """
                SELECT DISTINCT c.*
                FROM desktop_conversations AS c
                LEFT JOIN desktop_messages AS m
                    ON m.conversation_id = c.id AND m.principal_id = c.principal_id
                WHERE c.principal_id = ?
                  AND (lower(c.title) LIKE ? OR lower(m.content) LIKE ?)
                ORDER BY c.updated_at DESC
                LIMIT ?
                """.trimIndent(),
                arrayOf(
                    ConversationHistoryContract.localPrincipalId,
                    pattern,
                    pattern,
                    limit.toString(),
                ),
            )
        }
        return cursor.use { rows -> buildList { while (rows.moveToNext()) add(rows.toConversation()) } }
    }

    @Synchronized
    fun renameConversation(conversationId: String, title: String): HistoryConversation {
        val cleanTitle = title.trim()
        require(cleanTitle.isNotEmpty()) { "conversation title must not be empty" }
        val existing = requireNotNull(getConversation(conversationId)) { "Unknown conversation $conversationId" }
        val now = nowIso()
        val values = ContentValues().apply {
            put("title", cleanTitle)
            put("updated_at", now)
        }
        val changed = writableDatabase.update(
            "desktop_conversations",
            values,
            "id = ? AND principal_id = ?",
            arrayOf(conversationId, ConversationHistoryContract.localPrincipalId),
        )
        check(changed == 1) { "Conversation disappeared while renaming" }
        return existing.copy(title = cleanTitle, updatedAt = now)
    }

    @Synchronized
    fun nextSequence(conversationId: String): Int {
        requireNotNull(getConversation(conversationId)) { "Unknown conversation $conversationId" }
        readableDatabase.rawQuery(
            """
            SELECT COALESCE(MAX(sequence), 0)
            FROM desktop_messages
            WHERE conversation_id = ? AND principal_id = ?
            """.trimIndent(),
            arrayOf(conversationId, ConversationHistoryContract.localPrincipalId),
        ).use { cursor ->
            check(cursor.moveToFirst())
            return cursor.getInt(0) + 1
        }
    }

    @Synchronized
    fun saveMessage(message: HistoryMessage) {
        requireNotNull(getConversation(message.conversationId)) {
            "Unknown conversation ${message.conversationId}"
        }
        val now = nowIso()
        val values = message.copy(updatedAt = now).toValues()
        val db = writableDatabase
        db.beginTransaction()
        try {
            val existingStatus = db.rawQuery(
                "SELECT status FROM desktop_messages WHERE id = ? AND principal_id = ? LIMIT 1",
                arrayOf(message.id, ConversationHistoryContract.localPrincipalId),
            ).use { cursor ->
                if (!cursor.moveToFirst()) null else HistoryMessageStatus.fromWire(cursor.getString(0))
            }
            if (existingStatus != null) {
                check(ConversationHistoryContract.canPersistTransition(existingStatus, message.status)) {
                    "Stale terminal message update rejected: ${existingStatus.wireName} -> ${message.status.wireName}"
                }
                val changed = db.update(
                    "desktop_messages",
                    values,
                    "id = ? AND conversation_id = ? AND principal_id = ?",
                    arrayOf(
                        message.id,
                        message.conversationId,
                        ConversationHistoryContract.localPrincipalId,
                    ),
                )
                check(changed == 1) { "Message owner or conversation changed" }
            } else {
                db.insertOrThrow("desktop_messages", null, values)
            }
            db.update(
                "desktop_conversations",
                ContentValues().apply { put("updated_at", now) },
                "id = ? AND principal_id = ?",
                arrayOf(message.conversationId, ConversationHistoryContract.localPrincipalId),
            )
            db.setTransactionSuccessful()
        } finally {
            db.endTransaction()
        }
    }

    @Synchronized
    fun loadMessages(conversationId: String): List<HistoryMessage> {
        if (getConversation(conversationId) == null) return emptyList()
        return readableDatabase.query(
            "desktop_messages",
            null,
            "conversation_id = ? AND principal_id = ?",
            arrayOf(conversationId, ConversationHistoryContract.localPrincipalId),
            null,
            null,
            "sequence ASC",
        ).use { cursor -> buildList { while (cursor.moveToNext()) add(cursor.toMessage()) } }
    }

    @Synchronized
    fun loadState(conversationId: String): HistoryConversationState {
        val conversation = requireNotNull(getConversation(conversationId)) {
            "Unknown conversation $conversationId"
        }
        val interruptedTurnIds = mutableSetOf<String>()
        val messages = loadMessages(conversationId).map { message ->
            if (message.status == HistoryMessageStatus.Pending || message.status == HistoryMessageStatus.Streaming) {
                message.turnId?.let(interruptedTurnIds::add)
                val recovered = message.copy(
                    status = HistoryMessageStatus.Cancelled,
                    error = message.error.ifEmpty { ConversationHistoryContract.interruptedError },
                    updatedAt = nowIso(),
                )
                saveMessage(recovered)
                recovered
            } else {
                message
            }
        }

        val projection = loadSymbolicProjection(conversationId)
        if (
            projection != null &&
            projection.turnId != null &&
            projection.turnId in interruptedTurnIds &&
            projection.outcome in setOf("unknown", "pending")
        ) {
            saveSymbolicProjection(
                projection.copy(
                    projectionGeneration = projection.projectionGeneration + 1L,
                    outcome = "interrupted",
                    updatedAt = "",
                ),
                expectedGeneration = projection.projectionGeneration,
            )
        }

        return HistoryConversationState(
            conversation = getConversation(conversationId) ?: conversation,
            messages = messages,
        )
    }

    private fun installSchema(db: SQLiteDatabase) {
        // Keep the conversation ABI independent of DatabaseManager's shared
        // schema_migrations table. Desktop has unrelated stores that
        // historically reused numeric migration versions; Android's
        // SQLiteOpenHelper owns user_version instead.
        repairPrincipalColumn(db, "desktop_conversations")
        repairPrincipalColumn(db, "desktop_messages")
        repairSymbolicPolicyColumns(db)
        schemaStatements().forEach(db::execSQL)
    }

    private fun schemaStatements(): List<String> {
        val script = appContext.assets.open(ConversationHistoryContract.schemaAsset)
            .bufferedReader()
            .use { it.readText() }
        val withoutComments = script.lineSequence()
            .filterNot { it.trimStart().startsWith("--") }
            .joinToString("\n")
        return withoutComments.split(';').map(String::trim).filter(String::isNotEmpty)
    }

    private fun repairPrincipalColumn(db: SQLiteDatabase, table: String) {
        if (!tableExists(db, table)) return
        val columns = tableColumns(db, table)
        if ("principal_id" !in columns) {
            db.execSQL(
                "ALTER TABLE $table ADD COLUMN principal_id TEXT NOT NULL " +
                    "DEFAULT '${ConversationHistoryContract.localPrincipalId}'"
            )
        }
    }

    private fun repairSymbolicPolicyColumns(db: SQLiteDatabase) {
        val table = "desktop_symbolic_projections"
        if (!tableExists(db, table)) return
        val columns = tableColumns(db, table)
        if ("providers_enabled" !in columns) {
            db.execSQL(
                "ALTER TABLE $table ADD COLUMN providers_enabled INTEGER NOT NULL DEFAULT 1 " +
                    "CHECK (typeof(providers_enabled) = 'integer' AND providers_enabled IN (0, 1))"
            )
        }
        if ("max_model_calls" !in columns) {
            db.execSQL(
                "ALTER TABLE $table ADD COLUMN max_model_calls INTEGER NOT NULL DEFAULT 1 " +
                    "CHECK (typeof(max_model_calls) = 'integer' AND max_model_calls >= 0)"
            )
        }
    }

    private fun tableColumns(db: SQLiteDatabase, table: String): Set<String> =
        db.rawQuery("PRAGMA table_info($table)", null).use { cursor ->
            buildSet {
                val nameIndex = cursor.getColumnIndexOrThrow("name")
                while (cursor.moveToNext()) add(cursor.getString(nameIndex))
            }
        }

    private fun migrateLegacyLocalRows(db: SQLiteDatabase) {
        val legacyLocalPredicate = """
            principal_id = ?
            OR (
                substr(principal_id, 1, 4) = 'uid:'
                AND length(substr(principal_id, 5)) > 0
                AND substr(principal_id, 5) NOT GLOB '*[^0-9]*'
            )
        """.trimIndent()
        listOf("desktop_conversations", "desktop_messages").forEach { table ->
            if (!tableExists(db, table)) return@forEach
            val values = ContentValues().apply {
                put("principal_id", ConversationHistoryContract.localPrincipalId)
            }
            db.update(
                table,
                values,
                legacyLocalPredicate,
                arrayOf(ConversationHistoryContract.legacyLocalPrincipalId),
            )
        }
    }

    private fun tableExists(db: SQLiteDatabase, table: String): Boolean =
        db.rawQuery(
            "SELECT 1 FROM sqlite_master WHERE type = 'table' AND name = ? LIMIT 1",
            arrayOf(table),
        ).use { it.moveToFirst() }

    private fun Cursor.toConversation(): HistoryConversation = HistoryConversation(
        id = getString(getColumnIndexOrThrow("id")),
        title = getString(getColumnIndexOrThrow("title")),
        createdAt = getString(getColumnIndexOrThrow("created_at")),
        updatedAt = getString(getColumnIndexOrThrow("updated_at")),
        provider = getString(getColumnIndexOrThrow("provider")),
        model = getString(getColumnIndexOrThrow("model")),
    )

    private fun Cursor.toMessage(): HistoryMessage = HistoryMessage(
        id = getString(getColumnIndexOrThrow("id")),
        conversationId = getString(getColumnIndexOrThrow("conversation_id")),
        sequence = getInt(getColumnIndexOrThrow("sequence")),
        turnId = nullableString("turn_id"),
        role = HistoryMessageRole.fromWire(getString(getColumnIndexOrThrow("role"))),
        content = getString(getColumnIndexOrThrow("content")),
        status = HistoryMessageStatus.fromWire(getString(getColumnIndexOrThrow("status"))),
        error = getString(getColumnIndexOrThrow("error")),
        toolRunId = nullableString("tool_run_id"),
        createdAt = getString(getColumnIndexOrThrow("created_at")),
        updatedAt = getString(getColumnIndexOrThrow("updated_at")),
    )

    private fun Cursor.nullableString(column: String): String? {
        val index = getColumnIndexOrThrow(column)
        return if (isNull(index)) null else getString(index)
    }

    private fun HistoryMessage.toValues(): ContentValues = ContentValues().apply {
        put("id", id)
        put("conversation_id", conversationId)
        put("sequence", sequence)
        if (turnId == null) putNull("turn_id") else put("turn_id", turnId)
        put("role", role.wireName)
        put("content", content)
        put("status", status.wireName)
        put("error", error)
        if (toolRunId == null) putNull("tool_run_id") else put("tool_run_id", toolRunId)
        put("created_at", createdAt)
        put("updated_at", updatedAt)
        put("principal_id", ConversationHistoryContract.localPrincipalId)
    }

    companion object {
        private val isoFormatter = DateTimeFormatter.ofPattern(
            "yyyy-MM-dd'T'HH:mm:ss.SSSSSS",
            Locale.ROOT,
        )

        fun nowIso(): String = LocalDateTime.ofInstant(Instant.now(), ZoneOffset.UTC).format(isoFormatter)
    }
}
