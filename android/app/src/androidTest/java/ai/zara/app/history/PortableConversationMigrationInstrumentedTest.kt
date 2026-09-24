package ai.zara.app.history

import android.content.Context
import android.database.sqlite.SQLiteDatabase
import androidx.test.platform.app.InstrumentationRegistry
import org.junit.After
import org.junit.Assert.assertEquals
import org.junit.Assert.assertFalse
import org.junit.Assert.assertTrue
import org.junit.Before
import org.junit.Test

class PortableConversationMigrationInstrumentedTest {
    private lateinit var context: Context

    @Before
    fun setUp() {
        context = InstrumentationRegistry.getInstrumentation().targetContext
        context.deleteDatabase(ConversationHistoryContract.databaseName)
    }

    @After
    fun tearDown() {
        context.deleteDatabase(ConversationHistoryContract.databaseName)
    }

    @Test
    fun version2HistoryUpgradesInPlaceAndSurvivesHelperRecreation() {
        seedVersion2Database()

        val first = PortableConversationStore(context)
        try {
            assertEquals(4, first.readableDatabase.version)
            assertEquals("Legacy symbolic chat", first.getConversation(CONVERSATION_ID)?.title)
            assertEquals(listOf("remember this"), first.loadMessages(CONVERSATION_ID).map { it.content })
            assertTrue(indexExists(first.readableDatabase, "idx_desktop_symbolic_project"))
            assertTrue(indexExists(first.readableDatabase, "idx_desktop_symbolic_turn"))
            assertTrue(columnExists(first.readableDatabase, "desktop_symbolic_projections", "dialogue_act"))
            assertTrue(columnExists(first.readableDatabase, "desktop_symbolic_projections", "verified_outcome_refs"))
            assertTrue(columnExists(first.readableDatabase, "desktop_symbolic_projections", "providers_enabled"))
            assertTrue(columnExists(first.readableDatabase, "desktop_symbolic_projections", "max_model_calls"))

            val saved = first.saveSymbolicProjection(
                SymbolicConversationProjection(
                    conversationId = CONVERSATION_ID,
                    projectionGeneration = 1,
                    runtimeGeneration = 7,
                    turnId = "turn-v2",
                    outcome = "success",
                    projectId = "project-v2",
                    projectGeneration = 1,
                    dialogueAct = "verified",
                    dialogueStateJson = "{\"act\":\"resume\"}",
                    verifiedOutcomeRefs = listOf(VERIFIED_EFFECT_REF),
                    rendererProvenance = "symbolic-dcg/v1",
                    providersEnabled = false,
                    maxModelCalls = 0,
                    providerCalls = 0,
                    modelCalls = 0,
                ),
                expectedGeneration = 0,
            )
            saved.assertPureSymbolic()
            assertFalse(saved.providersEnabled)
            assertEquals(0L, saved.maxModelCalls)
            assertEquals("verified", saved.dialogueAct)
            assertEquals(listOf(VERIFIED_EFFECT_REF), saved.verifiedOutcomeRefs)
            assertEquals("symbolic-dcg/v1", saved.rendererProvenance)
        } finally {
            first.close()
        }

        val reopened = PortableConversationStore(context)
        try {
            assertEquals(4, reopened.readableDatabase.version)
            assertEquals("Legacy symbolic chat", reopened.getConversation(CONVERSATION_ID)?.title)
            assertEquals("remember this", reopened.loadMessages(CONVERSATION_ID).single().content)
            val projection = checkNotNull(reopened.loadSymbolicProjection(CONVERSATION_ID))
            projection.assertPureSymbolic()
            assertEquals("turn-v2", projection.turnId)
            assertEquals("success", projection.outcome)
            assertEquals("verified", projection.dialogueAct)
            assertEquals(listOf(VERIFIED_EFFECT_REF), projection.verifiedOutcomeRefs)
            assertEquals("symbolic-dcg/v1", projection.rendererProvenance)
            assertFalse(projection.providersEnabled)
            assertEquals(0L, projection.maxModelCalls)
            assertEquals(0L, projection.providerCalls)
            assertEquals(0L, projection.modelCalls)
        } finally {
            reopened.close()
        }
    }

    private fun seedVersion2Database() {
        val path = context.getDatabasePath(ConversationHistoryContract.databaseName)
        val parent = requireNotNull(path.parentFile)
        check(parent.isDirectory || parent.mkdirs()) { "failed to create database directory" }
        val db = SQLiteDatabase.openOrCreateDatabase(path, null)
        try {
            db.execSQL(
                """
                CREATE TABLE desktop_conversations (
                    id TEXT PRIMARY KEY,
                    title TEXT NOT NULL,
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL,
                    provider TEXT NOT NULL DEFAULT '',
                    model TEXT NOT NULL DEFAULT '',
                    principal_id TEXT NOT NULL DEFAULT 'local:owner'
                )
                """.trimIndent(),
            )
            db.execSQL(
                """
                CREATE TABLE desktop_messages (
                    id TEXT PRIMARY KEY,
                    conversation_id TEXT NOT NULL,
                    sequence INTEGER NOT NULL,
                    turn_id TEXT,
                    role TEXT NOT NULL,
                    content TEXT NOT NULL,
                    status TEXT NOT NULL,
                    error TEXT NOT NULL DEFAULT '',
                    tool_run_id TEXT,
                    created_at TEXT NOT NULL,
                    updated_at TEXT NOT NULL,
                    principal_id TEXT NOT NULL DEFAULT 'local:owner',
                    FOREIGN KEY(conversation_id)
                        REFERENCES desktop_conversations(id) ON DELETE CASCADE,
                    UNIQUE(conversation_id, sequence)
                )
                """.trimIndent(),
            )
            db.execSQL(
                """
                INSERT INTO desktop_conversations (
                    id, title, created_at, updated_at, provider, model, principal_id
                ) VALUES (?, ?, ?, ?, '', '', ?)
                """.trimIndent(),
                arrayOf(CONVERSATION_ID, "Legacy symbolic chat", STAMP, STAMP, "local:owner"),
            )
            db.execSQL(
                """
                INSERT INTO desktop_messages (
                    id, conversation_id, sequence, turn_id, role, content, status,
                    error, tool_run_id, created_at, updated_at, principal_id
                ) VALUES (?, ?, 1, ?, 'user', ?, 'complete', '', NULL, ?, ?, ?)
                """.trimIndent(),
                arrayOf("legacy-message", CONVERSATION_ID, "turn-legacy", "remember this", STAMP, STAMP, "local:owner"),
            )
            db.version = 2
        } finally {
            db.close()
        }
    }

    private fun indexExists(db: SQLiteDatabase, name: String): Boolean =
        db.rawQuery(
            "SELECT 1 FROM sqlite_master WHERE type = 'index' AND name = ?",
            arrayOf(name),
        ).use { it.moveToFirst() }

    private fun columnExists(db: SQLiteDatabase, table: String, name: String): Boolean =
        db.rawQuery("PRAGMA table_info($table)", null).use { cursor ->
            val nameIndex = cursor.getColumnIndexOrThrow("name")
            while (cursor.moveToNext()) {
                if (cursor.getString(nameIndex) == name) return@use true
            }
            false
        }

    private companion object {
        const val CONVERSATION_ID = "legacy-v2-conversation"
        const val STAMP = "2026-09-20T00:00:00.000000"
        const val VERIFIED_EFFECT_REF = "zara.verified-outcome/v2:7:effect:migrated-v2-turn"
    }
}
