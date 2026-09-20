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

class PortableConversationV3MigrationInstrumentedTest {
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
    fun version3ProjectionUpgradesFailClosedAndSurvivesHelperRecreation() {
        seedVersion3Database()

        val first = PortableConversationStore(context)
        try {
            assertEquals(4, first.readableDatabase.version)
            assertTrue(columnExists(first.readableDatabase, "desktop_symbolic_projections", "providers_enabled"))
            assertTrue(columnExists(first.readableDatabase, "desktop_symbolic_projections", "max_model_calls"))

            val migrated = checkNotNull(first.loadSymbolicProjection(CONVERSATION_ID))
            assertEquals(3L, migrated.projectionGeneration)
            assertEquals(7L, migrated.runtimeGeneration)
            assertEquals("turn-v3", migrated.turnId)
            assertEquals("project-v3", migrated.projectId)
            assertEquals(2L, migrated.projectGeneration)
            assertEquals("clarify", migrated.dialogueAct)
            assertEquals("{\"topic\":\"migration\"}", migrated.dialogueStateJson)
            assertEquals("symbolic-dcg/v1", migrated.rendererProvenance)
            assertTrue(migrated.providersEnabled)
            assertEquals(1L, migrated.maxModelCalls)
            assertEquals(0L, migrated.providerCalls)
            assertEquals(0L, migrated.modelCalls)
            assertTrue(runCatching { migrated.assertPureSymbolic() }.isFailure)

            val authoritative = first.saveSymbolicProjection(
                migrated.copy(
                    projectionGeneration = 4,
                    providersEnabled = false,
                    maxModelCalls = 0,
                ),
                expectedGeneration = 3,
            )
            authoritative.assertPureSymbolic()
        } finally {
            first.close()
        }

        val reopened = PortableConversationStore(context)
        try {
            val recovered = checkNotNull(reopened.loadSymbolicProjection(CONVERSATION_ID))
            recovered.assertPureSymbolic()
            assertEquals(4L, recovered.projectionGeneration)
            assertFalse(recovered.providersEnabled)
            assertEquals(0L, recovered.maxModelCalls)
            assertEquals(0L, recovered.providerCalls)
            assertEquals(0L, recovered.modelCalls)
            assertEquals("{\"topic\":\"migration\"}", recovered.dialogueStateJson)
        } finally {
            reopened.close()
        }
    }

    @Test
    fun persistedRealCounterCannotBecomeExactZeroAfterReopen() {
        val first = PortableConversationStore(context)
        first.createConversation("storage class", conversationId = CONVERSATION_ID)
        first.saveSymbolicProjection(
            SymbolicConversationProjection(
                conversationId = CONVERSATION_ID,
                projectionGeneration = 1,
                runtimeGeneration = 1,
                turnId = "turn-storage-class",
                outcome = "pending",
                projectId = "project-storage-class",
                projectGeneration = 1,
                dialogueAct = "clarify",
                dialogueStateJson = "{\"act\":\"clarify\"}",
                providersEnabled = false,
                maxModelCalls = 0,
                providerCalls = 0,
                modelCalls = 0,
            ),
            expectedGeneration = 0,
        ).assertPureSymbolic()

        first.writableDatabase.execSQL("PRAGMA ignore_check_constraints = ON")
        first.writableDatabase.execSQL(
            "UPDATE desktop_symbolic_projections SET max_model_calls = 0.5 WHERE conversation_id = ?",
            arrayOf(CONVERSATION_ID),
        )
        val storageClass = first.readableDatabase.rawQuery(
            "SELECT typeof(max_model_calls) FROM desktop_symbolic_projections WHERE conversation_id = ?",
            arrayOf(CONVERSATION_ID),
        ).use { cursor ->
            check(cursor.moveToFirst())
            cursor.getString(0)
        }
        assertEquals("real", storageClass)
        first.close()

        val reopened = PortableConversationStore(context)
        try {
            assertTrue(
                "REAL max_model_calls must fail closed instead of Cursor.getLong() -> 0",
                runCatching { reopened.loadSymbolicProjection(CONVERSATION_ID) }.isFailure,
            )
        } finally {
            reopened.close()
        }
    }

    @Test
    fun persistedTextCounterCannotBecomeExactZeroAfterReopen() {
        val first = PortableConversationStore(context)
        first.createConversation("storage class text", conversationId = CONVERSATION_ID)
        first.saveSymbolicProjection(
            SymbolicConversationProjection(
                conversationId = CONVERSATION_ID,
                projectionGeneration = 1,
                runtimeGeneration = 1,
                turnId = "turn-storage-class-text",
                outcome = "pending",
                projectId = "project-storage-class",
                projectGeneration = 1,
                dialogueAct = "clarify",
                dialogueStateJson = "{\"act\":\"clarify\"}",
                providersEnabled = false,
                maxModelCalls = 0,
                providerCalls = 0,
                modelCalls = 0,
            ),
            expectedGeneration = 0,
        ).assertPureSymbolic()

        first.writableDatabase.execSQL("PRAGMA ignore_check_constraints = ON")
        first.writableDatabase.execSQL(
            "UPDATE desktop_symbolic_projections SET max_model_calls = 'not-an-integer' WHERE conversation_id = ?",
            arrayOf(CONVERSATION_ID),
        )
        val storageClass = first.readableDatabase.rawQuery(
            "SELECT typeof(max_model_calls) FROM desktop_symbolic_projections WHERE conversation_id = ?",
            arrayOf(CONVERSATION_ID),
        ).use { cursor ->
            check(cursor.moveToFirst())
            cursor.getString(0)
        }
        assertEquals("text", storageClass)
        first.close()

        val reopened = PortableConversationStore(context)
        try {
            assertTrue(
                "TEXT max_model_calls must fail closed instead of Cursor.getLong() -> 0",
                runCatching { reopened.loadSymbolicProjection(CONVERSATION_ID) }.isFailure,
            )
        } finally {
            reopened.close()
        }
    }

    private fun seedVersion3Database() {
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
                CREATE TABLE desktop_symbolic_projections (
                    conversation_id TEXT NOT NULL,
                    principal_id TEXT NOT NULL DEFAULT 'local:owner',
                    turn_id TEXT,
                    outcome TEXT NOT NULL DEFAULT 'unknown',
                    projection_generation INTEGER NOT NULL DEFAULT 0,
                    runtime_generation INTEGER NOT NULL DEFAULT 0,
                    project_id TEXT,
                    project_generation INTEGER NOT NULL DEFAULT 0,
                    dialogue_act TEXT NOT NULL DEFAULT 'unknown',
                    dialogue_state_json TEXT NOT NULL DEFAULT '{}',
                    discourse_entities_json TEXT NOT NULL DEFAULT '[]',
                    unresolved_questions_json TEXT NOT NULL DEFAULT '[]',
                    expert_evidence_json TEXT NOT NULL DEFAULT '[]',
                    verified_facts_json TEXT NOT NULL DEFAULT '[]',
                    verified_outcome_refs TEXT NOT NULL DEFAULT '',
                    renderer_provenance TEXT NOT NULL DEFAULT '',
                    provider_calls INTEGER NOT NULL DEFAULT 0,
                    model_calls INTEGER NOT NULL DEFAULT 0,
                    updated_at TEXT NOT NULL,
                    PRIMARY KEY(conversation_id, principal_id),
                    FOREIGN KEY(conversation_id)
                        REFERENCES desktop_conversations(id) ON DELETE CASCADE
                )
                """.trimIndent(),
            )
            db.execSQL(
                """
                INSERT INTO desktop_conversations (
                    id, title, created_at, updated_at, provider, model, principal_id
                ) VALUES (?, ?, ?, ?, '', '', 'local:owner')
                """.trimIndent(),
                arrayOf(CONVERSATION_ID, "Existing v3 symbolic conversation", STAMP, STAMP),
            )
            db.execSQL(
                """
                INSERT INTO desktop_symbolic_projections (
                    conversation_id, principal_id, turn_id, outcome,
                    projection_generation, runtime_generation, project_id,
                    project_generation, dialogue_act, dialogue_state_json,
                    discourse_entities_json, unresolved_questions_json,
                    expert_evidence_json, verified_facts_json,
                    verified_outcome_refs, renderer_provenance,
                    provider_calls, model_calls, updated_at
                ) VALUES (?, 'local:owner', 'turn-v3', 'pending', 3, 7, 'project-v3', 2,
                    'clarify', '{\"topic\":\"migration\"}', '[]',
                    '[{\"question\":\"which one?\"}]',
                    '[{\"expert\":\"dotfiles\"}]',
                    '[{\"fact\":\"history survives\"}]',
                    'zara.verified-outcome/v1:effect:v3-existing-effect',
                    'symbolic-dcg/v1', 0, 0, ?)
                """.trimIndent(),
                arrayOf(CONVERSATION_ID, STAMP),
            )
            db.version = 3
        } finally {
            db.close()
        }
    }

    private fun columnExists(db: SQLiteDatabase, table: String, name: String): Boolean =
        db.rawQuery("PRAGMA table_info($table)", null).use { cursor ->
            val nameIndex = cursor.getColumnIndexOrThrow("name")
            while (cursor.moveToNext()) {
                if (cursor.getString(nameIndex) == name) return@use true
            }
            false
        }

    private companion object {
        const val CONVERSATION_ID = "legacy-v3-conversation"
        const val STAMP = "2026-09-20T12:00:00.000000"
    }
}
