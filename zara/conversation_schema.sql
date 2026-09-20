-- Zara portable conversation history schema v4.
--
-- Compatibility ABI: the desktop_* table names are intentionally retained so
-- existing Zara Desktop databases can be opened directly by Android and vice
-- versa. Do not rename these tables without a migration on both platforms.
--
-- The symbolic projection table is additive to the same conversation-history
-- database. It is not a second chat/history authority: every row is owned by a
-- canonical desktop_conversations row and may be rebuilt from canonical events
-- as later slices add replay support.

CREATE TABLE IF NOT EXISTS desktop_conversations (
    id TEXT PRIMARY KEY,
    title TEXT NOT NULL,
    created_at TEXT NOT NULL,
    updated_at TEXT NOT NULL,
    provider TEXT NOT NULL DEFAULT '',
    model TEXT NOT NULL DEFAULT '',
    principal_id TEXT NOT NULL DEFAULT 'local:owner'
);

CREATE TABLE IF NOT EXISTS desktop_messages (
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
);

CREATE TABLE IF NOT EXISTS desktop_symbolic_projections (
    conversation_id TEXT NOT NULL,
    principal_id TEXT NOT NULL DEFAULT 'local:owner',
    turn_id TEXT,
    outcome TEXT NOT NULL DEFAULT 'unknown'
        CHECK (outcome IN ('unknown', 'pending', 'success', 'cancelled', 'interrupted', 'error')),
    projection_generation INTEGER NOT NULL DEFAULT 0
        CHECK (typeof(projection_generation) = 'integer' AND projection_generation >= 0),
    runtime_generation INTEGER NOT NULL DEFAULT 0
        CHECK (typeof(runtime_generation) = 'integer' AND runtime_generation >= 0),
    project_id TEXT,
    project_generation INTEGER NOT NULL DEFAULT 0
        CHECK (typeof(project_generation) = 'integer' AND project_generation >= 0),
    dialogue_act TEXT NOT NULL DEFAULT 'unknown',
    dialogue_state_json TEXT NOT NULL DEFAULT '{}',
    discourse_entities_json TEXT NOT NULL DEFAULT '[]',
    unresolved_questions_json TEXT NOT NULL DEFAULT '[]',
    expert_evidence_json TEXT NOT NULL DEFAULT '[]',
    verified_facts_json TEXT NOT NULL DEFAULT '[]',
    verified_outcome_refs TEXT NOT NULL DEFAULT '',
    renderer_provenance TEXT NOT NULL DEFAULT '',
    providers_enabled INTEGER NOT NULL DEFAULT 1
        CHECK (typeof(providers_enabled) = 'integer' AND providers_enabled IN (0, 1)),
    max_model_calls INTEGER NOT NULL DEFAULT 1
        CHECK (typeof(max_model_calls) = 'integer' AND max_model_calls >= 0),
    provider_calls INTEGER NOT NULL DEFAULT 0
        CHECK (typeof(provider_calls) = 'integer' AND provider_calls >= 0),
    model_calls INTEGER NOT NULL DEFAULT 0
        CHECK (typeof(model_calls) = 'integer' AND model_calls >= 0),
    updated_at TEXT NOT NULL,
    PRIMARY KEY(conversation_id, principal_id),
    FOREIGN KEY(conversation_id)
        REFERENCES desktop_conversations(id) ON DELETE CASCADE
);

CREATE INDEX IF NOT EXISTS idx_desktop_conversations_updated
    ON desktop_conversations(updated_at DESC);
CREATE INDEX IF NOT EXISTS idx_desktop_messages_conversation
    ON desktop_messages(conversation_id, sequence);
CREATE INDEX IF NOT EXISTS idx_desktop_messages_turn
    ON desktop_messages(conversation_id, turn_id);
CREATE INDEX IF NOT EXISTS idx_desktop_messages_tool_run
    ON desktop_messages(conversation_id, tool_run_id);
CREATE INDEX IF NOT EXISTS idx_desktop_conversations_principal_updated
    ON desktop_conversations(principal_id, updated_at DESC);
CREATE INDEX IF NOT EXISTS idx_desktop_messages_principal_conversation
    ON desktop_messages(principal_id, conversation_id, sequence);
CREATE INDEX IF NOT EXISTS idx_desktop_symbolic_project
    ON desktop_symbolic_projections(principal_id, project_id, project_generation);
CREATE INDEX IF NOT EXISTS idx_desktop_symbolic_turn
    ON desktop_symbolic_projections(principal_id, turn_id);
