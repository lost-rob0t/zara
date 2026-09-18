-- Zara portable conversation history schema v2.
--
-- Compatibility ABI: the desktop_* table names are intentionally retained so
-- existing Zara Desktop databases can be opened directly by Android and vice
-- versa. Do not rename these tables without a migration on both platforms.

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
