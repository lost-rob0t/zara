package ai.zara.app.history

/**
 * Roll back a just-created canonical conversation when an outer UI metadata commit fails.
 *
 * This stays inside the canonical history owner and refuses to delete a row once any durable
 * message or symbolic projection has attached to it. The foreign-key cascade is therefore only
 * used for the empty creation record that has not become an observable conversation turn.
 */
internal fun PortableConversationStore.rollbackEmptyConversationCreation(conversationId: String) {
    synchronized(this) {
        check(loadMessages(conversationId).isEmpty()) {
            "Conversation creation rollback rejected after messages were persisted"
        }
        check(loadSymbolicProjection(conversationId) == null) {
            "Conversation creation rollback rejected after symbolic state was persisted"
        }
        val deleted = writableDatabase.delete(
            "desktop_conversations",
            "id = ? AND principal_id = ?",
            arrayOf(conversationId, ConversationHistoryContract.localPrincipalId),
        )
        check(deleted == 1) {
            "Conversation creation rollback lost canonical ownership"
        }
        check(getConversation(conversationId) == null) {
            "Conversation creation rollback postcondition was not durably observed"
        }
    }
}
