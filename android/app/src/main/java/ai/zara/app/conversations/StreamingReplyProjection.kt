package ai.zara.app.conversations

fun ConversationState.withStreamingReply(
    conversationId: String,
    expectedTurns: Int,
    text: String,
): ConversationState {
    val conversation = conversation(conversationId) ?: return this
    if (conversation.status != ConversationStatus.Running || conversation.turns.size != expectedTurns) return this
    val pending = conversation.turns.lastOrNull() ?: return this
    if (pending.success != null) return this
    val draft = conversation.copy(turns = conversation.turns.dropLast(1) + pending.copy(assistantText = text.take(65_536)))
    return copy(conversations = conversations.map { if (it.id == conversationId) draft else it })
}
