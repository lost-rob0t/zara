package ai.zara.app.conversations

import ai.zara.app.model.CloudModelHistory
import ai.zara.app.model.CloudModelTurn

fun ConversationRecord.cloudModelHistory(): CloudModelHistory = CloudModelHistory(
    localConversationId,
    turns.mapNotNull { turn ->
        val reply = turn.assistantText
        if (turn.success == true && !reply.isNullOrBlank()) CloudModelTurn(turn.userText, reply) else null
    },
)
