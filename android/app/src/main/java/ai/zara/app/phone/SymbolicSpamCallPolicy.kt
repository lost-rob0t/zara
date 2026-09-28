package ai.zara.app.phone

enum class SymbolicSpamAction {
    AlertUser,
    VoiceTakeover,
}

sealed interface SymbolicSpamDecision {
    data object PassThrough : SymbolicSpamDecision

    data class Answer(
        val greeting: String,
        val allowedActions: Set<SymbolicSpamAction>,
        val prologTools: Set<String> = emptySet(),
    ) : SymbolicSpamDecision
}

class SymbolicSpamCallPolicy {
    fun decide(isSuspectedSpam: Boolean): SymbolicSpamDecision =
        if (isSuspectedSpam) {
            SymbolicSpamDecision.Answer(
                greeting = GREETING,
                allowedActions = setOf(
                    SymbolicSpamAction.AlertUser,
                    SymbolicSpamAction.VoiceTakeover,
                ),
            )
        } else {
            SymbolicSpamDecision.PassThrough
        }

    companion object {
        const val GREETING = "Hi, I'm a symbolic system."
    }
}
